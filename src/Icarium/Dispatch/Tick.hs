-- Pure parser/summariser for one line of claude's stream-json output.
-- No I/O. Tested in isolation; consumed by Icarium.Dispatch.teeAndHeartbeat.
module Icarium.Dispatch.Tick (
    TickState (..),
    TickAction (..),
    emptyTickState,
    summariseTick,
) where

import Data.Aeson (Object, Result (..), Value (..), decodeStrict, fromJSON)
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as AKM
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T

data TickState = TickState
    { tsEventCount :: !Int
    , tsLastMessageId :: !(Maybe Text)
    , tsTokIn :: !Int
    , tsTokOut :: !Int
    , tsTokCache :: !Int
    , tsConsecutiveRetries :: !Int
    }

data TickAction = TickContinue | TickKill Text deriving (Show, Eq)

emptyTickState :: TickState
emptyTickState = TickState 0 Nothing 0 0 0 0

{- | Parse one JSONL line and return lines to emit on stderr plus a
watchdog action. Increments the event counter and prints a usage
summary every 20 events. Returns 'TickKill' when @threshold@ consecutive
api_retry events are seen with no substantive turn between them.
-}
summariseTick :: Int -> String -> BC.ByteString -> TickState -> ([String], TickState, TickAction)
summariseTick threshold ts bytes st0 =
    let st = st0{tsEventCount = tsEventCount st0 + 1}
        fallback = ([formatRow ts '?' "unknown" (BC.unpack (BC.take 120 bytes))], st, TickContinue)
     in case decodeStrict bytes :: Maybe Value of
            Nothing -> fallback
            Just (Object obj) ->
                let (outLines, st') = parseEvent ts st obj
                    action
                        | tsConsecutiveRetries st' >= threshold =
                            TickKill
                                ( "retry-storm: "
                                    <> T.pack (show (tsConsecutiveRetries st'))
                                    <> " consecutive api_retry events"
                                )
                        | otherwise = TickContinue
                 in (outLines, st', action)
            Just _ -> fallback

formatRow :: String -> Char -> String -> String -> String
formatRow ts sym kw body = ts ++ "  " ++ [sym] ++ " " ++ pad kw ++ body
  where
    pad k = k ++ replicate (max 0 (14 - length k)) ' '

parseEvent :: String -> TickState -> Object -> ([String], TickState)
parseEvent ts st obj = case lookStr "type" obj of
    Just "system" -> handleSystem ts st obj
    Just "assistant" -> handleAssistant ts st obj
    Just "user" -> handleUser ts st obj
    Just "result" -> handleResult ts st obj
    Just "rate_limit_event" -> ([], st)
    _ -> ([formatRow ts '?' "unknown" "unrecognised event type"], st)

handleSystem :: String -> TickState -> Object -> ([String], TickState)
handleSystem ts st obj = case lookStr "subtype" obj of
    Just "api_retry" ->
        let msg = maybe "" T.unpack (lookStr "message" obj)
            st' = st{tsConsecutiveRetries = tsConsecutiveRetries st + 1}
         in ([formatRow ts '!' "api_retry" msg], st')
    _ ->
        let model = maybe "?" T.unpack (lookStr "model" obj)
            sessId = maybe "?" (take 8 . T.unpack) (lookStr "session_id" obj)
         in ([formatRow ts '.' "system" ("model=" ++ model ++ " session=" ++ sessId)], st)

handleAssistant :: String -> TickState -> Object -> ([String], TickState)
handleAssistant ts st obj =
    let msg = lookObj "message" obj
        msgId = msg >>= lookStr "id"
        usageObj = msg >>= lookObj "usage"
        inToks = fromMaybe 0 (usageObj >>= lookInt "input_tokens")
        outToks = fromMaybe 0 (usageObj >>= lookInt "output_tokens")
        cacheToks = fromMaybe 0 (usageObj >>= lookInt "cache_read_input_tokens")
        -- assistant events repeat per content block with an identical
        -- message-start usage snapshot; only add once per distinct message id.
        st1
            | isJust msgId && msgId /= tsLastMessageId st =
                st
                    { tsLastMessageId = msgId
                    , tsTokIn = tsTokIn st + inToks
                    , tsTokOut = tsTokOut st + outToks
                    , tsTokCache = tsTokCache st + cacheToks
                    }
            | otherwise = st
        contents = msg >>= lookArr "content"
        eventLine = do
            xs <- contents
            c <- case xs of (x : _) -> Just x; [] -> Nothing
            parseContent ts c
        -- substantive assistant content (tool_use/text/thinking) resets the retry counter
        st2 = case eventLine of
            Just _ -> st1{tsConsecutiveRetries = 0}
            Nothing -> st1
        (usageLines, st3) = checkUsagePeriodic ts st2
     in (maybeToList eventLine ++ usageLines, st3)

parseContent :: String -> Value -> Maybe String
parseContent ts (Object c) = case lookStr "type" c of
    Just "thinking" ->
        let txt = maybe "" (take 80 . T.unpack) (lookStr "thinking" c)
         in Just (formatRow ts '>' "thinking" txt)
    Just "text" ->
        let txt = maybe "" (take 80 . T.unpack) (lookStr "text" c)
         in Just (formatRow ts '>' "assistant" txt)
    Just "tool_use" ->
        let name = maybe "?" T.unpack (lookStr "name" c)
            inputV = lookObj "input" c
            summary = summariseToolInput name inputV
         in Just (formatRow ts '*' "tool" (name ++ ": " ++ summary))
    _ -> Nothing
parseContent _ _ = Nothing

handleUser :: String -> TickState -> Object -> ([String], TickState)
handleUser ts st obj =
    let msg = lookObj "message" obj
        contents = fromMaybe [] (msg >>= lookArr "content")
        errorLines = mapMaybe (toolResultError ts) contents
        -- any tool_result (success or error) counts as a substantive user turn
        st' =
            if any isToolResult contents
                then st{tsConsecutiveRetries = 0}
                else st
     in (errorLines, st')

isToolResult :: Value -> Bool
isToolResult (Object o) = lookStr "type" o == Just "tool_result"
isToolResult _ = False

toolResultError :: String -> Value -> Maybe String
toolResultError ts (Object o)
    | Just (String "tool_result") <- lookRaw "type" o
    , Just (Bool True) <- lookRaw "is_error" o =
        Just (formatRow ts 'x' "tool_result" (errBody o))
  where
    errBody c = case lookRaw "content" c of
        Just (String t) -> take 80 (T.unpack t)
        _ -> "error"
toolResultError _ _ = Nothing

{- | The result event's usage is authoritative and cumulative for the
whole run, so it overwrites (not adds to) the accumulated totals.
-}
handleResult :: String -> TickState -> Object -> ([String], TickState)
handleResult ts st obj =
    let subtype = maybe "?" T.unpack (lookStr "subtype" obj)
        result = maybe "" (take 60 . T.unpack) (lookStr "result" obj)
        usageObj = lookObj "usage" obj
        inToks = usageObj >>= lookInt "input_tokens"
        outToks = usageObj >>= lookInt "output_tokens"
        cacheToks = usageObj >>= lookInt "cache_read_input_tokens"
        st' =
            st
                { tsConsecutiveRetries = 0
                , tsTokIn = fromMaybe (tsTokIn st) inToks
                , tsTokOut = fromMaybe (tsTokOut st) outToks
                , tsTokCache = fromMaybe (tsTokCache st) cacheToks
                }
        resultLine = formatRow ts '+' "result" (subtype ++ ": " ++ result)
        usageLine = case (inToks, outToks, cacheToks) of
            (Just i, Just o, Just c) ->
                [ formatRow
                    ts
                    '='
                    "usage"
                    ( "in "
                        ++ show i
                        ++ " / out "
                        ++ show o
                        ++ " / cache_read "
                        ++ show c
                    )
                ]
            _ -> []
     in (resultLine : usageLine, st')

checkUsagePeriodic :: String -> TickState -> ([String], TickState)
checkUsagePeriodic ts st
    | tsEventCount st >= 20 =
        let line =
                formatRow
                    ts
                    '='
                    "usage"
                    ( "in "
                        ++ show (tsTokIn st)
                        ++ " / out "
                        ++ show (tsTokOut st)
                        ++ " / cache_read "
                        ++ show (tsTokCache st)
                    )
         in ([line], st{tsEventCount = 0})
    | otherwise = ([], st)

summariseToolInput :: String -> Maybe Object -> String
summariseToolInput "Bash" (Just o) = take 80 $ maybe "?" T.unpack (lookStr "command" o)
summariseToolInput "Read" (Just o) = maybe "?" T.unpack (lookStr "file_path" o)
summariseToolInput "Edit" (Just o) = maybe "?" T.unpack (lookStr "file_path" o)
summariseToolInput "Write" (Just o) = maybe "?" T.unpack (lookStr "file_path" o)
summariseToolInput "Glob" (Just o) = maybe "?" T.unpack (lookStr "pattern" o)
summariseToolInput "Grep" (Just o) = maybe "?" T.unpack (lookStr "pattern" o)
summariseToolInput "Agent" (Just o) = maybe "?" T.unpack (lookStr "description" o)
summariseToolInput _ _ = "..."

lookRaw :: Text -> Object -> Maybe Value
lookRaw k = AKM.lookup (AK.fromText k)

lookStr :: Text -> Object -> Maybe Text
lookStr k obj = case lookRaw k obj of
    Just (String t) -> Just t
    _ -> Nothing

lookObj :: Text -> Object -> Maybe Object
lookObj k obj = case lookRaw k obj of
    Just (Object o) -> Just o
    _ -> Nothing

lookArr :: Text -> Object -> Maybe [Value]
lookArr k obj = case lookRaw k obj of
    Just v -> case fromJSON v :: Result [Value] of
        Success xs -> Just xs
        Error _ -> Nothing
    Nothing -> Nothing

lookInt :: Text -> Object -> Maybe Int
lookInt k obj = case lookRaw k obj of
    Just v -> case fromJSON v :: Result Int of
        Success n -> Just n
        Error _ -> Nothing
    Nothing -> Nothing
