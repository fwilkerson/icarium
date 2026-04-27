-- Pure parser/summariser for one line of claude's stream-json output.
-- No I/O. Tested in isolation; consumed by Icarium.Dispatch.teeAndHeartbeat.
module Icarium.Dispatch.Tick
    ( TickState(..)
    , emptyTickState
    , summariseTick
    ) where

import           Data.Aeson            (Object, Result (..), Value (..), decodeStrict, fromJSON)
import qualified Data.Aeson.Key        as AK
import qualified Data.Aeson.KeyMap     as AKM
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe            (fromMaybe, mapMaybe, maybeToList)
import           Data.Text             (Text)
import qualified Data.Text             as T

data TickState = TickState
    { tsEventCount :: !Int
    , tsLastIn     :: !Int
    , tsLastOut    :: !Int
    , tsLastCache  :: !Int
    }

emptyTickState :: TickState
emptyTickState = TickState 0 0 0 0

-- | Parse one JSONL line and return lines to emit on stderr.
-- Increments the event counter and prints a usage summary every 20 events.
summariseTick :: String -> BC.ByteString -> TickState -> ([String], TickState)
summariseTick ts bytes st0 =
    let st = st0 { tsEventCount = tsEventCount st0 + 1 }
    in case decodeStrict bytes :: Maybe Value of
        Nothing  -> ([row '?' "unknown" (BC.unpack (BC.take 120 bytes))], st)
        Just val -> case val of
            Object obj -> parseEvent st obj
            _          -> ([row '?' "unknown" (BC.unpack (BC.take 120 bytes))], st)
  where
    pad k      = k ++ replicate (max 0 (14 - length k)) ' '
    row sym kw body = ts ++ "  " ++ [sym] ++ " " ++ pad kw ++ body

    parseEvent st obj = case lookStr "type" obj of
        Just "system"           -> handleSystem st obj
        Just "assistant"        -> handleAssistant st obj
        Just "user"             -> handleUser st obj
        Just "result"           -> handleResult st obj
        Just "rate_limit_event" -> ([], st)
        _                       -> ([row '?' "unknown" (BC.unpack (BC.take 120 bytes))], st)

    handleSystem st obj =
        let model  = maybe "?" T.unpack (lookStr "model"      obj)
            sessId = maybe "?" (take 8 . T.unpack) (lookStr "session_id" obj)
        in ([row '.' "system" ("model=" ++ model ++ " session=" ++ sessId)], st)

    handleAssistant st obj =
        let msg       = lookObj "message" obj
            usageObj  = msg >>= lookObj "usage"
            inToks    = usageObj >>= lookInt "input_tokens"
            outToks   = usageObj >>= lookInt "output_tokens"
            cacheToks = usageObj >>= lookInt "cache_read_input_tokens"
            st1 = st { tsLastIn    = fromMaybe (tsLastIn    st) inToks
                     , tsLastOut   = fromMaybe (tsLastOut   st) outToks
                     , tsLastCache = fromMaybe (tsLastCache st) cacheToks
                     }
            contents  = msg >>= lookArr "content"
            eventLine = do
                xs <- contents
                c  <- case xs of { (x:_) -> Just x; [] -> Nothing }
                parseContent c
            (usageLines, st2) = checkUsagePeriodic st1
        in (maybeToList eventLine ++ usageLines, st2)

    parseContent (Object c) = case lookStr "type" c of
        Just "thinking" ->
            let txt = maybe "" (take 80 . T.unpack) (lookStr "thinking" c)
            in Just (row '>' "thinking" txt)
        Just "text" ->
            let txt = maybe "" (take 80 . T.unpack) (lookStr "text" c)
            in Just (row '>' "assistant" txt)
        Just "tool_use" ->
            let name    = maybe "?" T.unpack (lookStr "name" c)
                inputV  = lookObj "input" c
                summary = summariseToolInput name inputV
            in Just (row '*' "tool" (name ++ ": " ++ summary))
        _ -> Nothing
    parseContent _ = Nothing

    handleUser st obj =
        let msg      = lookObj "message" obj
            contents = fromMaybe [] (msg >>= lookArr "content")
        in (mapMaybe toolResultError contents, st)

    toolResultError (Object o)
        | Just (String "tool_result") <- lookRaw "type"     o
        , Just (Bool True)            <- lookRaw "is_error" o
        = Just (row 'x' "tool_result" (errBody o))
      where
        errBody c = case lookRaw "content" c of
            Just (String t) -> take 80 (T.unpack t)
            _               -> "error"
    toolResultError _ = Nothing

    handleResult st obj =
        let subtype   = maybe "?" T.unpack (lookStr "subtype" obj)
            result    = maybe "" (take 60 . T.unpack) (lookStr "result" obj)
            usageObj  = lookObj "usage" obj
            inToks    = usageObj >>= lookInt "input_tokens"
            outToks   = usageObj >>= lookInt "output_tokens"
            cacheToks = usageObj >>= lookInt "cache_read_input_tokens"
            resultLine = row '+' "result" (subtype ++ ": " ++ result)
            usageLine  = case (inToks, outToks, cacheToks) of
                (Just i, Just o, Just c) ->
                    [row '=' "usage" ("in " ++ show i ++ " / out " ++ show o
                                    ++ " / cache_read " ++ show c)]
                _ -> []
        in (resultLine : usageLine, st)

    checkUsagePeriodic st
        | tsEventCount st >= 20 =
            let line = row '=' "usage" ("in " ++ show (tsLastIn st)
                                      ++ " / out " ++ show (tsLastOut st)
                                      ++ " / cache_read " ++ show (tsLastCache st))
            in ([line], st { tsEventCount = 0 })
        | otherwise = ([], st)

summariseToolInput :: String -> Maybe Object -> String
summariseToolInput "Bash"  (Just o) = take 80 $ maybe "?" T.unpack (lookStr "command"     o)
summariseToolInput "Read"  (Just o) = maybe "?" T.unpack (lookStr "file_path"   o)
summariseToolInput "Edit"  (Just o) = maybe "?" T.unpack (lookStr "file_path"   o)
summariseToolInput "Write" (Just o) = maybe "?" T.unpack (lookStr "file_path"   o)
summariseToolInput "Glob"  (Just o) = maybe "?" T.unpack (lookStr "pattern"     o)
summariseToolInput "Grep"  (Just o) = maybe "?" T.unpack (lookStr "pattern"     o)
summariseToolInput "Agent" (Just o) = maybe "?" T.unpack (lookStr "description" o)
summariseToolInput _       _        = "..."

lookRaw :: Text -> Object -> Maybe Value
lookRaw k = AKM.lookup (AK.fromText k)

lookStr :: Text -> Object -> Maybe Text
lookStr k obj = case lookRaw k obj of
    Just (String t) -> Just t
    _               -> Nothing

lookObj :: Text -> Object -> Maybe Object
lookObj k obj = case lookRaw k obj of
    Just (Object o) -> Just o
    _               -> Nothing

lookArr :: Text -> Object -> Maybe [Value]
lookArr k obj = case lookRaw k obj of
    Just v  -> case fromJSON v :: Result [Value] of
                   Success xs -> Just xs
                   Error _    -> Nothing
    Nothing -> Nothing

lookInt :: Text -> Object -> Maybe Int
lookInt k obj = case lookRaw k obj of
    Just v  -> case fromJSON v :: Result Int of
                   Success n -> Just n
                   Error _   -> Nothing
    Nothing -> Nothing
