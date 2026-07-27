module Icarium.Commands.Util (
    -- * Errors
    fatal,
    lockBusy,
    resolveOrFatal,
    requireConfig,

    -- * Shared option readers
    taskStateReader,
    stateChoices,
    edgeKindReader,
    axisReader,
    routingP,

    -- * Category validation
    requireCategory,
    resolveAxisFlag,
    resolveCatFilters,

    -- * Body input handling
    BodyInput (..),
    bodyInputParser,
    resolveBody,

    -- * Small helpers
    subcmd,
    detectUtf8,
    detectTty,
    textOption,
    jsonFlag,
) where

import Data.Char (toUpper)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection)
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdout)

import Icarium.Config (Config, defaultConfigPath, loadConfig)
import Icarium.Render qualified as Render
import Icarium.Repo.Category qualified as RC
import Icarium.Types

fatal :: Int -> String -> IO a
fatal code msg = do
    hPutStrLn stderr ("icarium: error: " <> msg)
    exitWith (ExitFailure code)

{- | Exit for a claim that never got the database write lock. Code 3 —
operational failure — because exit 1 is how the claim commands say "no work",
and a caller that reads a lost race as an empty queue stops when it should
retry. @retry@ is the command to run again.
-}
lockBusy :: Text -> IO a
lockBusy = fatal 3 . T.unpack . Render.renderLockBusy

{- | Run an IO action returning Either; exit fatally with code 1 (the
convention for ID-resolution failures) and the Left's message, or
return the Right value.
-}
resolveOrFatal :: IO (Either String a) -> IO a
resolveOrFatal m = m >>= either (fatal 1) pure

-- | Load icarium.toml, or exit 2 with the parse error.
requireConfig :: IO Config
requireConfig =
    loadConfig defaultConfigPath
        >>= either (fatal 2 . ("config parse error:\n" <>)) pure

-- | The valid @--state@ values, in lifecycle order, spelled as the CLI takes them.
stateChoices :: String
stateChoices = T.unpack (T.intercalate " | " (map taskStateCli allTaskStates))

{- | Rejects bare @ready@ along with any other unknown value: the message
lists the states rather than special-casing the old name, which would make
the removed vocabulary look merely misspelled.
-}
taskStateReader :: ReadM TaskState
taskStateReader = eitherReader $ \s ->
    case parseTaskState (T.pack s) of
        Just st -> Right st
        Nothing ->
            Left $
                "invalid state: "
                    <> s
                    <> "; valid states are "
                    <> stateChoices

edgeKindReader :: ReadM EdgeKind
edgeKindReader = eitherReader $ \s ->
    case parseEdgeKind (T.pack s) of
        Just k -> Right k
        Nothing -> Left $ case s of
            "depends_on" -> "use `depends-on`, not `depends_on`"
            "derived_from" -> "use `derived-from`, not `derived_from`"
            _ ->
                "invalid edge kind: "
                    <> s
                    <> "; accepted: depends-on, references, derived-from, supersedes"

axisReader :: ReadM CategoryAxis
axisReader = eitherReader $ \s ->
    case parseCategoryAxis (T.pack s) of
        Just a -> Right a
        Nothing -> Left ("invalid axis: " <> s)

effortChoices :: String
effortChoices = T.unpack (T.intercalate " | " (map effortText allEfforts))

{- | @--effort@ where the empty string clears the value, matching the
category-axis flags. One flag sets and unsets, so there is no second
@--no-effort@ spelling to remember.
-}
effortReader :: ReadM (Maybe Effort)
effortReader = eitherReader $ \s ->
    if null s
        then Right Nothing
        else case parseEffort (T.pack s) of
            Just e -> Right (Just e)
            Nothing -> Left ("invalid effort: " <> s)

{- | The routing flags, as a patch over an existing 'Routing'. A flag that
isn't given leaves its field alone, so the same parser serves @task add@
(patching 'mempty'), @task update@ (patching the stored routing) and
@dispatch run@ (patching the empty flag-level routing). @subject@ names
what is being routed, e.g. @\"this task\"@.

A new routing knob is one more flag here and one more field in 'Routing';
no call site changes.
-}
routingP :: String -> Parser (Routing -> Routing)
routingP subject = patch <$> modelFlag <*> effortFlag
  where
    patch mm me r =
        r
            { rtModel = fromMaybe (rtModel r) mm
            , rtEffort = fromMaybe (rtEffort r) me
            }
    modelFlag =
        fmap blank
            <$> optional
                ( textOption
                    "model"
                    "NAME"
                    ("Use this model for " <> subject <> " instead of the [dispatch] default; empty string clears")
                )
    blank t = if T.null (T.strip t) then Nothing else Just t
    effortFlag =
        optional
            ( option
                effortReader
                ( long "effort"
                    <> metavar "LEVEL"
                    <> help ("Use this effort for " <> subject <> " instead of the [dispatch] default (" <> effortChoices <> "); empty string clears")
                )
            )

data BodyInput = BodyInline Text | BodyStdin | BodyNone

bodyInputParser :: Parser BodyInput
bodyInputParser =
    ( BodyInline . T.pack
        <$> strOption
            ( long "body"
                <> metavar "TEXT"
                <> help "Body as inline text (short bodies only; for real markdown, omit and Write to the path printed on stdout)"
            )
    )
        <|> flag
            BodyNone
            BodyStdin
            ( long "body-stdin"
                <> help "Read body from stdin (recommended for agents: pipe a heredoc). Alternative: omit and Write to the body path printed on stdout."
            )

{- | Explicit body flags must carry content; a silent empty body files a
title with no contract (issue #13). BodyNone stays legal: the
add-then-Write flow defers the body on purpose.
-}
resolveBody :: BodyInput -> IO Text
resolveBody BodyNone = pure ""
resolveBody (BodyInline t) = requireBodyContent "--body" t
resolveBody BodyStdin = TIO.getContents >>= requireBodyContent "--body-stdin"

requireBodyContent :: String -> Text -> IO Text
requireBodyContent flagName t
    | T.null (T.strip t) =
        fatal 2 (flagName <> ": empty body; to defer the body, omit the flag and Write to the path printed on stdout")
    | otherwise = pure t

-- | Shorthand for a subcommand with helper automatically attached.
subcmd :: String -> String -> Parser a -> Mod CommandFields a
subcmd n desc p = command n (info (p <**> helper) (progDesc desc))

detectTty :: IO Bool
detectTty = hIsTerminalDevice stdout

-- | Detect whether the terminal locale is UTF-8 capable.
detectUtf8 :: IO Bool
detectUtf8 = do
    lcAll <- lookupEnv "LC_ALL"
    lcCtype <- lookupEnv "LC_CTYPE"
    lang <- lookupEnv "LANG"
    let envVal = map toUpper $ fromMaybe "" (lcAll <|> lcCtype <|> lang)
    return ("UTF" `isInfixOf` envVal)

textOption :: String -> String -> String -> Parser Text
textOption flg mv hlp =
    T.pack <$> strOption (long flg <> metavar mv <> help hlp)

-- | The read surface's @--json@ switch. Same wording everywhere it appears.
jsonFlag :: Parser Bool
jsonFlag =
    switch
        ( long "json"
            <> help "Emit machine-readable JSON on stdout instead of the human table"
        )

requireCategory :: Connection -> CategoryAxis -> Text -> IO Category
requireCategory c axis name = do
    mc <- RC.findCategory c axis name
    case mc of
        Just cat -> pure cat
        Nothing ->
            fatal 2 $
                "unknown "
                    <> T.unpack (categoryAxisText axis)
                    <> ": "
                    <> T.unpack name
                    <> "; register it with `icarium category add --axis "
                    <> T.unpack (categoryAxisText axis)
                    <> " "
                    <> T.unpack name
                    <> "` (existing: `icarium category list`)"

{- | Drop the axes whose filter flag was absent and validate the rest, so a
typo'd category name fails before the query runs rather than silently
returning nothing.
-}
resolveCatFilters :: Connection -> [(CategoryAxis, Maybe Text)] -> IO [(CategoryAxis, Text)]
resolveCatFilters c flags =
    mapM (\(ax, n) -> (ax, n) <$ requireCategory c ax n) [(ax, n) | (ax, Just n) <- flags]

{- | Validate a @--domain@/@--discipline@/@--kind@ flag value for update commands.
@Nothing@   = flag not given → no-op.
@Just ""@   = flag given with empty string → clear the axis.
@Just name@ = flag given with a name → validate and return the category.
-}
resolveAxisFlag :: Connection -> CategoryAxis -> Maybe Text -> IO (Maybe (Maybe Category))
resolveAxisFlag _ _ Nothing = pure Nothing
resolveAxisFlag _ _ (Just "") = pure (Just Nothing)
resolveAxisFlag c ax (Just n) = Just . Just <$> requireCategory c ax n
