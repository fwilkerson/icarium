module Icarium.Commands.Util (
    -- * Errors
    fatal,
    resolveOrFatal,

    -- * Shared option readers
    taskStateReader,
    edgeKindReader,
    axisReader,
    effortReader,

    -- * Category validation
    requireCategory,
    resolveAxisFlag,

    -- * Node resolution
    requireTask,
    requireContext,
    resolveNode,

    -- * Body input handling
    BodyInput (..),
    bodyInputParser,
    resolveBody,

    -- * Small helpers
    subcmd,
    detectUtf8,
    detectTty,
    textOption,
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

import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Context qualified as RCx
import Icarium.Repo.Task qualified as RT
import Icarium.Types

fatal :: Int -> String -> IO a
fatal code msg = do
    hPutStrLn stderr ("icarium: error: " <> msg)
    exitWith (ExitFailure code)

{- | Run an IO action returning Either; exit fatally with code 1 (the
convention for ID-resolution failures) and the Left's message, or
return the Right value.
-}
resolveOrFatal :: IO (Either String a) -> IO a
resolveOrFatal m = m >>= either (fatal 1) pure

taskStateReader :: ReadM TaskState
taskStateReader = eitherReader $ \s ->
    case parseTaskState (T.pack s) of
        Just st -> Right st
        Nothing -> Left ("invalid state: " <> s)

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

effortReader :: ReadM Effort
effortReader = eitherReader $ \s ->
    case parseEffort (T.pack s) of
        Just e -> Right e
        Nothing -> Left ("invalid effort: " <> s)

data BodyInput = BodyInline Text | BodyFile FilePath | BodyNone

bodyInputParser :: Parser BodyInput
bodyInputParser =
    ( BodyInline . T.pack
        <$> strOption
            ( long "body"
                <> metavar "TEXT"
                <> help "Body as inline text"
            )
    )
        <|> ( BodyFile
                <$> strOption
                    ( long "body-file"
                        <> metavar "PATH"
                        <> help "Read body from file (use - for stdin)"
                    )
            )
        <|> pure BodyNone

resolveBody :: BodyInput -> IO Text
resolveBody BodyNone = pure ""
resolveBody (BodyInline t) = pure t
resolveBody (BodyFile "-") = TIO.getContents
resolveBody (BodyFile p) = TIO.readFile p

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

requireTask :: Connection -> Text -> IO Text
requireTask c input = do
    r <- RT.resolveTaskId c input
    case r of
        Right tid -> pure tid
        Left err -> fatal 2 err

requireContext :: Connection -> Text -> IO Text
requireContext c input = do
    r <- RCx.resolveContextId c input
    case r of
        Right cxid -> pure cxid
        Left err -> fatal 2 err

resolveNode :: Connection -> Text -> IO (NodeKind, Text)
resolveNode c input = do
    ts <- RT.getTasksByPrefix c input
    cxs <- RCx.getContextsByPrefix c input
    case (ts, cxs) of
        ([t], []) -> pure (TaskNode, taskId t)
        ([], [cx]) -> pure (ContextNode, contextId cx)
        ([], []) -> fatal 2 ("unknown node: " <> T.unpack input)
        _ -> fatal 2 ("ambiguous id: " <> T.unpack input)

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
                    <> "; add it to icarium.toml and run 'icarium category sync'"

{- | Validate a @--domain@/@--discipline@ flag value for update commands.
@Nothing@   = flag not given → no-op.
@Just ""@   = flag given with empty string → clear the axis.
@Just name@ = flag given with a name → validate and return the category.
-}
resolveAxisFlag :: Connection -> CategoryAxis -> Maybe Text -> IO (Maybe (Maybe Category))
resolveAxisFlag _ _ Nothing = pure Nothing
resolveAxisFlag _ _ (Just "") = pure (Just Nothing)
resolveAxisFlag c ax (Just n) = Just . Just <$> requireCategory c ax n
