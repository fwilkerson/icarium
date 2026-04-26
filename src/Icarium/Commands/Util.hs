module Icarium.Commands.Util
    ( -- * Errors
      fatal
      -- * Shared option readers
    , taskStateReader
    , edgeKindReader
    , axisReader
    , effortReader
      -- * Body input handling
    , BodyInput(..)
    , bodyInputParser
    , resolveBody
      -- * Small helpers
    , subcmd
    , detectUtf8
    ) where

import           Data.Char           (toUpper)
import           Data.List           (isInfixOf)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Options.Applicative
import           System.Environment  (lookupEnv)
import           System.Exit         (ExitCode (..), exitWith)
import           System.IO           (hPutStrLn, stderr)

import           Icarium.Types

fatal :: Int -> String -> IO a
fatal code msg = do
    hPutStrLn stderr ("icarium: error: " <> msg)
    exitWith (ExitFailure code)

taskStateReader :: ReadM TaskState
taskStateReader = eitherReader $ \s ->
    case parseTaskState (T.pack s) of
        Just st -> Right st
        Nothing -> Left ("invalid state: " <> s)

edgeKindReader :: ReadM EdgeKind
edgeKindReader = eitherReader $ \s ->
    case parseEdgeKind (T.pack s) of
        Just k  -> Right k
        Nothing -> Left $ case s of
            "depends_on"   -> "use `depends-on`, not `depends_on`"
            "derived_from" -> "use `derived-from`, not `derived_from`"
            _              -> "invalid edge kind: " <> s
                           <> "; accepted: depends-on, references, derived-from, supersedes"

axisReader :: ReadM CategoryAxis
axisReader = eitherReader $ \s ->
    case parseCategoryAxis (T.pack s) of
        Just a  -> Right a
        Nothing -> Left ("invalid axis: " <> s)

effortReader :: ReadM Effort
effortReader = eitherReader $ \s ->
    case parseEffort (T.pack s) of
        Just e  -> Right e
        Nothing -> Left ("invalid effort: " <> s)

data BodyInput = BodyInline Text | BodyFile FilePath | BodyNone

bodyInputParser :: Parser BodyInput
bodyInputParser =
        (BodyInline . T.pack
            <$> strOption (long "body" <> metavar "TEXT"
                        <> help "Body as inline text"))
    <|> (BodyFile
            <$> strOption (long "body-file" <> metavar "PATH"
                        <> help "Read body from file (use - for stdin)"))
    <|> pure BodyNone

resolveBody :: BodyInput -> IO Text
resolveBody BodyNone       = pure ""
resolveBody (BodyInline t) = pure t
resolveBody (BodyFile "-") = TIO.getContents
resolveBody (BodyFile p)   = TIO.readFile p

-- | Shorthand for a subcommand with helper automatically attached.
subcmd :: String -> String -> Parser a -> Mod CommandFields a
subcmd n desc p = command n (info (p <**> helper) (progDesc desc))

-- | Detect whether the terminal locale is UTF-8 capable.
detectUtf8 :: IO Bool
detectUtf8 = do
    lcAll   <- lookupEnv "LC_ALL"
    lcCtype <- lookupEnv "LC_CTYPE"
    lang    <- lookupEnv "LANG"
    let envVal = map toUpper $ fromMaybe "" (lcAll <|> lcCtype <|> lang)
    return ("UTF" `isInfixOf` envVal)
