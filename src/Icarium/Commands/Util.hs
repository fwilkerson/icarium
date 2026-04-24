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
    ) where

import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Options.Applicative
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
        Nothing -> Left ("invalid edge kind: " <> s)

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

data BodyInput = BodyInline Text | BodyFile FilePath | BodyStdin | BodyNone

bodyInputParser :: Parser BodyInput
bodyInputParser =
        (BodyInline . T.pack
            <$> strOption (long "body" <> metavar "TEXT"
                        <> help "Body as inline text"))
    <|> (BodyFile
            <$> strOption (long "body-file" <> metavar "PATH"
                        <> help "Read body from file"))
    <|> flag' BodyStdin (long "body-stdin" <> help "Read body from stdin")
    <|> pure BodyNone

resolveBody :: BodyInput -> IO Text
resolveBody BodyNone       = pure ""
resolveBody (BodyInline t) = pure t
resolveBody (BodyFile p)   = TIO.readFile p
resolveBody BodyStdin      = TIO.getContents

-- | Shorthand for a subcommand with helper automatically attached.
subcmd :: String -> String -> Parser a -> Mod CommandFields a
subcmd n desc p = command n (info (p <**> helper) (progDesc desc))
