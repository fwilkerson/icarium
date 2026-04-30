module Icarium.Heartbeat (pidAlive, heartbeatStale) where

import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time            (UTCTime, defaultTimeLocale, diffUTCTime, parseTimeM)
import           System.Exit          (ExitCode (..))
import           System.Process.Typed (nullStream, proc, runProcess, setStderr, setStdout)

pidAlive :: Int -> IO Bool
pidAlive pid = do
    code <- runProcess
        $ setStdout nullStream
        $ setStderr nullStream
        $ proc "kill" ["-0", show pid]
    pure $ case code of
        ExitSuccess   -> True
        ExitFailure _ -> False

heartbeatStale :: UTCTime -> Int -> Text -> Bool
heartbeatStale now thresholdSec hbText =
    case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack hbText) of
        Nothing -> True
        Just hb -> diffUTCTime now hb > fromIntegral thresholdSec
