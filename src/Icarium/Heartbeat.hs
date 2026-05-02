module Icarium.Heartbeat (pidAlive, heartbeatStale) where

import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime)
import System.Exit (ExitCode (..))
import System.Process.Typed (nullStream, proc, runProcess, setStderr, setStdout)

import Icarium.Db (parseDbTime)

pidAlive :: Int -> IO Bool
pidAlive pid = do
    code <-
        runProcess $
            setStdout nullStream $
                setStderr nullStream $
                    proc "kill" ["-0", show pid]
    pure $ case code of
        ExitSuccess -> True
        ExitFailure _ -> False

heartbeatStale :: UTCTime -> Int -> Text -> Bool
heartbeatStale now thresholdSec hbText =
    case parseDbTime hbText of
        Nothing -> True
        Just hb -> diffUTCTime now hb > fromIntegral thresholdSec
