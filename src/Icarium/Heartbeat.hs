module Icarium.Heartbeat (
    pidAlive,
    heartbeatStale,
    DispatchHealth (..),
    dispatchHealth,
    healthInterrupted,
    dispatchIsInterrupted,
) where

import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime)
import System.Exit (ExitCode (..))
import System.Process.Typed (nullStream, proc, runProcess, setStderr, setStdout)

import Icarium.Db (parseDbTime)
import Icarium.Types (Dispatch (..))

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

{- | Liveness evidence for an open dispatch. Kept as both flags rather than a
single verdict because callers report them separately in recovery notes.
-}
data DispatchHealth = DispatchHealth
    { dhAlive :: Bool
    , dhStale :: Bool
    }
    deriving (Eq, Show)

dispatchHealth :: UTCTime -> Int -> Dispatch -> IO DispatchHealth
dispatchHealth now staleSec d = do
    alive <- maybe (pure False) pidAlive (dispatchPid d)
    pure $ DispatchHealth alive (heartbeatStale now staleSec (dispatchHeartbeat d))

healthInterrupted :: DispatchHealth -> Bool
healthInterrupted h = not (dhAlive h) || dhStale h

dispatchIsInterrupted :: UTCTime -> Int -> Dispatch -> IO Bool
dispatchIsInterrupted now staleSec d = healthInterrupted <$> dispatchHealth now staleSec d
