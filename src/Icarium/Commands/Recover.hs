module Icarium.Commands.Recover (Options, parser, run) where

import           Control.Monad          (forM_, void)
import           Data.Either            (fromRight)
import           Data.Maybe             (isNothing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Time              (UTCTime, defaultTimeLocale, diffUTCTime, getCurrentTime,
                                         parseTimeM)
import           Database.SQLite.Simple (Connection)
import           Options.Applicative
import           System.Exit            (ExitCode (..))
import           System.Process.Typed   (nullStream, proc, runProcess, setStderr, setStdout)

import           Icarium.Commands.Util  (fatal)
import           Icarium.Config         (DispatchConfig (..), cfgDispatch, defaultConfigPath,
                                         loadConfig)
import           Icarium.Db             (defaultDbPath, withDb)
import qualified Icarium.Git            as Git
import qualified Icarium.Repo.Dispatch  as RD
import qualified Icarium.Repo.Task      as RT
import           Icarium.Types

data Options = Options
    { oDispatchId :: Maybe Text
    }

parser :: Parser Options
parser = Options
    <$> optional (T.pack <$> strOption (long "dispatch" <> metavar "ID"
                                       <> help "Reconcile a single dispatch id"))

run :: Options -> IO ()
run o = do
    cfg <- loadConfig defaultConfigPath >>= \case
        Left  e -> fatal 2 ("config parse error:\n" <> e)
        Right c -> pure c
    let staleSec = dcHeartbeatStaleSeconds (cfgDispatch cfg)
    withDb defaultDbPath $ \c -> do
        open <- case oDispatchId o of
            Just raw -> do
                did <- RD.resolveDispatchId c raw >>= \case
                    Left err -> fatal 1 err
                    Right x  -> pure x
                md <- RD.getDispatch c did
                pure $ case md of
                    Just d | isNothing (dispatchOutcome d) -> [d]
                    _ -> []
            Nothing  -> RD.listOpenDispatches c
        if null open
            then TIO.putStrLn "no open dispatches"
            else do
                now <- getCurrentTime
                forM_ open (reconcile c now staleSec)

-- | Decide whether a dispatch is actually orphaned, then update state
-- accordingly. Prints a one-line audit record per reconciled dispatch.
reconcile :: Connection -> UTCTime -> Int -> Dispatch -> IO ()
reconcile c now staleSec d = do
    alive <- maybe (pure False) isPidAlive (dispatchPid d)
    stale <- isHeartbeatStale now staleSec (dispatchHeartbeat d)
    if alive && not stale
        then pure ()   -- genuinely still running; leave it
        else do
            uncommitted <- fmap not Git.isClean
            lastCommit  <- fmap (fromRight "") (Git.revParse (dispatchBranch d))
            let notes = T.intercalate "; "
                    [ "interrupted"
                    , "alive=" <> boolText alive
                    , "stale=" <> boolText stale
                    , "uncommitted=" <> boolText uncommitted
                    , "last_commit=" <> lastCommit
                    ]
            RD.finishDispatch c (dispatchId d) OInterrupted Nothing (Just notes)
            void $ RT.updateTask c (dispatchTaskId d) RT.emptyUpdate
                { RT.tuState       = Just Blocked
                , RT.tuBlockReason = Just (Just notes)
                }
            TIO.putStrLn $ "dispatch:" <> dispatchId d
                <> "  task:" <> dispatchTaskId d
                <> "  branch:" <> dispatchBranch d
                <> "  " <> notes

-- | @kill -0 PID@: exit 0 means the process exists, non-zero means it
-- doesn't. We suppress all output so the audit log isn't noisy.
isPidAlive :: Int -> IO Bool
isPidAlive pid = do
    code <- runProcess
        $ setStdout nullStream
        $ setStderr nullStream
        $ proc "kill" ["-0", show pid]
    pure $ case code of
        ExitSuccess   -> True
        ExitFailure _ -> False

-- | The heartbeat text is SQLite's @datetime('now')@ output, UTC.
isHeartbeatStale :: UTCTime -> Int -> Text -> IO Bool
isHeartbeatStale now thresholdSec hbText =
    pure $ case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S"
                           (T.unpack hbText) of
        Nothing -> True   -- unparseable -> safest to treat as stale
        Just hb -> diffUTCTime now hb > fromIntegral thresholdSec

boolText :: Bool -> Text
boolText True  = "yes"
boolText False = "no"
