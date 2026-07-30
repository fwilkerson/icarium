module Icarium.Commands.Dispatch (Command, parser, run) where

import Control.Monad (forM, forM_, unless, void, when)
import Data.Either (fromRight)
import Data.List (nub)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection)
import Options.Applicative
import System.Directory (doesDirectoryExist, doesFileExist)

import Icarium.Git qualified as Git

import Icarium.Commands.Util
import Icarium.Config (Config, DispatchConfig (..), cfgDispatch)
import Icarium.Db (withDb, withDbSync)
import Icarium.Dispatch.Drain qualified as Drain
import Icarium.Dispatch.Merge (MergeOutcome (..), mergeParked)
import Icarium.Dispatch.PostClaude (checkpointDirtyTree)
import Icarium.Dispatch.Worktree (teardownWorktree, worktreePath)
import Icarium.Events qualified as Ev
import Icarium.Heartbeat (dispatchHealth, healthInterrupted)
import Icarium.Render qualified as Render
import Icarium.Repo.Context qualified as RCx
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Task qualified as RT
import Icarium.Types

data Command
    = Run RunOpts
    | List ListOpts
    | Show ShowOpts
    | Logs LogsOpts
    | Recover RecoverOpts
    | Merge MergeOpts
    | Stats StatsOpts

parser :: Parser Command
parser =
    subparser
        ( subcmd
            "run"
            "Run one dispatch (with TASK_ID) or drain the ready queue in priority order (no TASK_ID, optionally --max N). Exit 3 if any dispatch failed or stayed parked; exit 0 only when everything selected succeeded and landed."
            (Run <$> runP)
            <> subcmd "list" "List dispatches (alias: ls)" (List <$> listP)
            <> subcmd "show" "Show a single dispatch" (Show <$> showP)
            <> subcmd "logs" "Print the jsonl event log" (Logs <$> logsP)
            <> subcmd
                "recover"
                "Reconcile dispatches whose orchestrator died mid-run: mark outcome interrupted, move task to blocked with structured notes."
                (Recover <$> recoverP)
            <> subcmd
                "merge"
                "Land a parked dispatch branch on its base: fast-forward when possible, else rebase + re-run gates."
                (Merge <$> mergeP)
            <> subcmd
                "stats"
                "Spend and outcome summary (run counts, token sums) for budget checks."
                (Stats <$> statsP)
        )

run :: FilePath -> Command -> IO ()
run db = \case
    Run o -> runRun db o
    List o -> runList db o
    Show o -> runShow db o
    Logs o -> runLogs db o
    Recover o -> runRecover db o
    Merge o -> runMerge db o
    Stats o -> runStats db o

-- =============================================================
-- run  (single-task dispatch or queue drain)
-- =============================================================

data RunOpts = RunOpts
    { rTaskId :: Maybe Text
    , rMax :: Maybe Int
    , rRouting :: Routing
    , rBase :: Maybe Text
    , rDryRun :: Bool
    }

runP :: Parser RunOpts
runP =
    RunOpts
        <$> optional (T.pack <$> strArgument (metavar "TASK_ID"))
        <*> optional
            ( option
                auto
                ( long "max"
                    <> metavar "N"
                    <> help "Cap dispatches when draining the queue; a named TASK_ID is a drain of one"
                )
            )
        <*> (($ mempty) <$> routingP (FreshRouting "this dispatch"))
        <*> optional (textOption "base-branch" "NAME" "Override the base branch for git operations")
        <*> switch (long "dry-run" <> help "Build the plan and prompt; don't cut git or call claude")

{- | Flags in, a selector and a cap out. Everything a run decides — what it
selects, why it stops, what it exits with — belongs to
"Icarium.Dispatch.Drain"; a named task is a drain whose selector yields once,
so there is nothing to branch on here.
-}
runRun :: FilePath -> RunOpts -> IO ()
runRun db o = do
    cfg <- requireConfig
    forM_ (rMax o) $ \n -> when (n <= 0) $ fatal 2 "max must be > 0"
    withDbSync db $ \conn -> do
        selector <- case rTaskId o of
            Nothing -> pure Drain.QueueHead
            Just raw -> Drain.Named <$> resolveOrFatal (RT.resolveTaskId conn raw)
        report <-
            Drain.drain
                Drain.DrainRequest
                    { Drain.dqConn = conn
                    , Drain.dqDbPath = db
                    , Drain.dqConfig = cfg
                    , Drain.dqSelector = selector
                    , Drain.dqCap = rMax o
                    , Drain.dqDryRun = rDryRun o
                    , Drain.dqRouting = rRouting o
                    , Drain.dqBaseOverride = rBase o
                    }
        forM_ (Drain.runExit report) $ \(code, msg) -> fatal code (T.unpack msg)

-- =============================================================
-- list
-- =============================================================

data ListOpts = ListOpts
    { lTask :: Maybe Text
    , lOutcome :: Maybe DispatchOutcome
    , lLimit :: Maybe Int
    , lParked :: Bool
    }

listP :: Parser ListOpts
listP =
    ListOpts
        <$> optional (textOption "task" "TASK_ID" "Only dispatches for this task")
        <*> optional
            ( option
                outcomeReader
                ( long "outcome"
                    <> metavar "OUTCOME"
                    <> help "success | failure | interrupted"
                )
            )
        <*> optional
            ( option
                auto
                ( long "limit"
                    <> metavar "N"
                    <> help "Return at most N dispatches"
                )
            )
        <*> switch (long "parked" <> help "Only parked dispatches (successful, not yet merged)")

outcomeReader :: ReadM DispatchOutcome
outcomeReader = eitherReader $ \s ->
    case parseDispatchOutcome (T.pack s) of
        Just o -> Right o
        Nothing -> Left ("invalid outcome: " <> s)

runList :: FilePath -> ListOpts -> IO ()
runList db o = withDb db $ \c -> do
    mTaskId <- traverse (resolveOrFatal . RT.resolveTaskId c) (lTask o)
    ds <-
        if lParked o
            then do
                parked <- RD.listParkedDispatches c
                pure $ maybe parked (\tid -> filter ((== tid) . dispatchTaskId) parked) mTaskId
            else RD.listDispatches c mTaskId
    let filtered0 = case lOutcome o of
            Nothing -> ds
            Just want -> filter ((Just want ==) . dispatchOutcome) ds
        filtered = maybe filtered0 (`take` filtered0) (lLimit o)
    now <- getCurrentTime
    let taskIds = nub (map dispatchTaskId filtered)
    tasks <- RT.getTasksByIds c taskIds
    let titleMap = [(taskId t, taskTitle t) | t <- tasks]
    ctxCounts <- forM filtered $ \d ->
        length <$> RCx.contextsFromDispatch c (dispatchId d)
    let rows =
            zipWith
                ( \d kc ->
                    Render.DispatchRow
                        { Render.drDispatch = d
                        , Render.drTaskTitle = fromMaybe "" (lookup (dispatchTaskId d) titleMap)
                        , Render.drCtxCount = kc
                        , Render.drDuration = Render.renderDispatchDuration now d
                        }
                )
                filtered
                ctxCounts
    utf8 <- detectUtf8
    TIO.putStr (Render.renderDispatchList utf8 rows)

-- =============================================================
-- show
-- =============================================================

newtype ShowOpts = ShowOpts {sId :: Text}

showP :: Parser ShowOpts
showP =
    ShowOpts . T.pack
        <$> strArgument (metavar "DISPATCH_ID")

runShow :: FilePath -> ShowOpts -> IO ()
runShow db o = withDb db $ \c -> do
    did <- resolveOrFatal (RD.resolveDispatchId c (sId o))
    md <- RD.getDispatch c did
    case md of
        Nothing -> fatal 1 ("dispatch not found: " <> T.unpack did)
        Just d -> do
            mt <- RT.getTask c (dispatchTaskId d)
            ks <- RCx.contextsFromDispatch c (dispatchId d)
            mRetryId <- case dispatchOutcome d of
                Just OFailure -> do
                    later <- RD.listDispatches c (Just (dispatchTaskId d))
                    pure $ case filter (\d2 -> dispatchStartedAt d2 > dispatchStartedAt d) later of
                        (d2 : _) -> Just (dispatchId d2)
                        [] -> Nothing
                _ -> pure Nothing
            TIO.putStr (Render.renderDispatch d mt ks mRetryId)

-- =============================================================
-- logs
-- =============================================================

data LogsOpts = LogsOpts
    { gId :: Text
    , gTail :: Maybe Int
    }

logsP :: Parser LogsOpts
logsP =
    LogsOpts . T.pack
        <$> strArgument (metavar "DISPATCH_ID")
        <*> optional
            ( option
                auto
                ( long "tail"
                    <> metavar "N"
                    <> help "Print only the last N lines"
                )
            )

runLogs :: FilePath -> LogsOpts -> IO ()
runLogs db o = withDb db $ \c -> do
    did <- resolveOrFatal (RD.resolveDispatchId c (gId o))
    md <- RD.getDispatch c did
    case md of
        Nothing -> fatal 1 ("dispatch not found: " <> T.unpack did)
        Just d -> do
            case dispatchLogPath d of
                Nothing -> fatal 1 ("no log recorded for dispatch " <> T.unpack did)
                Just p -> printLogFile (T.unpack p) (gTail o)
            case dispatchReviewerLogPath d of
                Nothing -> pure ()
                Just rp -> do
                    TIO.putStrLn ""
                    TIO.putStrLn "--- reviewer log ---"
                    printLogFile (T.unpack rp) (gTail o)

printLogFile :: FilePath -> Maybe Int -> IO ()
printLogFile path mTail = do
    exists <- doesFileExist path
    unless exists $
        fatal 1 ("log file missing: " <> path)
    contents <- readFile path
    let ls = lines contents
        out = case mTail of
            Nothing -> ls
            Just n -> drop (max 0 (length ls - n)) ls
    mapM_ putStrLn out

-- =============================================================
-- recover  (reconcile orphaned dispatches)
-- =============================================================

newtype RecoverOpts = RecoverOpts
    { recDispatchId :: Maybe Text
    }

recoverP :: Parser RecoverOpts
recoverP =
    RecoverOpts
        <$> optional (T.pack <$> strArgument (metavar "DISPATCH_ID"))

runRecover :: FilePath -> RecoverOpts -> IO ()
runRecover db o = do
    cfg <- requireConfig
    let staleSec = dcHeartbeatStaleSeconds (cfgDispatch cfg)
    withDb db $ \c -> do
        open <- case recDispatchId o of
            Just raw -> do
                did <- resolveOrFatal (RD.resolveDispatchId c raw)
                md <- RD.getDispatch c did
                pure $ case md of
                    Just d | isNothing (dispatchOutcome d) -> [d]
                    _ -> []
            Nothing -> RD.listOpenDispatches c
        if null open
            then TIO.putStrLn "no open dispatches"
            else do
                now <- getCurrentTime
                forM_ open (reconcileDispatch db c (cfgDispatch cfg) now staleSec)
                Git.worktreePrune "."

reconcileDispatch :: FilePath -> Connection -> DispatchConfig -> UTCTime -> Int -> Dispatch -> IO ()
reconcileDispatch db c dcfg now staleSec d = do
    health <- dispatchHealth now staleSec d
    if not (healthInterrupted health)
        then pure ()
        else do
            -- If the dispatch worktree survived the crash, preserve any
            -- in-flight work on the branch, then remove it. When it's gone
            -- there is nothing to say about uncommitted state — the invoking
            -- checkout's dirtiness is not this dispatch's.
            let wt = worktreePath (dispatchId d)
            wtExists <- doesDirectoryExist wt
            mDirty <-
                if wtExists
                    then do
                        dirty <- checkpointDirtyTree wt (dispatchId d) "interrupted"
                        teardownWorktree "." dcfg wt
                        pure (Just dirty)
                    else pure Nothing
            lastCommit <- fmap (fromRight "") (Git.revParse "." (dispatchBranch d))
            let notes = Render.renderRecoveryNotes health mDirty lastCommit
            RD.finishDispatch c (dispatchId d) OInterrupted (Just notes)
            Ev.emit db "dispatch recover" $
                Ev.DispatchFinished (dispatchId d) (dispatchTaskId d) OInterrupted
            mTask <- RT.getTask c (dispatchTaskId d)
            void $
                RT.updateTask
                    c
                    (dispatchTaskId d)
                    RT.emptyUpdate
                        { RT.tuState = Just Blocked
                        , RT.tuBlockReason = Just (Just notes)
                        }
            forM_ mTask $ \t ->
                Ev.emit db "dispatch recover" $
                    Ev.TaskUpdated (dispatchTaskId d) (taskState t) Blocked
            TIO.putStrLn (Render.renderRecovered d notes)

-- =============================================================
-- merge  (land parked dispatches on their base)
-- =============================================================

data MergeOpts = MergeOpts
    { mId :: Maybe Text
    , mAll :: Bool
    }

mergeP :: Parser MergeOpts
mergeP =
    MergeOpts
        <$> optional (T.pack <$> strArgument (metavar "DISPATCH_ID"))
        <*> switch (long "all" <> help "Land every parked dispatch, oldest first")

runMerge :: FilePath -> MergeOpts -> IO ()
runMerge db o = case (mId o, mAll o) of
    (Just _, True) -> fatal 2 "--all and DISPATCH_ID are mutually exclusive"
    (Nothing, False) -> fatal 2 "provide a DISPATCH_ID or --all"
    (Just raw, False) -> withMergeCtx (\cfg c -> mergeOne cfg c raw)
    (Nothing, True) -> withMergeCtx mergeAll
  where
    withMergeCtx k = do
        cfg <- requireConfig
        withDbSync db (k cfg)

-- | Land a single parked dispatch; any blocked outcome is fatal.
mergeOne :: Config -> Connection -> Text -> IO ()
mergeOne cfg c raw = do
    did <- resolveOrFatal (RD.resolveDispatchId c raw)
    md <- RD.getDispatch c did
    d <- maybe (fatal 1 ("dispatch not found: " <> T.unpack did)) pure md
    case dispatchOutcome d of
        Just OSuccess -> pure ()
        other ->
            fatal 1 $
                "not a successful dispatch (outcome: "
                    <> maybe "open" (T.unpack . dispatchOutcomeText) other
                    <> ")"
    forM_ (dispatchMergeSha d) $ \sha ->
        fatal 1 ("already merged: " <> T.unpack sha)
    mergeParked cfg c d >>= \case
        MergeLanded sha -> TIO.putStrLn (Render.renderLanded d sha)
        MergeBlocked code note -> fatal code (T.unpack note)
        MergeStopped note -> fatal 3 (T.unpack note)

{- | Land every parked dispatch oldest-first. A blocked dispatch stays
parked (its notes were updated by the merge path) and the run continues;
worktree back-pressure stops it, leaving the rest untouched. Exits 3
when anything failed to land.
-}
mergeAll :: Config -> Connection -> IO ()
mergeAll cfg c = do
    parked <- RD.listParkedDispatches c
    if null parked
        then TIO.putStrLn "no parked dispatches"
        else do
            outcomes <- landInOrder parked
            let landed = length [() | MergeLanded _ <- outcomes]
                blocked = length [() | MergeBlocked _ _ <- outcomes]
                unattempted = length parked - length outcomes
            TIO.putStrLn (Render.renderMergeTally (length parked) landed blocked unattempted)
            when (landed < length parked) $
                fatal 3 "not all parked dispatches landed"
  where
    landInOrder [] = pure []
    landInOrder (d : ds) = do
        out <- mergeParked cfg c d
        TIO.putStrLn (Render.renderMergeAttempt d out)
        case out of
            MergeStopped _ -> pure [out]
            _ -> (out :) <$> landInOrder ds

-- =============================================================
-- stats  (spend/outcome summary for budget checks)
-- =============================================================

newtype StatsOpts = StatsOpts {stSince :: Maybe Text}

statsP :: Parser StatsOpts
statsP =
    StatsOpts
        <$> optional
            ( textOption
                "since"
                "TIMESTAMP"
                "Only dispatches started at or after this UTC timestamp (storage format: YYYY-MM-DD HH:MM:SS); omit for all dispatches"
            )

runStats :: FilePath -> StatsOpts -> IO ()
runStats db o = withDb db $ \c -> do
    stats <- RD.getDispatchStats c (stSince o)
    TIO.putStr (Render.renderDispatchStats (stSince o) stats)
