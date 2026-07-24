module Icarium.Commands.Dispatch (Command, parser, run) where

import Control.Monad (forM, forM_, unless, void, when)
import Data.Either (fromRight)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (nub)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection)
import Options.Applicative
import System.Directory (doesDirectoryExist, doesFileExist)
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals (Handler (..), installHandler, raiseSignal, sigINT)

import Icarium.Dispatch.LogResult (readLogResult)
import Icarium.Git qualified as Git

import Icarium.Commands.Util
import Icarium.Config (Config, DispatchConfig (..), cfgDispatch)
import Icarium.Db (withDb, withDbSync)
import Icarium.Dispatch qualified as D
import Icarium.Dispatch.Merge (MergeOutcome (..), mergeParked)
import Icarium.Dispatch.PostClaude (checkpointDirtyTree)
import Icarium.Dispatch.Worktree (
    WorktreeError (..),
    teardownWorktree,
    worktreeErrorText,
    worktreePath,
 )
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
            "Run one dispatch (with TASK_ID) or drain the ready queue in priority order (no TASK_ID, optionally --max N)."
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
                    <> help "Cap dispatches in queue mode (ignored with TASK_ID)"
                )
            )
        <*> (($ mempty) <$> routingP "this dispatch")
        <*> optional (textOption "base-branch" "NAME" "Override the base branch for git operations")
        <*> switch (long "dry-run" <> help "Build the plan and prompt; don't cut git or call claude")

runRun :: FilePath -> RunOpts -> IO ()
runRun db o = do
    cfg <- requireConfig
    case rTaskId o of
        Just rawId ->
            withDbSync db $ \c -> do
                tid <- resolveOrFatal (RT.resolveTaskId c rawId)
                -- A dry run previews; it must not move the task.
                before <- RT.getTask c tid
                mt <-
                    if rDryRun o
                        then pure (maybe RT.NoCandidate RT.Claimed before)
                        else do
                            owner <- defaultOwner
                            r <- RT.claimTask c tid owner
                            -- A busy lock claimed nothing; the log is
                            -- append-only, so a claim event written here
                            -- could never be retracted.
                            case r of
                                RT.Claimed _ ->
                                    forM_ before $ \t ->
                                        Ev.emit db "dispatch run" (Ev.TaskClaimed tid (taskState t) owner)
                                _ -> pure ()
                            pure r
                case mt of
                    RT.NoCandidate -> fatal 1 ("task not found: " <> T.unpack tid)
                    RT.LockBusy -> lockBusy ("icarium dispatch run " <> T.unpack tid)
                    RT.Claimed task -> do
                        eres <-
                            D.dispatch
                                c
                                D.DispatchRequest
                                    { D.drTask = task
                                    , D.drConfig = cfg
                                    , D.drDbPath = db
                                    , D.drDryRun = rDryRun o
                                    , D.drRouting = rRouting o
                                    , D.drBaseOverride = rBase o
                                    }
                        case eres of
                            Left err -> do
                                release db c o task
                                fatal 3 (T.unpack (worktreeErrorText err))
                            Right res -> do
                                D.applyOutcomeToTask c db task res
                                summarize res
                                autoMerge cfg c res >>= mapM_ (uncurry reportSingleAutoMerge)
        Nothing -> do
            forM_ (rMax o) $ \n ->
                when (n <= 0) $ fatal 2 "max must be > 0"
            sigCount <- newIORef (0 :: Int)
            let sigHandler = do
                    modifyIORef sigCount (+ 1)
                    n <- readIORef sigCount
                    when (n >= 2) $ do
                        void $ installHandler sigINT Default Nothing
                        raiseSignal sigINT
            void $ installHandler sigINT (Catch sigHandler) Nothing
            parkedCount <- newIORef (0 :: Int)
            withDbSync db $ \conn -> do
                let ctx =
                        DrainCtx
                            { dctxDb = db
                            , dctxOpts = o
                            , dctxCfg = cfg
                            , dctxMCap = rMax o
                            , dctxSigCount = sigCount
                            , dctxParkedCount = parkedCount
                            , dctxConn = conn
                            }
                drainLoop ctx 0
            parked <- readIORef parkedCount
            when (parked > 0) $
                fatal 3 "some dispatches stayed parked; land with `icarium dispatch merge --all`"

data DrainCtx = DrainCtx
    { dctxDb :: FilePath
    , dctxOpts :: RunOpts
    , dctxCfg :: Config
    , dctxMCap :: Maybe Int
    , dctxSigCount :: IORef Int
    , dctxParkedCount :: IORef Int
    , dctxConn :: Connection
    }

drainLoop :: DrainCtx -> Int -> IO ()
drainLoop ctx !i
    | Just cap <- dctxMCap ctx
    , i >= cap =
        hPutStrLn stderr ("icarium: reached max dispatches (" <> show cap <> "); stopping")
    | otherwise = do
        let conn = dctxConn ctx
            opts = dctxOpts ctx
            cfg = dctxCfg ctx
            db = dctxDb ctx
        -- Claiming is the selection: taking the queue head and marking it
        -- in_progress is one atomic step, so racing drains cannot pick the
        -- same task. A dry run previews the head instead, moving nothing.
        -- Headless only: 'ready_interactive' is work a human must do.
        mt <-
            if rDryRun opts
                then maybe RT.NoCandidate RT.Claimed . listToMaybe <$> RT.queueTasks conn [ReadyHeadless]
                else do
                    owner <- defaultOwner
                    r <- RT.claimNextTask conn [ReadyHeadless] owner
                    case r of
                        -- The queue it drew from names the state it came from.
                        RT.Claimed t ->
                            Ev.emit db "dispatch run" (Ev.TaskClaimed (taskId t) ReadyHeadless owner)
                        _ -> pure ()
                    pure r
        case mt of
            RT.NoCandidate -> hPutStrLn stderr "icarium: ready queue empty; stopping"
            -- Not an empty queue: stopping the drain here would silently leave
            -- ready work behind, so fail loudly instead.
            RT.LockBusy -> lockBusy "icarium dispatch run"
            RT.Claimed t -> do
                hPutStrLn stderr $ "icarium: dispatching " <> T.unpack (taskId t)
                eres <-
                    D.dispatch
                        conn
                        D.DispatchRequest
                            { D.drTask = t
                            , D.drConfig = cfg
                            , D.drDbPath = db
                            , D.drDryRun = rDryRun opts
                            , D.drRouting = rRouting opts
                            , D.drBaseOverride = rBase opts
                            }
                case eres of
                    -- No work started, so the claim is released either way:
                    -- capacity may free up later (back-pressure), while a
                    -- setup error would just repeat.
                    Left err@(WtNoCapacity _) -> do
                        release db conn opts t
                        hPutStrLn stderr $
                            "icarium: " <> T.unpack (worktreeErrorText err) <> "; stopping"
                    Left err -> do
                        release db conn opts t
                        fatal 3 (T.unpack (worktreeErrorText err))
                    Right res -> do
                        D.applyOutcomeToTask conn db t res
                        TIO.hPutStrLn stderr ("icarium: " <> Render.renderRunOutcome res)
                        printSummary res
                        stopped <-
                            autoMerge cfg conn res >>= \case
                                Nothing -> pure False
                                Just (d, out) -> case out of
                                    MergeLanded sha -> False <$ TIO.putStrLn (Render.renderLanded d sha)
                                    MergeBlocked _ note -> do
                                        modifyIORef (dctxParkedCount ctx) (+ 1)
                                        False <$ TIO.hPutStrLn stderr ("icarium: " <> Render.renderStillParked d note)
                                    -- Worktree back-pressure is machine-level: the next
                                    -- dispatch would hit the same wall, so stop the drain.
                                    MergeStopped note -> do
                                        modifyIORef (dctxParkedCount ctx) (+ 1)
                                        True <$ TIO.hPutStrLn stderr ("icarium: " <> note <> "; stopping")
                        n <- readIORef (dctxSigCount ctx)
                        -- A dry run leaves the task ready, so recursing would
                        -- preview the same head forever.
                        unless (stopped || rDryRun opts) $
                            if n >= 1
                                then
                                    hPutStrLn
                                        stderr
                                        "icarium: SIGINT received; stopping after current dispatch"
                                else drainLoop ctx (i + 1)

{- | Undo a claim whose dispatch never started (no-op under --dry-run,
which never claimed).
-}
release :: FilePath -> Connection -> RunOpts -> Task -> IO ()
release db conn opts t = unless (rDryRun opts) $ do
    RT.releaseTask conn (taskId t)
    Ev.emit db "dispatch run" (Ev.TaskUpdated (taskId t) InProgress ReadyHeadless)

{- | Land a just-successful dispatch immediately (attempt-then-park).
Returns Nothing when there is nothing to land: dry-run, non-success, or
already merged (no-commit successes are pre-stamped merged).
-}
autoMerge :: Config -> Connection -> D.DispatchResult -> IO (Maybe (Dispatch, MergeOutcome))
autoMerge cfg c res
    | Just did <- D.dresDispatchId res
    , OSuccess <- D.dresOutcome res =
        RD.getDispatch c did >>= \case
            Just d | Nothing <- dispatchMergeSha d -> Just . (d,) <$> mergeParked cfg c d
            _ -> pure Nothing
    | otherwise = pure Nothing

{- | Single-run reporting: a dispatch that stays parked is an incomplete
outcome — exit 3, naming the fixing command.
-}
reportSingleAutoMerge :: Dispatch -> MergeOutcome -> IO ()
reportSingleAutoMerge d = \case
    MergeLanded sha -> TIO.putStrLn (Render.renderLanded d sha)
    MergeBlocked _ note -> stillParked note
    MergeStopped note -> stillParked note
  where
    stillParked note = fatal 3 (T.unpack (Render.renderStillParked d note))

-- | Print the enriched summary block; does not exit on failure.
printSummary :: D.DispatchResult -> IO ()
printSummary r = do
    mLog <- maybe (pure Nothing) readLogResult (D.dresLogPath r)
    -- Diff against the dispatch branch, which still exists here: the
    -- auto-merge (which deletes it) runs after this summary. The branch may
    -- be gone for no-commit runs — changedFiles returns [] then.
    files <- maybe (pure []) (\sha -> Git.changedFiles "." sha (D.dresBranch r)) (D.dresBaseSha r)
    TIO.putStr (Render.renderRunSummary r mLog files)

summarize :: D.DispatchResult -> IO ()
summarize r = do
    printSummary r
    case D.dresOutcome r of
        OSuccess -> pure ()
        _ -> fatal 3 "dispatch did not succeed"

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
