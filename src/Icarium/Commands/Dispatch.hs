module Icarium.Commands.Dispatch (Command, parser, run, printSummary) where

import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Either (fromRight)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (nub)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection)
import Options.Applicative
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals (Handler (..), installHandler, raiseSignal, sigINT)
import Text.Printf (printf)

import Icarium.Dispatch.LogResult (
    LogResult (..),
    LogUsage (..),
    fmtMs,
    readLogResult,
    trimResult,
 )
import Icarium.Git qualified as Git

import Icarium.Commands.Util
import Icarium.Config (
    CommandsConfig (..),
    Config,
    DispatchConfig (..),
    cfgCommands,
    cfgDispatch,
    defaultConfigPath,
    loadConfig,
 )
import Icarium.Db (parseDbTime, withDbReadOnly, withDbSync)
import Icarium.Dispatch qualified as D
import Icarium.Dispatch.PostClaude (runGate)
import Icarium.Dispatch.Worktree (
    WorktreeError (..),
    rebuildWorktree,
    teardownWorktree,
    worktreeErrorText,
 )
import Icarium.Heartbeat (heartbeatStale, pidAlive)
import Icarium.Render qualified as Render
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Edge qualified as RE
import Icarium.Repo.Task qualified as RT
import Icarium.Types

data Command
    = Run RunOpts
    | List ListOpts
    | Show ShowOpts
    | Logs LogsOpts
    | Recover RecoverOpts
    | Merge MergeOpts

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
        )

run :: FilePath -> Command -> IO ()
run db = \case
    Run o -> runRun db o
    List o -> runList db o
    Show o -> runShow db o
    Logs o -> runLogs db o
    Recover o -> runRecover db o
    Merge o -> runMerge db o

-- =============================================================
-- run  (single-task dispatch or queue drain)
-- =============================================================

data RunOpts = RunOpts
    { rTaskId :: Maybe Text
    , rMax :: Maybe Int
    , rModel :: Maybe Text
    , rEffort :: Maybe Effort
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
        <*> optional (textOption "model" "MODEL" "Override the model for this dispatch")
        <*> optional
            ( option
                effortReader
                ( long "effort"
                    <> metavar "LEVEL"
                    <> help "low | medium | high"
                )
            )
        <*> optional (textOption "base-branch" "NAME" "Override the base branch for git operations")
        <*> switch (long "dry-run" <> help "Build the plan and prompt; don't cut git or call claude")

runRun :: FilePath -> RunOpts -> IO ()
runRun db o = do
    cfg <-
        loadConfig defaultConfigPath >>= \case
            Left e -> fatal 2 ("config parse error:\n" <> e)
            Right c -> pure c
    case rTaskId o of
        Just rawId ->
            withDbSync db $ \c -> do
                tid <- resolveOrFatal (RT.resolveTaskId c rawId)
                mt <- RT.getTask c tid
                case mt of
                    Nothing -> fatal 1 ("task not found: " <> T.unpack tid)
                    Just task -> do
                        eres <-
                            D.dispatch
                                c
                                D.DispatchRequest
                                    { D.drTask = task
                                    , D.drConfig = cfg
                                    , D.drDbPath = db
                                    , D.drDryRun = rDryRun o
                                    , D.drModelOverride = rModel o
                                    , D.drEffortOverride = rEffort o
                                    , D.drBaseOverride = rBase o
                                    }
                        case eres of
                            Left err -> fatal 3 (T.unpack (worktreeErrorText err))
                            Right res -> do
                                D.applyOutcomeToTask c task res
                                summarize res
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
            withDbSync db $ \conn -> do
                let ctx =
                        DrainCtx
                            { dctxDb = db
                            , dctxOpts = o
                            , dctxCfg = cfg
                            , dctxMCap = rMax o
                            , dctxSigCount = sigCount
                            , dctxConn = conn
                            }
                drainLoop ctx 0

data DrainCtx = DrainCtx
    { dctxDb :: FilePath
    , dctxOpts :: RunOpts
    , dctxCfg :: Config
    , dctxMCap :: Maybe Int
    , dctxSigCount :: IORef Int
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
        ts <- RT.listTasks conn [] True Nothing Nothing
        case ts of
            [] -> hPutStrLn stderr "icarium: ready queue empty; stopping"
            (t : _) -> do
                hPutStrLn stderr $ "icarium: dispatching " <> T.unpack (taskId t)
                eres <-
                    D.dispatch
                        conn
                        D.DispatchRequest
                            { D.drTask = t
                            , D.drConfig = cfg
                            , D.drDbPath = db
                            , D.drDryRun = rDryRun opts
                            , D.drModelOverride = rModel opts
                            , D.drEffortOverride = rEffort opts
                            , D.drBaseOverride = rBase opts
                            }
                case eres of
                    -- The task was never touched; capacity may free up later
                    -- (back-pressure), while a setup error would just repeat.
                    Left err@(WtNoCapacity _) ->
                        hPutStrLn stderr $
                            "icarium: " <> T.unpack (worktreeErrorText err) <> "; stopping"
                    Left err -> fatal 3 (T.unpack (worktreeErrorText err))
                    Right res -> do
                        D.applyOutcomeToTask conn t res
                        TIO.hPutStrLn stderr $
                            "icarium: "
                                <> dispatchOutcomeText (D.dresOutcome res)
                                <> " \x2014 "
                                <> D.dresNotes res
                        printSummary res
                        n <- readIORef (dctxSigCount ctx)
                        if n >= 1
                            then
                                hPutStrLn
                                    stderr
                                    "icarium: SIGINT received; stopping after current dispatch"
                            else drainLoop ctx (i + 1)

-- | Print the enriched summary block; does not exit on failure.
printSummary :: D.DispatchResult -> IO ()
printSummary r = do
    let idPart = fromMaybe "(dry-run)" (D.dresDispatchId r)
    TIO.putStrLn ""
    TIO.putStrLn $ "dispatch: " <> idPart
    TIO.putStrLn $ "outcome:  " <> dispatchOutcomeText (D.dresOutcome r)
    TIO.putStrLn $ "branch:   " <> D.dresBranch r
    TIO.putStrLn $ "notes:    " <> D.dresNotes r
    case D.dresLogPath r of
        Nothing -> pure ()
        Just lp -> do
            mLR <- readLogResult lp
            case mLR of
                Nothing -> pure ()
                Just lr -> do
                    mapM_
                        TIO.putStrLn
                        [ "turns:    " <> maybe "-" (T.pack . show) (lrNumTurns lr)
                        , "duration: "
                            <> maybe "-" fmtMs (lrDurationMs lr)
                            <> maybe "" (\a -> " (api: " <> fmtMs a <> ")") (lrDurationApiMs lr)
                        , "cost:     " <> maybe "-" (T.pack . printf "$%.4f") (lrCostUsd lr)
                        , "tokens:   " <> fmtTokens (lrUsage lr)
                        ]
                    case lrResultText lr >>= \t -> if T.null t then Nothing else Just t of
                        Nothing -> pure ()
                        Just t -> TIO.putStrLn $ "result:   " <> trimResult t
    case D.dresBaseSha r of
        Nothing -> pure ()
        Just sha -> do
            -- Diff against the dispatch branch: the invoking checkout's HEAD
            -- never advances now that successful runs park. The branch may
            -- already be gone (no-commit) — changedFiles returns [] then.
            files <- Git.changedFiles "." sha (D.dresBranch r)
            case files of
                [] -> pure ()
                _ -> do
                    let shown = take 10 files
                        extra = length files - length shown
                        pad = T.replicate 10 " "
                        extraLine = [T.pack (show extra) <> " more" | extra > 0]
                        allItems = shown ++ extraLine
                    TIO.putStrLn $ "files:    " <> T.intercalate ("\n" <> pad) allItems
  where
    fmtTokens Nothing = "-"
    fmtTokens (Just u) =
        "in "
            <> maybe "-" (T.pack . show) (luInputTokens u)
            <> " / out "
            <> maybe "-" (T.pack . show) (luOutputTokens u)
            <> " / cache "
            <> maybe "-" (T.pack . show) (luCacheReads u)

summarize :: D.DispatchResult -> IO ()
summarize r = do
    printSummary r
    case D.dresOutcome r of
        OSuccess -> pure ()
        _ -> fatal 3 "dispatch did not succeed"

-- =============================================================
-- list
-- =============================================================

formatDispatchDuration :: UTCTime -> Dispatch -> Text
formatDispatchDuration now d =
    case parseDbTime (dispatchStartedAt d) of
        Nothing -> ""
        Just start ->
            let (diff, isOpen) = case dispatchEndedAt d >>= parseDbTime of
                    Just end -> (diffUTCTime end start, False)
                    Nothing -> (diffUTCTime now start, True)
                secs = max 0 (round (toRational diff) :: Int)
                body = Render.fmtSecs secs
             in if isOpen then body <> " (running)" else body

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
runList db o = withDbReadOnly db $ \c -> do
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
        length
            <$> RE.contextDerivedFromDispatch
                c
                (dispatchTaskId d)
                (dispatchStartedAt d)
                (dispatchEndedAt d)
    let rows =
            zipWith
                ( \d kc ->
                    Render.DispatchRow
                        { Render.drDispatch = d
                        , Render.drTaskTitle = fromMaybe "" (lookup (dispatchTaskId d) titleMap)
                        , Render.drCtxCount = kc
                        , Render.drDuration = formatDispatchDuration now d
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
runShow db o = withDbReadOnly db $ \c -> do
    did <- resolveOrFatal (RD.resolveDispatchId c (sId o))
    md <- RD.getDispatch c did
    case md of
        Nothing -> fatal 1 ("dispatch not found: " <> T.unpack did)
        Just d -> do
            mt <- RT.getTask c (dispatchTaskId d)
            ks <-
                RE.contextDerivedFromDispatch
                    c
                    (dispatchTaskId d)
                    (dispatchStartedAt d)
                    (dispatchEndedAt d)
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
runLogs db o = withDbReadOnly db $ \c -> do
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
    cfg <-
        loadConfig defaultConfigPath >>= \case
            Left e -> fatal 2 ("config parse error:\n" <> e)
            Right c -> pure c
    let staleSec = dcHeartbeatStaleSeconds (cfgDispatch cfg)
    withDbReadOnly db $ \c -> do
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
                forM_ open (reconcileDispatch c now staleSec)

reconcileDispatch :: Connection -> UTCTime -> Int -> Dispatch -> IO ()
reconcileDispatch c now staleSec d = do
    alive <- maybe (pure False) pidAlive (dispatchPid d)
    let stale = heartbeatStale now staleSec (dispatchHeartbeat d)
    if alive && not stale
        then pure ()
        else do
            uncommitted <- fmap not (Git.isClean ".")
            lastCommit <- fmap (fromRight "") (Git.revParse "." (dispatchBranch d))
            let notes =
                    T.intercalate
                        "; "
                        [ "interrupted"
                        , "alive=" <> boolText alive
                        , "stale=" <> boolText stale
                        , "uncommitted=" <> boolText uncommitted
                        , "last_commit=" <> lastCommit
                        ]
            RD.finishDispatch c (dispatchId d) OInterrupted Nothing (Just notes)
            void $
                RT.updateTask
                    c
                    (dispatchTaskId d)
                    RT.emptyUpdate
                        { RT.tuState = Just Blocked
                        , RT.tuBlockReason = Just (Just notes)
                        }
            TIO.putStrLn $
                "dispatch:"
                    <> dispatchId d
                    <> "  task:"
                    <> dispatchTaskId d
                    <> "  branch:"
                    <> dispatchBranch d
                    <> "  "
                    <> notes

boolText :: Bool -> Text
boolText True = "yes"
boolText False = "no"

-- =============================================================
-- merge  (land a parked dispatch on its base)
-- =============================================================

newtype MergeOpts = MergeOpts {mId :: Text}

mergeP :: Parser MergeOpts
mergeP =
    MergeOpts . T.pack
        <$> strArgument (metavar "DISPATCH_ID")

{- | Merge-queue semantics: fast-forward when the base hasn't moved since
the park; otherwise rebase the branch in a rebuilt worktree, re-run the
gates against the post-rebase state, then fast-forward. A conflict or
gate failure leaves the dispatch parked with updated notes.
-}
runMerge :: FilePath -> MergeOpts -> IO ()
runMerge db o = do
    cfg <-
        loadConfig defaultConfigPath >>= \case
            Left e -> fatal 2 ("config parse error:\n" <> e)
            Right c -> pure c
    withDbSync db $ \c -> do
        did <- resolveOrFatal (RD.resolveDispatchId c (mId o))
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
        let branch = dispatchBranch d
            base = dispatchBaseBranch d
        Git.revParse "." branch >>= \case
            Left _ -> fatal 1 ("branch missing (deleted manually?): " <> T.unpack branch)
            Right _ -> pure ()
        ffPossible <- Git.mergeBaseIsAncestor "." base branch
        unless ffPossible $ rebaseThenGate cfg c did branch base
        landFF did base branch
        newSha <- either (fatal 1 . show) pure =<< Git.revParse "." base
        RD.setMerged c did newSha
        -- `branch -d` checks merged-ness against HEAD, which may be an
        -- unrelated checkout; the sha equality proves the branch landed.
        branchSha <- fromRight "" <$> Git.revParse "." branch
        when (branchSha == newSha) $ void (Git.deleteBranchForce "." branch)
        TIO.putStrLn $
            "merged " <> T.take 10 did <> ": " <> base <> " -> " <> T.take 10 newSha

{- | Rebase the parked branch onto the current base tip and re-run the
gates there. On success the branch fast-forwards from base. Note the
rebase rewrites the parked branch even when the gates then fail — the
pre-rebase commits stay reachable via the reflog, and retrying the merge
after a fix is a plain FF.
-}
rebaseThenGate :: Config -> Connection -> Text -> Text -> Text -> IO ()
rebaseThenGate cfg conn did branch base = do
    let dcfg = cfgDispatch cfg
    hPutStrLn stderr ("icarium: base moved since park; rebasing " <> T.unpack branch)
    wt <-
        rebuildWorktree "." dcfg did branch
            >>= either (fatal 3 . T.unpack . worktreeErrorText) pure
    Git.rebase wt base >>= \case
        Left _ -> do
            Git.rebaseAbort wt
            teardownWorktree "." dcfg wt
            RD.updateNotes conn did ("merge conflict; needs manual rebase onto " <> base)
            fatal 3 ("rebase onto " <> T.unpack base <> " failed; dispatch stays parked")
        Right () -> do
            cc <- maybe (fatal 2 "no [commands] section configured") pure (cfgCommands cfg)
            gateResult <- runExceptT $ do
                ExceptT (runGate wt (ccBuild cc))
                ExceptT (runGate wt (ccTest cc))
            case gateResult of
                Left note -> do
                    teardownWorktree "." dcfg wt
                    RD.updateNotes conn did ("gates failed after rebase: " <> note)
                    fatal 3 ("gates failed after rebase; dispatch stays parked: " <> T.unpack note)
                Right () -> teardownWorktree "." dcfg wt

-- | Fast-forward base to the dispatch branch, wherever base lives.
landFF :: Text -> Text -> Text -> IO ()
landFF did base branch = do
    mWhere <- Git.branchCheckedOutAt "." base
    here <- either (const Nothing) (Just . T.unpack) <$> Git.topLevel "."
    case mWhere of
        -- Base checked out nowhere: FF its ref via a throwaway worktree.
        Nothing -> do
            let path = ".icarium/wt/merge-" <> T.unpack did
            void (Git.worktreeRemove "." path True)
            Git.worktreePrune "."
            Git.worktreeAddExisting "." path base
                >>= either (fatal 1 . ("cannot create merge worktree: " <>) . show) pure
            r <- Git.ffMerge path branch
            void (Git.worktreeRemove "." path True)
            Git.worktreePrune "."
            either (fatal 1 . ("ff-merge failed: " <>) . show) pure r
        Just wtPath
            | Just wtPath == here -> do
                clean <- Git.isClean "."
                unless clean $
                    fatal 1 "base is checked out here with a dirty tree; commit or stash first"
                Git.ffMerge "." branch
                    >>= either (fatal 1 . ("ff-merge failed: " <>) . show) pure
            | otherwise ->
                fatal 1 $
                    "base branch "
                        <> T.unpack base
                        <> " is checked out at "
                        <> wtPath
                        <> "; merge there or free it first"
