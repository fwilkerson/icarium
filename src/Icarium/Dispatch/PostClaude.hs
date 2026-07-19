module Icarium.Dispatch.PostClaude (
    PostClaudeResult (..),
    handlePostClaude,
    handlePostClaudeWithReview,
    writeWarnContextEntry,
    checkpointDirtyTree,
    postClaudeGuard,
    runGate,
    runGates,
) where

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.Process.Typed (runProcess, setWorkingDir, shell)

import Icarium.Bodies.Sweep (refreshTaskBody)
import Icarium.Config (CommandsConfig (..), Config (..), DispatchConfig (..), ReviewConfig (..))
import Icarium.Dispatch.BodyDiff (bodyChanged, diffBody, renderBodyReport)
import Icarium.Dispatch.Outcome (DispatchCtx (..), DispatchResult, FinishArgs (..), finishWith)
import Icarium.Dispatch.Reviewer (ReviewResult (..), runReviewer)
import Icarium.Git qualified as Git
import Icarium.Node (createContextWithBody)
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Context qualified as RCx
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Edge qualified as RE
import Icarium.Types

data PostClaudeResult
    = PCDone DispatchResult
    | -- | Current dispatch closed with OFailure; Text = findings for next attempt.
      PCRetry DispatchResult Text

-- | Backward-compatible interface (no review). Used by tests.
handlePostClaude ::
    DispatchCtx ->
    Config ->
    Bool ->
    ExitCode ->
    Text ->
    FilePath ->
    IO DispatchResult
handlePostClaude dx cfg noCommit exit baseSha logPath = do
    res <- handlePostClaudeImpl dx cfg Nothing Nothing noCommit exit baseSha logPath
    pure $ case res of
        PCDone dr -> dr
        PCRetry dr _ -> dr

{- | Full interface used by the dispatch loop; runs reviewer when configured.
The reviewer system prompt (already loaded, before the worker started —
see 'Icarium.Dispatch.Reviewer.loadReviewerPrompt') is passed in rather than
read here, so the reviewer never sees a prompt the worker had a chance to
influence mid-run. @baselineBody@ is the task body at FIRST attempt start —
the tamper baseline for the body-change report (retries keep the original,
so a failed attempt cannot launder its edits into a clean baseline).
-}
handlePostClaudeWithReview ::
    DispatchCtx ->
    Config ->
    Task ->
    Text ->
    Maybe Text ->
    Bool ->
    ExitCode ->
    Text ->
    FilePath ->
    IO PostClaudeResult
handlePostClaudeWithReview dx cfg task baselineBody =
    handlePostClaudeImpl dx cfg (Just (task, baselineBody))

-- =============================================================
-- Internal implementation
-- =============================================================

handlePostClaudeImpl ::
    DispatchCtx ->
    Config ->
    Maybe (Task, Text) ->
    Maybe Text ->
    Bool ->
    ExitCode ->
    Text ->
    FilePath ->
    IO PostClaudeResult
handlePostClaudeImpl dx cfg mTask mSysPrompt noCommit exit baseSha logPath = do
    let conn = dxConn dx
        did = dxDid dx
        branch = dxBranch dx
        wt = dxWorkDir dx
        ret = dcLogRetentionRuns (cfgDispatch cfg)
        maxMins = dcMaxMinutesPerDispatch (cfgDispatch cfg)
        finish o mSha notes =
            finishWith
                dx
                FinishArgs
                    { faOutcome = o
                    , faSha = mSha
                    , faNotes = notes
                    , faRetention = ret
                    , faLogPath = Just logPath
                    , faBaseSha = Just baseSha
                    }
        checkExit = case exit of
            ExitFailure 124 -> throwE ("timed out after " <> T.pack (show maxMins) <> " minutes")
            ExitFailure c -> throwE ("claude exited " <> T.pack (show c))
            ExitSuccess -> pure ()
        -- Returns Nothing for no-commit success, Just () when gates passed.
        preStep = do
            checkExit
            porcelain <- liftIO (Git.statusPorcelain wt)
            mBranchSha <- liftIO (Git.revParse wt branch)
            mapM_ throwE (postClaudeGuard noCommit porcelain mBranchSha baseSha)
            if noCommit
                then pure Nothing
                else do
                    liftIO $ case mBranchSha of
                        Right sha -> RD.setLastCommit conn did sha
                        Left _ -> pure ()
                    liftIO (runGates wt cfg) >>= either throwE pure
                    pure (Just ())

    runExceptT preStep >>= \case
        Left notes -> do
            void (checkpointDirtyTree wt did notes)
            PCDone <$> finish OFailure Nothing notes
        Right Nothing -> do
            -- Nothing to land: stamp merged so the dispatch never shows as
            -- parked. Ordered after finish, which writes merge_sha = NULL.
            dr <- finish OSuccess Nothing "no-commit task"
            RD.setMerged conn did baseSha
            pure (PCDone dr)
        Right (Just ()) ->
            runReviewThenPark dx cfg mTask mSysPrompt finish logPath maxMins baseSha

{- | Reviewer gate, then park: the dispatch branch is left for the CLI
layer's auto-merge (or @icarium dispatch merge@) to land. Nothing here
touches the base branch.
-}
runReviewThenPark ::
    DispatchCtx ->
    Config ->
    Maybe (Task, Text) ->
    Maybe Text ->
    (DispatchOutcome -> Maybe Text -> Text -> IO DispatchResult) ->
    FilePath ->
    Int ->
    Text ->
    IO PostClaudeResult
runReviewThenPark dx cfg mTask mSysPrompt finish logPath maxMins baseSha = do
    let conn = dxConn dx
        db = dxDbPath dx
        did = dxDid dx
        wt = dxWorkDir dx
        activeReview = do
            (task, baseline) <- mTask
            rc <- cfgReview cfg
            if rcEnabled rc then Just (task, baseline, rc) else Nothing
    mReviewResult <- case activeReview of
        Nothing -> pure Nothing
        Just (task0, baselineBody, rcfg) -> do
            -- Mid-run body-file edits (e.g. a ## Proof section) must reach
            -- the reviewer; task0 predates the worker run. The section diff
            -- against the first-attempt baseline is the tamper signal: the
            -- body is worker-writable, so the reviewer must be told how the
            -- text it judges against changed under it.
            task <- refreshTaskBody conn db task0
            let bodyDiff = diffBody baselineBody (taskBody task)
                reviewModel = fromMaybe (dcModel (cfgDispatch cfg)) (rcModel rcfg)
                reviewerLogPath = takeDirectory logPath <> "/" <> T.unpack did <> "-reviewer.jsonl"
            diffText <- Git.diffPatch wt baseSha
            rr <- runReviewer wt reviewModel mSysPrompt (renderBodyReport bodyDiff) (taskTitle task) (taskBody task) diffText reviewerLogPath maxMins
            RD.setReviewInfo conn did (rrVerdict rr) (rrLogPath rr) (bodyChanged bodyDiff)
            hPutStrLn stderr ("[reviewer] verdict: " <> T.unpack (reviewVerdictText (rrVerdict rr)))
            pure (Just (task, rr))
    case mReviewResult of
        Just (_, rr) | rrVerdict rr == RVFail -> do
            let findings = rrFindings rr
            dr <- finish OFailure Nothing ("reviewer: fail\n" <> findings)
            pure (PCRetry dr findings)
        _ -> do
            -- Parked-ness is derived (merge_sha NULL), so the note records
            -- only what stays true after the branch lands.
            let notes = case mReviewResult of
                    Just (_, rr)
                        | rrVerdict rr == RVWarn ->
                            "reviewer warn\n" <> rrFindings rr
                    _ -> "gates passed"
            dr <- finish OSuccess Nothing notes
            case mReviewResult of
                Just (task, rr) | rrVerdict rr == RVWarn -> do
                    cats <- RC.taskCategoriesFor conn (taskId task)
                    writeWarnContextEntry conn db task cats (rrFindings rr)
                _ -> pure ()
            pure (PCDone dr)

writeWarnContextEntry :: Connection -> FilePath -> Task -> [Category] -> Text -> IO ()
writeWarnContextEntry conn db task cats findings = do
    (cid, _) <-
        createContextWithBody
            conn
            db
            RCx.NewContext
                { RCx.ncTitle = "reviewer warn: " <> taskTitle task
                , RCx.ncBody = findings
                }
    forM_ cats $ \cat -> RC.attachContextCategory conn cid (categoryId cat)
    -- Link the note back to its task for provenance (task references ctx).
    void $ RE.insertEdge conn References TaskNode (taskId task) ContextNode cid

{- | If the given worktree is dirty, commit everything to its current branch
with a wip message. Preserves in-flight work on the dispatch branch so a
human can inspect it after a failure. Returns whether the tree was dirty
(and therefore checkpointed).
-}
checkpointDirtyTree :: FilePath -> Text -> Text -> IO Bool
checkpointDirtyTree wt did note = do
    porcelain <- Git.statusPorcelain wt
    if T.null (T.strip porcelain)
        then pure False
        else do
            let shortNote = T.take 60 (T.takeWhile (/= '\n') note)
                msg = "wip: dispatch " <> did <> " (failed: " <> shortNote <> ")"
            void $ Git.commitAll wt msg
            pure True

{- | Pure guard logic for the post-claude checks. Returns Just an error
message if a guard fires, Nothing if all pass. The Bool is whether this is
a no-commit task.
  * Dirty-tree guard fires in both modes when @porcelain@ (raw
    `git status --porcelain` output) is non-empty after stripping.
  * No-commit mode: fires when the branch SHA resolved and differs from
    baseSha — the agent committed despite being told not to.
  * Commit mode: fires when the branch SHA equals baseSha (agent exited
    success but made no commits).
-}
postClaudeGuard :: Bool -> Text -> Either e Text -> Text -> Maybe Text
postClaudeGuard noCommit porcelain mBranchSha baseSha
    | not (T.null porcStripped) = Just dirtyMsg
    | noCommit = case branchSha of
        Just sha | sha /= baseSha -> Just "no-commit task: agent left commits on dispatch branch (branch retained for inspection)"
        _ -> Nothing
    | branchSha == Just baseSha = Just "agent made no commits on dispatch branch"
    | otherwise = Nothing
  where
    porcStripped = T.strip porcelain
    branchSha = either (const Nothing) Just mBranchSha
    dirtyMsg =
        "agent left uncommitted changes; refusing to accept\nuncommitted:\n"
            <> T.intercalate "\n" (map ("  " <>) (T.lines porcStripped))

{- | Run the configured build and test gates in order, stopping at the
first failure. This owns the gate contract — which commands run, in what
order, and the missing-config message — for both the post-claude check
and the merge rebase path.
-}
runGates :: FilePath -> Config -> IO (Either Text ())
runGates dir cfg = runExceptT $ do
    cc <- maybe (throwE "no [commands] section configured") pure (cfgCommands cfg)
    ExceptT (runGate dir (ccBuild cc))
    ExceptT (runGate dir (ccTest cc))

{- | Run a shell command (as a single string, so users can include
pipes and &&) inside the given directory. Returns () on exit 0;
otherwise a short note.
-}
runGate :: FilePath -> Text -> IO (Either Text ())
runGate dir cmdText
    | T.null (T.strip cmdText) = pure (Right ())
    | otherwise = do
        code <- runProcess (setWorkingDir dir (shell (T.unpack cmdText)))
        pure $ case code of
            ExitSuccess -> Right ()
            ExitFailure c -> Left (cmdText <> " -> exit " <> T.pack (show c))
