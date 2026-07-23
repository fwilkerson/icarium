module Icarium.Dispatch.PostClaude (
    PostClaudeResult (..),
    PostClaudeArgs (..),
    handlePostClaudeWithReview,
    writeWarnContextEntry,
    checkpointDirtyTree,
    postClaudeGuard,
) where

import Control.Monad (forM_, mfilter, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)

import Icarium.Bodies.Sweep (refreshTaskBody)
import Icarium.Config (Config (..), DispatchConfig (..), ReviewConfig (..))
import Icarium.Dispatch.BodyDiff (bodyChanged, diffBody, renderBodyReport)
import Icarium.Dispatch.Gate (GateEnv (..), GateHeartbeat (..), gateBudgetUsecs, runGates)
import Icarium.Dispatch.LogResult (LogResult (..), readLogResult)
import Icarium.Dispatch.Outcome (DispatchCtx (..), DispatchResult, FinishArgs (..), finishWith)
import Icarium.Dispatch.Payload (
    Finding,
    WorkerPayload (..),
    WorkerStatus (..),
    decodeWorkerPayload,
    renderFindings,
 )
import Icarium.Dispatch.Reviewer (ReviewResult (..), rrReport, rrVerdict, runReviewer)
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

data PostClaudeArgs = PostClaudeArgs
    { pcaCtx :: DispatchCtx
    , pcaConfig :: Config
    , pcaTask :: Task
    , pcaBaselineBody :: Text
    {- ^ Task body at FIRST attempt start — the tamper baseline for the
    body-change report (retries keep the original, so a failed attempt
    cannot launder its edits into a clean baseline).
    -}
    , pcaSysPrompt :: Maybe Text
    {- ^ Reviewer system prompt, loaded before the worker started (see
    'Icarium.Dispatch.Reviewer.loadReviewerPrompt') rather than read
    here, so the reviewer never sees a prompt the worker had a chance
    to influence mid-run.
    -}
    , pcaExit :: ExitCode
    , pcaBaseSha :: Text
    , pcaLogPath :: FilePath
    }

-- | Post-run handling for one dispatch; runs the reviewer when configured.
handlePostClaudeWithReview :: PostClaudeArgs -> IO PostClaudeResult
handlePostClaudeWithReview args = do
    mPayload <- readWorkerPayload (pcaLogPath args)
    let PostClaudeArgs
            { pcaCtx = dx
            , pcaConfig = cfg
            , pcaExit = exit
            , pcaBaseSha = baseSha
            , pcaLogPath = logPath
            } = args
        conn = dxConn dx
        did = dxDid dx
        branch = dxBranch dx
        wt = dxWorkDir dx
        noCommit = taskNoCommit (pcaTask args)
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
                    , faPayload = mPayload
                    }
        checkExit = case exit of
            ExitFailure 124 -> throwE ("timed out after " <> T.pack (show maxMins) <> " minutes")
            ExitFailure c -> throwE ("claude exited " <> T.pack (show c))
            ExitSuccess -> pure ()
        -- A block is the worker's one unilateral claim (ADR 0008): it did not
        -- deliver the task, so the dispatch did not succeed and its branch must
        -- not auto-merge. Checked ahead of the guards so the recorded note is
        -- the worker's own reason rather than the generic "made no commits".
        checkWorkerBlocked = case mPayload of
            Just p
                | WBlocked <- wpStatus p ->
                    throwE ("worker blocked: " <> fromMaybe "no reason given" (wpBlockReason p))
            _ -> pure ()
        -- Returns Nothing for no-commit success, Just () when gates passed.
        preStep = do
            checkExit
            checkWorkerBlocked
            porcelain <- liftIO (Git.statusPorcelain wt)
            mBranchSha <- liftIO (Git.revParse wt branch)
            mapM_ throwE (postClaudeGuard noCommit porcelain mBranchSha baseSha)
            if noCommit
                then pure Nothing
                else do
                    liftIO $ case mBranchSha of
                        Right sha -> RD.setLastCommit conn did sha
                        Left _ -> pure ()
                    liftIO (runGates gates (cfgCommands cfg)) >>= either throwE pure
                    pure (Just ())
        -- Gates run on behalf of this dispatch: they keep its heartbeat warm
        -- and, on expiry, leave the wedged process tree in its log.
        gates =
            GateEnv
                { geDir = wt
                , geBudgetUsecs = gateBudgetUsecs (cfgDispatch cfg)
                , geLogPath = Just logPath
                , geHeartbeat = Just GateHeartbeat{ghDbPath = dxDbPath dx, ghDid = did}
                }

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
            runReviewThenPark args finish

{- | Reviewer gate, then park: the dispatch branch is left for the CLI
layer's auto-merge (or @icarium dispatch merge@) to land. Nothing here
touches the base branch.
-}
runReviewThenPark ::
    PostClaudeArgs ->
    (DispatchOutcome -> Maybe Text -> Text -> IO DispatchResult) ->
    IO PostClaudeResult
runReviewThenPark args finish = do
    let PostClaudeArgs
            { pcaCtx = dx
            , pcaConfig = cfg
            , pcaTask = task0
            , pcaBaselineBody = baselineBody
            , pcaSysPrompt = mSysPrompt
            , pcaBaseSha = baseSha
            , pcaLogPath = logPath
            } = args
        conn = dxConn dx
        db = dxDbPath dx
        did = dxDid dx
        wt = dxWorkDir dx
        maxMins = dcMaxMinutesPerDispatch (cfgDispatch cfg)
    mReviewResult <- case mfilter rcEnabled (cfgReview cfg) of
        Nothing -> pure Nothing
        Just rcfg -> do
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
            let report = rrReport rr
            dr <- finish OFailure Nothing ("reviewer: fail\n" <> report)
            pure (PCRetry dr report)
        _ -> do
            -- Parked-ness is derived (merge_sha NULL), so the note records
            -- only what stays true after the branch lands.
            let notes = case mReviewResult of
                    Just (_, rr)
                        | rrVerdict rr == RVWarn ->
                            "reviewer warn\n" <> rrReport rr
                    _ -> "gates passed"
            dr <- finish OSuccess Nothing notes
            case mReviewResult of
                -- Right by construction: a warn verdict comes from findings.
                Just (task, rr)
                    | rrVerdict rr == RVWarn
                    , Right fs <- rrOutcome rr -> do
                        cats <- RC.taskCategoriesFor conn (taskId task)
                        writeWarnContextEntry conn db did task cats fs
                _ -> pure ()
            pure (PCDone dr)

{- | The worker's final message is a 'workerSchema' payload, validated by the
harness. Absent means it never reached a final message (timeout, kill);
undecodable means @--json-schema@ did not take, which is worth saying out loud
because every mutation the payload implies is silently skipped.
-}
readWorkerPayload :: FilePath -> IO (Maybe WorkerPayload)
readWorkerPayload logPath = do
    mLR <- readLogResult logPath
    case mfilter (not . T.null) (mLR >>= lrResultText) of
        Nothing -> pure Nothing
        Just txt -> case decodeWorkerPayload txt of
            Right p -> pure (Just p)
            Left e -> do
                hPutStrLn stderr ("icarium: worker payload not decodable: " <> T.unpack e)
                pure Nothing

{- | One ctx entry per warned dispatch, not per finding: the entry records that
this review happened, and @/curate-ctx@ promotes it as a whole. The body is the
findings table.
-}
writeWarnContextEntry :: Connection -> FilePath -> Text -> Task -> [Category] -> [Finding] -> IO ()
writeWarnContextEntry conn db did task cats findings = do
    (cid, _) <-
        createContextWithBody
            conn
            db
            RCx.NewContext
                { RCx.ncTitle = "reviewer warn: " <> taskTitle task
                , RCx.ncBody = renderFindings findings
                , RCx.ncSourceDispatch = Just did
                }
    forM_ cats (RC.attachContextCategory conn cid)
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
