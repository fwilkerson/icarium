module Icarium.Dispatch.PostClaude (
    PostClaudeResult (..),
    PostClaudeArgs (..),
    handlePostClaudeWithReview,
    writeWarnContextEntry,
    checkpointDirtyTree,
) where

import Control.Monad (forM_, mfilter, void, when)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection)
import System.Exit (ExitCode)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)

import Icarium.Bodies.Sweep (refreshTaskBody)
import Icarium.Config (Config (..), DispatchConfig (..), ReviewConfig (..))
import Icarium.Dispatch.BodyDiff (bodyChanged, diffBody, renderBodyReport)
import Icarium.Dispatch.Decide (
    Decision (..),
    DecisionInput (..),
    DecisionReason (..),
    GitSignals (..),
    decideOutcome,
    renderReason,
    reviewVerdict,
 )
import Icarium.Dispatch.Gate (GateEnv (..), GateHeartbeat (..), gateBudgetUsecs, runGates)
import Icarium.Dispatch.LogResult (LogResult (..), readLogResult)
import Icarium.Dispatch.Outcome (DispatchCtx (..), DispatchResult, FinishArgs (..), finishWith)
import Icarium.Dispatch.Payload (
    Finding,
    WorkerPayload (..),
    decodeWorkerPayload,
    renderFindings,
 )
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

{- | Post-run handling for one dispatch. Gathers the signals a finished run
left behind, hands them to 'decideOutcome', and acts on what comes back.

The expensive stages are skipped when the cheap signals already settle the
run — no gates on a tree that failed a guard, no reviewer on a failed gate.
'Decide' still owns the precedence; this only avoids paying for evidence that
could not change the answer.
-}
handlePostClaudeWithReview :: PostClaudeArgs -> IO PostClaudeResult
handlePostClaudeWithReview args = do
    mPayload <- readWorkerPayload (pcaLogPath args)
    porcelain <- Git.statusPorcelain wt
    mBranchSha <- Git.revParse wt branch
    let signals mGate mReview =
            DecisionInput
                { diNoCommit = taskNoCommit (pcaTask args)
                , diExit = pcaExit args
                , diTimeoutMinutes = dcMaxMinutesPerDispatch (cfgDispatch cfg)
                , diPayload = mPayload
                , diGit =
                    GitSignals
                        { gsPorcelain = porcelain
                        , gsBranchSha = first (T.pack . show) mBranchSha
                        , gsBaseSha = pcaBaseSha args
                        }
                , diGate = mGate
                , diReview = mReview
                }
        -- Gates run on behalf of this dispatch: they keep its heartbeat warm
        -- and, on expiry, leave the wedged process tree in its log.
        gateEnv =
            GateEnv
                { geDir = wt
                , geBudgetUsecs = gateBudgetUsecs (cfgDispatch cfg)
                , geLogPath = Just (pcaLogPath args)
                , geHeartbeat = Just GateHeartbeat{ghDbPath = dxDbPath dx, ghDid = did}
                }
    -- 'Clean' before any gate ran means nothing so far has settled the run:
    -- there is a commit to land, so the expensive stages are worth paying for.
    case dReason (decideOutcome (signals Nothing Nothing)) of
        Clean -> do
            either (const (pure ())) (RD.setLastCommit conn did) mBranchSha
            runGates gateEnv (cfgCommands cfg) >>= \case
                Left note -> settle args mPayload Nothing (decideOutcome (signals (Just (Left note)) Nothing))
                Right () -> do
                    mReviewed <- runReviewStage args
                    settle args mPayload (fst <$> mReviewed) $
                        decideOutcome (signals (Just (Right ())) (rrOutcome . snd <$> mReviewed))
        _ -> settle args mPayload Nothing (decideOutcome (signals Nothing Nothing))
  where
    dx = pcaCtx args
    cfg = pcaConfig args
    conn = dxConn dx
    did = dxDid dx
    branch = dxBranch dx
    wt = dxWorkDir dx

{- | Record the decision, then do the writes it implies. The dispatch branch is
left for the CLI layer's auto-merge (or @icarium dispatch merge@) to land —
nothing here touches the base branch.
-}
settle :: PostClaudeArgs -> Maybe WorkerPayload -> Maybe Task -> Decision -> IO PostClaudeResult
settle args mPayload mReviewedTask decision = do
    -- Preserve in-flight work on the dispatch branch: it is what survives the
    -- worktree teardown, and a human reads it after a failure.
    when (dOutcome decision == OFailure) $
        void (checkpointDirtyTree wt did (renderReason (dReason decision)))
    dr <-
        finishWith
            dx
            FinishArgs
                { faDecision = decision
                , faSha = Nothing
                , faRetention = dcLogRetentionRuns (cfgDispatch (pcaConfig args))
                , faLogPath = Just (pcaLogPath args)
                , faBaseSha = Just (pcaBaseSha args)
                , faPayload = mPayload
                }
    case dReason decision of
        -- Nothing to land: stamp merged so the dispatch never shows as
        -- parked. Ordered after finish, which writes merge_sha = NULL.
        NoCommitClean -> RD.setMerged conn did (pcaBaseSha args)
        ReviewerWarn findings -> forM_ mReviewedTask $ \task -> do
            cats <- RC.taskCategoriesFor conn (taskId task)
            writeWarnContextEntry conn (dxDbPath dx) did task cats findings
        _ -> pure ()
    pure (maybe (PCDone dr) (PCRetry dr) (dRetry decision))
  where
    dx = pcaCtx args
    conn = dxConn dx
    did = dxDid dx
    wt = dxWorkDir dx

{- | Run the reviewer, when one is configured, and record what it said.
Returns the refreshed task alongside, since a warn mints a ctx entry off it.
-}
runReviewStage :: PostClaudeArgs -> IO (Maybe (Task, ReviewResult))
runReviewStage args = case mfilter rcEnabled (cfgReview cfg) of
    Nothing -> pure Nothing
    Just rcfg -> do
        -- Mid-run body-file edits (e.g. a ## Proof section) must reach
        -- the reviewer; pcaTask predates the worker run. The section diff
        -- against the first-attempt baseline is the tamper signal: the
        -- body is worker-writable, so the reviewer must be told how the
        -- text it judges against changed under it.
        task <- refreshTaskBody conn db (pcaTask args)
        let bodyDiff = diffBody (pcaBaselineBody args) (taskBody task)
            reviewModel = fromMaybe (dcModel (cfgDispatch cfg)) (rcModel rcfg)
            reviewerLogPath = takeDirectory (pcaLogPath args) <> "/" <> T.unpack did <> "-reviewer.jsonl"
            maxMins = dcMaxMinutesPerDispatch (cfgDispatch cfg)
        diffText <- Git.diffPatch wt (pcaBaseSha args)
        rr <-
            runReviewer
                wt
                reviewModel
                (pcaSysPrompt args)
                (renderBodyReport bodyDiff)
                (taskTitle task)
                (taskBody task)
                diffText
                reviewerLogPath
                maxMins
        let verdict = reviewVerdict (rrOutcome rr)
        RD.setReviewInfo conn did verdict (rrLogPath rr) (bodyChanged bodyDiff)
        hPutStrLn stderr ("[reviewer] verdict: " <> T.unpack (reviewVerdictText verdict))
        pure (Just (task, rr))
  where
    dx = pcaCtx args
    cfg = pcaConfig args
    conn = dxConn dx
    db = dxDbPath dx
    did = dxDid dx
    wt = dxWorkDir dx

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
