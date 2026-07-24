module Icarium.Dispatch.Outcome (
    DispatchCtx (..),
    DispatchResult (..),
    FinishArgs (..),
    applyOutcomeToTask,
    finishWith,
) where

import Control.Monad (forM_, void, when)
import Data.Bifunctor (second)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import System.Directory (doesFileExist, removeFile)

import Icarium.Dispatch.Decide (Decision (..), renderReason)
import Icarium.Dispatch.Payload (FutureNote (..), WorkerPayload (..), WorkerStatus (..))
import Icarium.Events qualified as Ev
import Icarium.Git qualified as Git
import Icarium.Node (createContextWithBody)
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Context qualified as RCx
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Edge qualified as RE
import Icarium.Repo.Task qualified as RT
import Icarium.Types

data DispatchCtx = DispatchCtx
    { dxConn :: Connection
    , dxDbPath :: FilePath
    , dxDid :: Text
    , dxBranch :: Text
    , dxBase :: Text
    , dxWorkDir :: FilePath
    -- ^ The dispatch worktree; every git and gate operation targets it.
    }

data DispatchResult = DispatchResult
    { dresDispatchId :: Maybe Text
    , dresOutcome :: DispatchOutcome
    , dresBranch :: Text
    , dresNotes :: Text
    , dresLogPath :: Maybe FilePath
    , dresBaseSha :: Maybe Text
    , dresPayload :: Maybe WorkerPayload
    {- ^ The worker's schema-validated return; 'Nothing' when it never got as
    far as a final message (timeout, kill) or emitted something undecodable.
    -}
    , dresTaskTransition :: Maybe (TaskState, Maybe Text)
    {- ^ The task state this dispatch decided on, and the block reason to
    record with it — see 'Icarium.Dispatch.Decide.dTaskTransition'. 'Nothing'
    on a dry run, which decides nothing.
    -}
    }

data FinishArgs = FinishArgs
    { faDecision :: Decision
    , faSha :: Maybe Text
    , faRetention :: Int
    , faLogPath :: Maybe FilePath
    , faBaseSha :: Maybe Text
    , faPayload :: Maybe WorkerPayload
    }

finishWith :: DispatchCtx -> FinishArgs -> IO DispatchResult
finishWith dx FinishArgs{faDecision = decision, faSha = mSha, faRetention = retention, faLogPath = mLogPath, faBaseSha = mBaseSha, faPayload = mPayload} = do
    let conn = dxConn dx
        did = dxDid dx
        branch = dxBranch dx
        wt = dxWorkDir dx
        outcome = dOutcome decision
    -- On failure, snapshot any dirty state onto the dispatch branch before
    -- the worktree is torn down; the branch is what survives. Best-effort:
    -- ignore git errors so we don't mask the original failure note.
    mWipSha <-
        if outcome == OFailure
            then do
                clean <- Git.isClean wt
                if clean
                    then pure Nothing
                    else do
                        r <- Git.commitAll wt ("WIP: dispatch " <> did <> " failed before commit")
                        case r of
                            Left _ -> pure Nothing
                            Right () -> either (const Nothing) Just <$> Git.revParse wt "HEAD"
            else pure Nothing
    -- The wip sha is only known here, and the block reason a failure records
    -- is its note verbatim — so both pick up the same suffix.
    let withWip t = t <> maybe "" ("\nwip_commit: " <>) mWipSha
        enrichedNotes = withWip (renderReason (dReason decision))
    RD.finishDispatch conn did outcome mSha (Just enrichedNotes)
    -- The row is the only place the owning task is recorded; the event
    -- carries it so a watcher need not join back to the DB.
    mDispatch <- RD.getDispatch conn did
    forM_ mDispatch $ \d -> do
        let tid = dispatchTaskId d
        -- Escalation first: it is the worker's report, and the outcome
        -- below is icarium's conclusion about it (ADR 0008).
        forM_ (blockReason mPayload) $ \reason ->
            Ev.emit (dxDbPath dx) "dispatch" (Ev.DispatchEscalated did tid reason)
        Ev.emit (dxDbPath dx) "dispatch" (Ev.DispatchFinished did tid outcome)
    pruneLogFiles conn retention
    pure
        DispatchResult
            { dresDispatchId = Just did
            , dresOutcome = outcome
            , dresBranch = branch
            , dresNotes = enrichedNotes
            , dresLogPath = mLogPath
            , dresBaseSha = mBaseSha
            , dresPayload = mPayload
            , dresTaskTransition = second (fmap withWip) <$> dTaskTransition decision
            }

-- | The worker's escalation reason, when it reported one.
blockReason :: Maybe WorkerPayload -> Maybe Text
blockReason mPayload = do
    p <- mPayload
    case wpStatus p of
        WBlocked -> Just (fromMaybe "no reason given" (wpBlockReason p))
        WSubmitted -> Nothing

pruneLogFiles :: Connection -> Int -> IO ()
pruneLogFiles conn retention = do
    paths <- RD.logPathsOutsideRetention conn retention
    mapM_ deleteIfExists paths
  where
    deleteIfExists p = do
        exists <- doesFileExist p
        when exists (removeFile p)

{- | Perform every tracker mutation the dispatch implies. Intended to be
called from the CLI layer after @dispatch@ returns.

'Icarium.Dispatch.Decide' already chose the target state; this decides only
whether the live row still deserves it. A success lands 'Done' only from a
state that is still in flight — the DB may have moved on since the run
started, and a task a human has since abandoned must not be resurrected. An
interrupted run decides nothing and is left to @icarium dispatch recover@; a
dry run (dispatch id absent) is a no-op.

Then the payload's @for_future_agents@ notes, whatever the outcome: a run
that blocked still learned something, and that is what the next attempt needs.
-}
applyOutcomeToTask :: Connection -> FilePath -> Task -> DispatchResult -> IO ()
applyOutcomeToTask conn db t res
    | Just did <- dresDispatchId res = do
        applyState
        ingestFutureNotes conn db did t (maybe [] wpForFutureAgents (dresPayload res))
    | otherwise = pure ()
  where
    applyState = forM_ (dresTaskTransition res) $ \(target, mReason) -> do
        mFresh <- RT.getTask conn (taskId t)
        forM_ mFresh $ \t' ->
            when (stillEligible target t') $
                transition
                    t'
                    target
                    RT.emptyUpdate
                        { RT.tuState = Just target
                        , RT.tuBlockReason = Just mReason
                        }
    stillEligible Done t' = taskState t' `elem` [InProgress, ReadyHeadless]
    stillEligible _ _ = True
    transition before new upd = do
        void $ RT.updateTask conn (taskId before) upd
        Ev.emit db "dispatch" (Ev.TaskUpdated (taskId before) (taskState before) new)

{- | One ctx entry per note, tagged with the task's retrieval axes and linked
back to it. The payload names no categories: the worker knows no more about
retrieval than triage did, and a wrongly-tagged entry surfaces nowhere useful
where a coarse one at least surfaces on the task's own axis. Axis eligibility
is by construction — 'RC.attachContextCategory' drops what cannot ride on a
context.
-}
ingestFutureNotes :: Connection -> FilePath -> Text -> Task -> [FutureNote] -> IO ()
ingestFutureNotes _ _ _ _ [] = pure ()
ingestFutureNotes conn db did t notes = do
    cats <- RC.taskCategoriesFor conn (taskId t)
    forM_ notes $ \n -> do
        (cid, _) <-
            createContextWithBody
                conn
                db
                RCx.NewContext
                    { RCx.ncTitle = fnTitle n
                    , RCx.ncBody = fnBody n
                    , RCx.ncSourceDispatch = Just did
                    }
        forM_ cats (RC.attachContextCategory conn cid)
        void $ RE.insertEdge conn DerivedFrom ContextNode cid TaskNode (taskId t)
        Ev.emit db "dispatch" (Ev.CtxCreated cid)
