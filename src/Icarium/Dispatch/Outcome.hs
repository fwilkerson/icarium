module Icarium.Dispatch.Outcome (
    DispatchCtx (..),
    DispatchResult (..),
    FinishArgs (..),
    applyOutcomeToTask,
    finishWith,
    pruneLogFiles,
) where

import Control.Monad (forM_, void, when)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import System.Directory (doesFileExist, removeFile)

import Icarium.Dispatch.Payload (FutureNote (..), WorkerPayload (..))
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
    }

data FinishArgs = FinishArgs
    { faOutcome :: DispatchOutcome
    , faSha :: Maybe Text
    , faNotes :: Text
    , faRetention :: Int
    , faLogPath :: Maybe FilePath
    , faBaseSha :: Maybe Text
    , faPayload :: Maybe WorkerPayload
    }

finishWith :: DispatchCtx -> FinishArgs -> IO DispatchResult
finishWith dx FinishArgs{faOutcome = outcome, faSha = mSha, faNotes = notes, faRetention = retention, faLogPath = mLogPath, faBaseSha = mBaseSha, faPayload = mPayload} = do
    let conn = dxConn dx
        did = dxDid dx
        branch = dxBranch dx
        wt = dxWorkDir dx
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
    let enrichedNotes = case mWipSha of
            Nothing -> notes
            Just sha -> notes <> "\nwip_commit: " <> sha
    RD.finishDispatch conn did outcome mSha (Just enrichedNotes)
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
            }

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

State:

* success and task still in flight -> mark 'done' (the agent no longer
  self-updates; we don't want to re-pick it).
* failure -> mark 'blocked' with the dispatch notes as reason. A worker that
  reported @blocked@ arrives here as a failure, carrying its own
  @block_reason@ as those notes — the gate folds the block into the outcome
  (see 'Icarium.Dispatch.PostClaude') so that everything keyed off outcome,
  auto-merge included, agrees with the task row.
* interrupted -> leave to @icarium dispatch recover@.
* dry-run (dispatch id absent) -> no-op.

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
    applyState = case dresOutcome res of
        OSuccess -> do
            mFresh <- RT.getTask conn (taskId t)
            case mFresh of
                Just t'
                    | taskState t' `elem` [InProgress, ReadyHeadless] ->
                        void $
                            RT.updateTask
                                conn
                                (taskId t')
                                RT.emptyUpdate
                                    { RT.tuState = Just Done
                                    }
                _ -> pure ()
        OFailure ->
            void $
                RT.updateTask
                    conn
                    (taskId t)
                    RT.emptyUpdate
                        { RT.tuState = Just Blocked
                        , RT.tuBlockReason = Just (Just (dresNotes res))
                        }
        OInterrupted -> pure ()

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
