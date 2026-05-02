module Icarium.Dispatch.Outcome (
    DispatchCtx (..),
    DispatchResult (..),
    applyOutcomeToTask,
    finishWith,
    pruneLogFiles,
) where

import Control.Monad (void, when)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import System.Directory (doesFileExist, removeFile)

import Icarium.Git qualified as Git
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Task qualified as RT
import Icarium.Types

data DispatchCtx = DispatchCtx
    { dxConn :: Connection
    , dxDid :: Text
    , dxBranch :: Text
    , dxBase :: Text
    }

data DispatchResult = DispatchResult
    { dresDispatchId :: Maybe Text
    , dresOutcome :: DispatchOutcome
    , dresBranch :: Text
    , dresNotes :: Text
    , dresLogPath :: Maybe FilePath
    , dresBaseSha :: Maybe Text
    }

finishWith ::
    DispatchCtx ->
    DispatchOutcome ->
    Maybe Text ->
    Text ->
    Int ->
    Maybe FilePath ->
    Maybe Text ->
    IO DispatchResult
finishWith dx outcome mSha notes retention mLogPath mBaseSha = do
    let conn = dxConn dx
        did = dxDid dx
        branch = dxBranch dx
        base = dxBase dx
    -- Best-effort: on failure, return to the base branch so the next
    -- dispatch (e.g. drain mode) doesn't fail its on-base-branch
    -- precondition. Ignore errors here so we don't mask the original
    -- failure note.
    when (outcome == OFailure) $ void (Git.checkout base)
    RD.finishDispatch conn did outcome mSha (Just notes)
    pruneLogFiles conn retention
    pure
        DispatchResult
            { dresDispatchId = Just did
            , dresOutcome = outcome
            , dresBranch = branch
            , dresNotes = notes
            , dresLogPath = mLogPath
            , dresBaseSha = mBaseSha
            }

pruneLogFiles :: Connection -> Int -> IO ()
pruneLogFiles conn retention = do
    paths <- RD.logPathsOutsideRetention conn retention
    mapM_ deleteIfExists paths
  where
    deleteIfExists p = do
        exists <- doesFileExist p
        when exists (removeFile p)

{- | Reconcile task state with the dispatch outcome. Intended to be
called from the CLI layer after @dispatch@ returns.

* success and task still 'ready' -> mark 'done' (the agent
  presumably didn't self-update; we don't want to re-pick it).
* failure -> mark 'blocked' with the dispatch notes as reason.
* interrupted -> leave to @icarium dispatch recover@.
* dry-run (dispatch id absent) -> no-op.
-}
applyOutcomeToTask :: Connection -> Task -> DispatchResult -> IO ()
applyOutcomeToTask conn t res
    | Nothing <- dresDispatchId res = pure ()
    | otherwise = case dresOutcome res of
        OSuccess -> do
            mFresh <- RT.getTask conn (taskId t)
            case mFresh of
                Just t'
                    | taskState t' `elem` [InProgress, Ready] ->
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
