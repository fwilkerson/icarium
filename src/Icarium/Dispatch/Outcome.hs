module Icarium.Dispatch.Outcome (
    DispatchCtx (..),
    DispatchResult (..),
    FinishArgs (..),
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

data FinishArgs = FinishArgs
    { faOutcome :: DispatchOutcome
    , faSha :: Maybe Text
    , faNotes :: Text
    , faRetention :: Int
    , faLogPath :: Maybe FilePath
    , faBaseSha :: Maybe Text
    }

finishWith :: DispatchCtx -> FinishArgs -> IO DispatchResult
finishWith dx FinishArgs{faOutcome = outcome, faSha = mSha, faNotes = notes, faRetention = retention, faLogPath = mLogPath, faBaseSha = mBaseSha} = do
    let conn = dxConn dx
        did = dxDid dx
        branch = dxBranch dx
        base = dxBase dx
    -- On failure, snapshot any dirty worktree/index onto the dispatch branch
    -- before returning to base. Without this, `git checkout base` carries
    -- staged or unstaged changes onto main when the dispatch branch SHA equals
    -- base SHA (i.e. the agent never committed). Best-effort: ignore git errors
    -- so we don't mask the original failure note.
    mWipSha <-
        if outcome == OFailure
            then do
                clean <- Git.isClean
                if clean
                    then pure Nothing
                    else do
                        r <- Git.commitAll ("WIP: dispatch " <> did <> " failed before commit")
                        case r of
                            Left _ -> pure Nothing
                            Right () -> either (const Nothing) Just <$> Git.revParse "HEAD"
            else pure Nothing
    when (outcome == OFailure) $ void (Git.checkout base)
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
