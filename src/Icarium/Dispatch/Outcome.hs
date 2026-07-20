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
