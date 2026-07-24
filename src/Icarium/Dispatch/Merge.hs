module Icarium.Dispatch.Merge (
    MergeOutcome (..),
    mergeParked,
) where

import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Either (fromRight)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection)
import System.IO (hPutStrLn, stderr)

import Icarium.Config (Config, cfgCommands, cfgDispatch)
import Icarium.Dispatch.Gate (GateEnv (..), gateBudgetUsecs, runGates)
import Icarium.Dispatch.Worktree (
    WorktreeError (..),
    mergeWorktreePath,
    rebuildWorktree,
    removeQuietly,
    teardownWorktree,
    worktreeErrorText,
 )
import Icarium.Git qualified as Git
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Types

{- | Result of one parked-dispatch merge attempt. 'MergeBlocked' carries
the exit code the single-dispatch CLI path reports (1 = git/user error,
2 = config, 3 = rebase/gate failure); a bulk run ignores it and moves on.
-}
data MergeOutcome
    = -- | Landed; the new base tip sha.
      MergeLanded Text
    | -- | Stays parked; exit code + reason.
      MergeBlocked Int Text
    | -- | Back-pressure (worktree_setup exit 75): stop a bulk run cleanly.
      MergeStopped Text

{- | Land one parked dispatch on its base: fast-forward when the base
hasn't moved since the park; otherwise rebase the branch in a rebuilt
worktree, re-run the gates against the post-rebase state, then
fast-forward. Any blocked outcome leaves the dispatch parked and persists
its reason to the dispatch notes. On success the dispatch is stamped
merged and its branch deleted. Preconditions (outcome success, not
already merged) are the caller's job.
-}
mergeParked :: Config -> Connection -> Dispatch -> IO MergeOutcome
mergeParked cfg conn d = do
    out <- fmap (either id MergeLanded) . runExceptT $ attempt
    case out of
        MergeBlocked _ note ->
            RD.updateDispatch conn (dispatchId d) RD.emptyUpdate{RD.duNotes = Just note}
        _ -> pure ()
    pure out
  where
    attempt = do
        let did = dispatchId d
            branch = dispatchBranch d
            base = dispatchBaseBranch d
        liftIO (Git.revParse "." branch) >>= \case
            Left _ -> throwE (MergeBlocked 1 ("branch missing (deleted manually?): " <> branch))
            Right _ -> pure ()
        ffPossible <- liftIO (Git.mergeBaseIsAncestor "." base branch)
        unless ffPossible (rebaseThenGate cfg d)
        landFF did base branch
        newSha <-
            liftIO (Git.revParse "." base)
                >>= either (throwE . MergeBlocked 1 . T.pack . show) pure
        liftIO (RD.setMerged conn did newSha)
        -- `branch -d` checks merged-ness against HEAD, which may be an
        -- unrelated checkout; the sha equality proves the branch landed.
        branchSha <- liftIO (fromRight "" <$> Git.revParse "." branch)
        when (branchSha == newSha) $
            liftIO (void (Git.deleteBranchForce "." branch))
        pure newSha

{- | Rebase the parked branch onto the current base tip and re-run the
gates there. Note the rebase rewrites the parked branch even when the
gates then fail — the pre-rebase commits stay reachable via the reflog,
and retrying the merge after a fix is a plain FF.
-}
rebaseThenGate :: Config -> Dispatch -> ExceptT MergeOutcome IO ()
rebaseThenGate cfg d = do
    let dcfg = cfgDispatch cfg
        did = dispatchId d
        branch = dispatchBranch d
        base = dispatchBaseBranch d
    cc <-
        maybe (throwE (MergeBlocked 2 "no [commands] section configured")) pure (cfgCommands cfg)
    liftIO $ hPutStrLn stderr ("icarium: base moved since park; rebasing " <> T.unpack branch)
    wt <-
        liftIO (rebuildWorktree "." dcfg did branch) >>= \case
            Left err@(WtNoCapacity _) -> throwE (MergeStopped (worktreeErrorText err))
            Left err -> throwE (MergeBlocked 3 (worktreeErrorText err))
            Right wt -> pure wt
    liftIO (Git.rebase wt base) >>= \case
        Left _ -> do
            liftIO $ do
                Git.rebaseAbort wt
                teardownWorktree "." dcfg wt
            throwE (MergeBlocked 3 ("merge conflict; needs manual rebase onto " <> base))
        Right () -> do
            -- The row ended when it parked, so there is no heartbeat to keep
            -- warm — but a gate that wedges here still leaves its forensics
            -- in the dispatch's own log.
            let gates =
                    GateEnv
                        { geDir = wt
                        , geBudgetUsecs = gateBudgetUsecs dcfg
                        , geLogPath = T.unpack <$> dispatchLogPath d
                        , geHeartbeat = Nothing
                        }
            gateResult <- liftIO (runGates gates (Just cc))
            liftIO (teardownWorktree "." dcfg wt)
            case gateResult of
                Left note -> throwE (MergeBlocked 3 ("gates failed after rebase: " <> note))
                Right () -> pure ()

-- | Fast-forward base to the dispatch branch, wherever base lives.
landFF :: Text -> Text -> Text -> ExceptT MergeOutcome IO ()
landFF did base branch = do
    mWhere <- liftIO (Git.branchCheckedOutAt "." base)
    here <- liftIO (either (const Nothing) (Just . T.unpack) <$> Git.topLevel ".")
    case mWhere of
        -- Base checked out nowhere: FF its ref via a throwaway worktree.
        Nothing -> do
            let path = mergeWorktreePath did
            liftIO (removeQuietly "." path)
            liftIO (Git.worktreeAddExisting "." path base)
                >>= either (blocked . ("cannot create merge worktree: " <>) . T.pack . show) pure
            r <- liftIO (Git.ffMerge path branch)
            liftIO (removeQuietly "." path)
            either (blocked . ("ff-merge failed: " <>) . T.pack . show) pure r
        Just wtPath
            | Just wtPath == here -> do
                clean <- liftIO (Git.isClean ".")
                unless clean $
                    blocked "base is checked out here with a dirty tree; commit or stash first"
                liftIO (Git.ffMerge "." branch)
                    >>= either (blocked . ("ff-merge failed: " <>) . T.pack . show) pure
            | otherwise ->
                blocked $
                    "base branch "
                        <> base
                        <> " is checked out at "
                        <> T.pack wtPath
                        <> "; merge there or free it first"
  where
    blocked :: Text -> ExceptT MergeOutcome IO a
    blocked = throwE . MergeBlocked 1
