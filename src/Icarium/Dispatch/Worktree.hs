module Icarium.Dispatch.Worktree (
    WorktreeError (..),
    worktreeErrorText,
    worktreePath,
    mergeWorktreePath,
    setupExitError,
    createDispatchWorktree,
    rebuildWorktree,
    teardownWorktree,
    removeQuietly,
) where

import Control.Monad (void, when)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (copyFile, doesDirectoryExist, doesFileExist, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process.Typed (runProcess, setWorkingDir, shell)

import Icarium.Config (DispatchConfig (..), defaultConfigPath)
import Icarium.Git qualified as Git

data WorktreeError
    = -- | worktree_setup exited 75 (EX_TEMPFAIL): no capacity, try later.
      WtNoCapacity Text
    | WtSetupFailed Text
    | WtGitFailed Text
    deriving (Show, Eq)

worktreeErrorText :: WorktreeError -> Text
worktreeErrorText = \case
    WtNoCapacity t -> "no worktree capacity: " <> t
    WtSetupFailed t -> "worktree setup failed: " <> t
    WtGitFailed t -> "worktree git operation failed: " <> t

-- | Where a dispatch's worktree lives, relative to the invoking checkout.
worktreePath :: Text -> FilePath
worktreePath did = ".icarium" </> "wt" </> T.unpack did

-- | Where the throwaway worktree for landing a merge lives.
mergeWorktreePath :: Text -> FilePath
mergeWorktreePath did = ".icarium" </> "wt" </> ("merge-" <> T.unpack did)

-- | Classify a worktree_setup exit code. 75 is the back-pressure contract.
setupExitError :: Text -> Int -> WorktreeError
setupExitError cmd 75 = WtNoCapacity (cmd <> " -> exit 75")
setupExitError cmd c = WtSetupFailed (cmd <> " -> exit " <> T.pack (show c))

{- | Cut a fresh worktree for a dispatch: new branch from @base@ at
@worktreePath did@, the invoking checkout's icarium.toml copied in
(it is gitignored by design), then @worktree_setup@ run inside it.
On any failure the worktree is removed; the branch from a failed
setup is deleted too (it has no commits).
-}
createDispatchWorktree ::
    FilePath -> DispatchConfig -> Text -> Text -> Text -> IO (Either WorktreeError FilePath)
createDispatchWorktree repo dcfg did branch base = do
    let wt = worktreePath did
    r <- Git.worktreeAdd repo wt branch base
    case r of
        Left e -> pure (Left (WtGitFailed (T.pack (show e))))
        Right () ->
            provision repo dcfg wt `orCleanup` do
                removeQuietly repo wt
                void (Git.deleteBranch repo branch)

{- | Re-create a worktree for an existing dispatch branch (merge-rebase,
inspection). Clears any stale directory left by an earlier aborted run
first. The branch is retained on failure — it holds real work.
-}
rebuildWorktree ::
    FilePath -> DispatchConfig -> Text -> Text -> IO (Either WorktreeError FilePath)
rebuildWorktree repo dcfg did branch = do
    let wt = worktreePath did
    stale <- doesDirectoryExist wt
    when stale (removeQuietly repo wt)
    r <- Git.worktreeAddExisting repo wt branch
    case r of
        Left e -> pure (Left (WtGitFailed (T.pack (show e))))
        Right () -> provision repo dcfg wt `orCleanup` removeQuietly repo wt

-- | Copy config in and run worktree_setup; classify its exit code.
provision :: FilePath -> DispatchConfig -> FilePath -> IO (Either WorktreeError FilePath)
provision repo dcfg wt = do
    hasToml <- doesFileExist (repo </> defaultConfigPath)
    when hasToml (copyFile (repo </> defaultConfigPath) (wt </> defaultConfigPath))
    case dcWorktreeSetup dcfg of
        Nothing -> pure (Right wt)
        Just cmd -> do
            code <- runProcess (setWorkingDir wt (shell (T.unpack cmd)))
            pure $ case code of
                ExitSuccess -> Right wt
                ExitFailure c -> Left (setupExitError cmd c)

orCleanup :: IO (Either e a) -> IO () -> IO (Either e a)
orCleanup act cleanup = do
    r <- act
    case r of
        Left _ -> cleanup >> pure r
        Right _ -> pure r

{- | Run @worktree_teardown@ (best-effort), then force-remove the
worktree and prune. Dirty state must already be checkpointed by the
caller — removal is unconditional. Safe to call on a missing worktree.
-}
teardownWorktree :: FilePath -> DispatchConfig -> FilePath -> IO ()
teardownWorktree repo dcfg wt = do
    exists <- doesDirectoryExist wt
    when exists $
        case dcWorktreeTeardown dcfg of
            Nothing -> pure ()
            Just cmd -> do
                code <- runProcess (setWorkingDir wt (shell (T.unpack cmd)))
                case code of
                    ExitSuccess -> pure ()
                    ExitFailure c ->
                        hPutStrLn stderr $
                            "icarium: worktree_teardown exited " <> show c <> " (continuing)"
    removeQuietly repo wt

-- | Force-remove a worktree, clear any leftover directory, and prune.
removeQuietly :: FilePath -> FilePath -> IO ()
removeQuietly repo wt = do
    void (Git.worktreeRemove repo wt True)
    -- worktree remove can refuse (e.g. locked); make sure the path is gone
    -- so a later rebuild never collides, then let git forget the entry.
    leftover <- doesDirectoryExist wt
    when leftover (removePathForcibly wt)
    Git.worktreePrune repo
