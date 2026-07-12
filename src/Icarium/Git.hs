module Icarium.Git (
    GitError (..),
    runGit,
    isClean,
    statusPorcelain,
    currentBranch,
    revParse,
    createBranch,
    checkout,
    ffMerge,
    stashUntracked,
    deleteBranch,
    changedFiles,
    diffPatch,
    commitAll,
) where

import Control.Monad (void)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import System.Exit (ExitCode (..))
import System.Process.Typed (proc, readProcess)

-- | A failed git invocation.
data GitError = GitError
    { gitCmd :: [String]
    , gitStderr :: Text
    , gitExit :: Int
    }
    deriving (Show)

{- | Run @git -C \<dir\>@ with the given args. Returns stdout (stripped)
on success, a structured error otherwise. Every operation is explicit
about the repo/worktree it acts on; nothing depends on the process cwd.
-}
runGit :: FilePath -> [String] -> IO (Either GitError Text)
runGit dir args = do
    let allArgs = "-C" : dir : args
    (code, out, err) <- readProcess (proc "git" allArgs)
    let outT = T.strip (decodeUtf8 (BL.toStrict out))
        errT = T.strip (decodeUtf8 (BL.toStrict err))
    pure $ case code of
        ExitSuccess -> Right outT
        ExitFailure c ->
            Left
                GitError
                    { gitCmd = "git" : allArgs
                    , gitStderr = errT
                    , gitExit = c
                    }

isClean :: FilePath -> IO Bool
isClean dir = T.null . T.strip <$> statusPorcelain dir

{- | Raw `git status --porcelain` output (already stripped). Empty means
the working tree is clean. On git failure returns a non-empty sentinel
so callers conservatively treat the tree as dirty.
-}
statusPorcelain :: FilePath -> IO Text
statusPorcelain dir = do
    r <- runGit dir ["status", "--porcelain"]
    pure $ case r of
        Right out -> out
        Left _ -> "?? <git status failed>"

currentBranch :: FilePath -> IO (Either GitError Text)
currentBranch dir = runGit dir ["rev-parse", "--abbrev-ref", "HEAD"]

-- | Resolve a ref to its full SHA.
revParse :: FilePath -> Text -> IO (Either GitError Text)
revParse dir ref = runGit dir ["rev-parse", "--verify", T.unpack ref]

-- | Create and check out a new branch from the given base.
createBranch :: FilePath -> Text -> Text -> IO (Either GitError ())
createBranch dir name base =
    void
        <$> runGit dir ["checkout", "-b", T.unpack name, T.unpack base]

checkout :: FilePath -> Text -> IO (Either GitError ())
checkout dir branch = void <$> runGit dir ["checkout", T.unpack branch]

{- | Fast-forward the current branch to the named branch. Fails if
the FF isn't possible — we never want a merge commit.
-}
ffMerge :: FilePath -> Text -> IO (Either GitError ())
ffMerge dir branch =
    void
        <$> runGit dir ["merge", "--ff-only", T.unpack branch]

{- | Stash working-tree changes including untracked files with a
deterministic message. Used by recovery to preserve in-flight work.
-}
stashUntracked :: FilePath -> Text -> IO (Either GitError ())
stashUntracked dir msg =
    void
        <$> runGit dir ["stash", "push", "-u", "-m", T.unpack msg]

{- | Delete a fully-merged local branch. Uses -d (safe delete) so git
will refuse if the branch has unmerged commits.
-}
deleteBranch :: FilePath -> Text -> IO (Either GitError ())
deleteBranch dir name =
    void
        <$> runGit dir ["branch", "-d", T.unpack name]

-- | Stage all changes and create a commit with the given message.
commitAll :: FilePath -> Text -> IO (Either GitError ())
commitAll dir msg = do
    r <- runGit dir ["add", "-A"]
    case r of
        Left e -> pure (Left e)
        Right _ -> void <$> runGit dir ["commit", "-m", T.unpack msg]

-- | Files changed between the given base SHA and HEAD; returns [] on git error.
changedFiles :: FilePath -> Text -> IO [Text]
changedFiles dir baseSha = do
    r <- runGit dir ["diff", "--name-only", T.unpack baseSha <> "..HEAD"]
    case r of
        Left _ -> pure []
        Right out -> pure (filter (not . T.null) (T.lines out))

-- | Full patch between the given base SHA and HEAD; returns empty on git error.
diffPatch :: FilePath -> Text -> IO Text
diffPatch dir baseSha = do
    r <- runGit dir ["diff", T.unpack baseSha <> "..HEAD"]
    pure $ case r of
        Left _ -> ""
        Right out -> out
