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

{- | Run @git@ with the given args. Returns stdout (stripped) on
success, a structured error otherwise.
-}
runGit :: [String] -> IO (Either GitError Text)
runGit args = do
    (code, out, err) <- readProcess (proc "git" args)
    let outT = T.strip (decodeUtf8 (BL.toStrict out))
        errT = T.strip (decodeUtf8 (BL.toStrict err))
    pure $ case code of
        ExitSuccess -> Right outT
        ExitFailure c ->
            Left
                GitError
                    { gitCmd = "git" : args
                    , gitStderr = errT
                    , gitExit = c
                    }

isClean :: IO Bool
isClean = T.null . T.strip <$> statusPorcelain

{- | Raw `git status --porcelain` output (already stripped). Empty means
the working tree is clean. On git failure returns a non-empty sentinel
so callers conservatively treat the tree as dirty.
-}
statusPorcelain :: IO Text
statusPorcelain = do
    r <- runGit ["status", "--porcelain"]
    pure $ case r of
        Right out -> out
        Left _ -> "?? <git status failed>"

currentBranch :: IO (Either GitError Text)
currentBranch = runGit ["rev-parse", "--abbrev-ref", "HEAD"]

-- | Resolve a ref to its full SHA.
revParse :: Text -> IO (Either GitError Text)
revParse ref = runGit ["rev-parse", "--verify", T.unpack ref]

-- | Create and check out a new branch from the given base.
createBranch :: Text -> Text -> IO (Either GitError ())
createBranch name base =
    void
        <$> runGit
            ["checkout", "-b", T.unpack name, T.unpack base]

checkout :: Text -> IO (Either GitError ())
checkout branch = void <$> runGit ["checkout", T.unpack branch]

{- | Fast-forward the current branch to the named branch. Fails if
the FF isn't possible — we never want a merge commit.
-}
ffMerge :: Text -> IO (Either GitError ())
ffMerge branch =
    void
        <$> runGit
            ["merge", "--ff-only", T.unpack branch]

{- | Stash working-tree changes including untracked files with a
deterministic message. Used by recovery to preserve in-flight work.
-}
stashUntracked :: Text -> IO (Either GitError ())
stashUntracked msg =
    void
        <$> runGit
            ["stash", "push", "-u", "-m", T.unpack msg]

{- | Delete a fully-merged local branch. Uses -d (safe delete) so git
will refuse if the branch has unmerged commits.
-}
deleteBranch :: Text -> IO (Either GitError ())
deleteBranch name =
    void
        <$> runGit
            ["branch", "-d", T.unpack name]

-- | Stage all changes and create a commit with the given message.
commitAll :: Text -> IO (Either GitError ())
commitAll msg = do
    r <- runGit ["add", "-A"]
    case r of
        Left e -> pure (Left e)
        Right _ -> void <$> runGit ["commit", "-m", T.unpack msg]

-- | Files changed between the given base SHA and HEAD; returns [] on git error.
changedFiles :: Text -> IO [Text]
changedFiles baseSha = do
    r <- runGit ["diff", "--name-only", T.unpack baseSha <> "..HEAD"]
    case r of
        Left _ -> pure []
        Right out -> pure (filter (not . T.null) (T.lines out))

-- | Full patch between the given base SHA and HEAD; returns empty on git error.
diffPatch :: Text -> IO Text
diffPatch baseSha = do
    r <- runGit ["diff", T.unpack baseSha <> "..HEAD"]
    pure $ case r of
        Left _ -> ""
        Right out -> out
