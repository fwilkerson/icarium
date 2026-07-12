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
    worktreeAdd,
    worktreeAddExisting,
    worktreeRemove,
    worktreePrune,
    rebase,
    rebaseAbort,
    mergeBaseIsAncestor,
    branchCheckedOutAt,
    parseWorktreeList,
) where

import Control.Monad (void)
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe, listToMaybe)
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

-- =============================================================
-- Worktrees
-- =============================================================

-- | Add a worktree at @path@ on a new branch cut from @base@.
worktreeAdd :: FilePath -> FilePath -> Text -> Text -> IO (Either GitError ())
worktreeAdd repo path branch base =
    void
        <$> runGit repo ["worktree", "add", path, "-b", T.unpack branch, T.unpack base]

-- | Add a worktree at @path@ checking out an existing branch.
worktreeAddExisting :: FilePath -> FilePath -> Text -> IO (Either GitError ())
worktreeAddExisting repo path branch =
    void
        <$> runGit repo ["worktree", "add", path, T.unpack branch]

-- | Remove the worktree at @path@. @force@ discards dirty state.
worktreeRemove :: FilePath -> FilePath -> Bool -> IO (Either GitError ())
worktreeRemove repo path force =
    void
        <$> runGit repo (["worktree", "remove"] <> ["--force" | force] <> [path])

-- | Prune stale worktree admin entries. Best-effort.
worktreePrune :: FilePath -> IO ()
worktreePrune repo = void (runGit repo ["worktree", "prune"])

-- =============================================================
-- Rebase / ancestry
-- =============================================================

-- | Rebase the current branch onto @onto@.
rebase :: FilePath -> Text -> IO (Either GitError ())
rebase dir onto = void <$> runGit dir ["rebase", T.unpack onto]

-- | Abort an in-progress rebase. Best-effort.
rebaseAbort :: FilePath -> IO ()
rebaseAbort dir = void (runGit dir ["rebase", "--abort"])

{- | True when @a@ is an ancestor of @b@ (i.e. @b@ can fast-forward from
@a@). Exit 0 = ancestor; any other exit (not-ancestor, bad ref) = False.
-}
mergeBaseIsAncestor :: FilePath -> Text -> Text -> IO Bool
mergeBaseIsAncestor dir a b = do
    r <- runGit dir ["merge-base", "--is-ancestor", T.unpack a, T.unpack b]
    pure $ case r of
        Right _ -> True
        Left _ -> False

{- | The worktree path where @branch@ is checked out, if any. Drives
merge-case selection (land in place vs. temp worktree).
-}
branchCheckedOutAt :: FilePath -> Text -> IO (Maybe FilePath)
branchCheckedOutAt repo branch = do
    r <- runGit repo ["worktree", "list", "--porcelain"]
    pure $ case r of
        Left _ -> Nothing
        Right out ->
            listToMaybe
                [ path
                | (path, Just ref) <- parseWorktreeList out
                , stripRefsHeads ref == branch
                ]
  where
    stripRefsHeads ref = fromMaybe ref (T.stripPrefix "refs/heads/" ref)

{- | Parse @git worktree list --porcelain@ into @(path, branch)@ pairs.
Stanzas are separated by blank lines; each has a @worktree \<path\>@ line
and optionally a @branch refs/heads/\<name\>@ line. Detached-HEAD and bare
stanzas have no branch line, so their branch is 'Nothing'. The branch
value is the raw ref (e.g. @refs/heads/main@).
-}
parseWorktreeList :: Text -> [(FilePath, Maybe Text)]
parseWorktreeList = concatMap stanza . splitStanzas . T.lines
  where
    splitStanzas ls =
        case break blank (dropWhile blank ls) of
            ([], _) -> []
            (s, rest) -> s : splitStanzas rest
    blank = T.null . T.strip
    stanza sLines =
        [ (T.unpack path, field "branch ")
        | path <- take 1 (values "worktree ")
        ]
      where
        field key = listToMaybe (values key)
        values key = [v | l <- sLines, Just v <- [T.stripPrefix key l]]
