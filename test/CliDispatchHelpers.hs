{- | Scaffolding for the specs that drive a real dispatch: a throwaway git
repo, an icarium.toml pointed at the committed stub @claude@, and the small
git/dispatch observations those tests assert on.

These drive ./bin/icarium as a subprocess against the throwaway repo, with
the committed test/fixtures/claude stub resolved via PATH. The stub's
behavior is selected by STUB_CLAUDE_MODE (see the script). Each test gets
its own repo + DB, so they stay isolated under parallel execution.
-}
module CliDispatchHelpers (
    runDispatch,
    withDispatchRepo,
    runDispatchParked,
    stubToml,
    stubTomlWith,
    agreementToml,
    addReadyTask,
    gitOut,
    countLines,
    worktreeCount,
    dispatchBranches,
    parkedId,
    badgedId,
) where

import Control.Exception (evaluate)
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isInfixOf)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, makeAbsolute, removeFile)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed (proc, readProcess, setEnv, setWorkingDir)

import CliHelpers (absBin)
import TestHelpers (withTestRepo)

-- | The committed stub-@claude@ fixtures dir, resolved once at load time.
{-# NOINLINE absFixtures #-}
absFixtures :: FilePath
absFixtures = unsafePerformIO (makeAbsolute "test/fixtures")

-- | Directory holding ./bin/icarium, so a child @icarium@ resolves on PATH.
binDir :: FilePath
binDir = takeDirectory absBin

{- | Run ./bin/icarium inside a test repo with the stub @claude@ (and the
icarium binary itself) prepended to PATH. @STUB_CLAUDE_MODE@ picks the stub's
behavior. typed-process's 'setEnv' replaces the whole environment, so we
build from the parent's, override PATH, and add the mode. --db is absolute
and cwd is the repo, so the binary's @git -C .@ calls resolve there.
-}
runDispatch :: FilePath -> FilePath -> Maybe String -> [String] -> IO (ExitCode, String, String)
runDispatch repo db mMode args = do
    absDb <- makeAbsolute db
    parentEnv <- getEnvironment
    let path0 = fromMaybe "" (lookup "PATH" parentEnv)
        -- ICARIUM_* must not leak in: when this suite itself runs inside a
        -- dispatch worker, the inherited ICARIUM_TASK_ID poisons the nested
        -- dispatch's icarium calls and the review tests fail with exit 3.
        base = filter ((`notElem` ["PATH", "STUB_CLAUDE_MODE", "ICARIUM_TASK_ID", "ICARIUM_DB"]) . fst) parentEnv
        env =
            ("PATH", absFixtures <> ":" <> binDir <> ":" <> path0)
                : maybe id (\m -> (("STUB_CLAUDE_MODE", m) :)) mMode base
    (code, out, err) <-
        readProcess (setEnv env (setWorkingDir repo (proc absBin (["--db", absDb] <> args))))
    pure (code, BLC.unpack out, BLC.unpack err)

{- | A git repo with a committed .gitignore (so a worktree's .icarium/ is
ignored), a stub-friendly icarium.toml, an empty .icarium/, and one commit
on main. Yields the repo dir and the DB path (created on first command).
-}
withDispatchRepo :: (FilePath -> FilePath -> IO a) -> IO a
withDispatchRepo k =
    withTestRepo $ \dir -> do
        writeFile (dir </> ".gitignore") ".icarium/\nicarium.toml\n"
        _ <- readProcess (setWorkingDir dir (proc "git" ["add", ".gitignore"]))
        _ <- readProcess (setWorkingDir dir (proc "git" ["commit", "-m", "gitignore"]))
        writeFile (dir </> "icarium.toml") stubToml
        createDirectoryIfMissing True (dir </> ".icarium")
        k dir (dir </> ".icarium" </> "icarium.db")

-- | Default stub config: trivial gates, no worktree hooks.
stubToml :: String
stubToml = stubTomlWith "true" Nothing Nothing

{- | icarium.toml driving the stub model, with an overridable test gate and
optional worktree_setup / worktree_teardown commands.
-}
stubTomlWith :: String -> Maybe String -> Maybe String -> String
stubTomlWith testCmd mSetup mTeardown =
    unlines $
        [ "[project]"
        , "integration_branch = \"main\""
        , "[commands]"
        , "build = \"true\""
        , "test  = " <> show testCmd
        , "[dispatch]"
        , "model  = \"stub\""
        , "effort = \"low\""
        , "tools = [\"Bash\"]"
        , "allowed_tools = [\"Bash\"]"
        , "scratch_dir = \".icarium/scratch\""
        , "max_minutes_per_dispatch = 2"
        , "heartbeat_stale_seconds  = 300"
        , "log_retention_runs       = 5"
        ]
            <> maybe [] (\c -> ["worktree_setup = " <> show c]) mSetup
            <> maybe [] (\c -> ["worktree_teardown = " <> show c]) mTeardown
            <> [ "[categories]"
               , "domains     = [\"core\"]"
               , "disciplines = [\"development\"]"
               ]

{- | stubToml with a [dispatch] agreement_path entry (tomland has no
append-into-table story, so the whole file is spelled out; same
precedent as testDispatchDryRunMcpConfig).
-}
agreementToml :: String -> String
agreementToml path =
    unlines
        [ "[project]"
        , "integration_branch = \"main\""
        , "[commands]"
        , "build = \"true\""
        , "test  = \"true\""
        , "[dispatch]"
        , "model  = \"stub\""
        , "effort = \"low\""
        , "tools = [\"Bash\"]"
        , "allowed_tools = [\"Bash\"]"
        , "scratch_dir = \".icarium/scratch\""
        , "max_minutes_per_dispatch = 2"
        , "heartbeat_stale_seconds  = 300"
        , "log_retention_runs       = 5"
        , "agreement_path = " <> show path
        , "[categories]"
        , "domains     = [\"core\"]"
        , "disciplines = [\"development\"]"
        ]

{- | Count lines in a file, forcing the read fully (a plain lazy
@readFile@ leaves the handle open, so a concurrent appender can leak into
a later forced read). Used to observe gate/teardown side effects.
-}
countLines :: FilePath -> IO Int
countLines p = readFile p >>= evaluate . length . lines

-- | Run git in a repo dir; return stdout stripped of trailing whitespace.
gitOut :: FilePath -> [String] -> IO String
gitOut dir args = do
    (_, out, _) <- readProcess (setWorkingDir dir (proc "git" args))
    pure (dropWhileEnd isSpace (BLC.unpack out))

-- | Number of worktrees registered in the repo (1 = just the main checkout).
worktreeCount :: FilePath -> IO Int
worktreeCount dir = length . filter (not . null) . lines <$> gitOut dir ["worktree", "list"]

-- | Full names of any dispatch/* branches.
dispatchBranches :: FilePath -> IO [String]
dispatchBranches dir =
    filter (not . null) . lines
        <$> gitOut dir ["branch", "--list", "dispatch/*", "--format=%(refname:short)"]

-- | Add a ready task, returning its full id.
addReadyTask :: FilePath -> FilePath -> String -> IO String
addReadyTask dir db title = do
    (_, out, _) <- runDispatch dir db Nothing ["task", "add", title, "--state", "ready-headless"]
    pure (head (words out))

-- | First token of the first non-empty line (a 10-char id prefix from a list).
firstListId :: String -> String
firstListId out = case filter (not . null) (lines out) of
    (l : _) -> head (words l)
    [] -> ""

-- | The parked dispatch id prefix, or "" if none parked.
parkedId :: FilePath -> FilePath -> IO String
parkedId dir db = firstListId . snd3 <$> runDispatch dir db Nothing ["dispatch", "list", "--parked"]
  where
    snd3 (_, b, _) = b

-- | Id prefix of the first `dispatch list` row carrying the given badge.
badgedId :: FilePath -> FilePath -> String -> IO String
badgedId dir db badge = do
    (_, out, _) <- runDispatch dir db Nothing ["dispatch", "list"]
    pure $ case filter (badge `isInfixOf`) (lines out) of
        (l : _) -> head (words l)
        [] -> ""

{- | Dispatch with a throwaway dirty file in the base checkout, so the
auto-merge blocks and the dispatch parks (the run exits 3 by design).
Restores the checkout before returning the run's result.
-}
runDispatchParked :: FilePath -> FilePath -> [String] -> IO (ExitCode, String, String)
runDispatchParked dir db args = do
    writeFile (dir </> "dirty-blocker.txt") "block auto-merge\n"
    r <- runDispatch dir db (Just "commit") (["dispatch", "run"] <> args)
    removeFile (dir </> "dirty-blocker.txt")
    pure r
