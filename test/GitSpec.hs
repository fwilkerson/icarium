module GitSpec (tests) where

import Control.Monad (void)
import Data.Text (Text)
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.FilePath ((</>))
import System.Process.Typed (proc, readProcess, setWorkingDir)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Icarium.Git qualified as Git
import TestHelpers (withTestRepo)

tests :: TestTree
tests =
    testGroup
        "Git"
        [ testGroup "parseWorktreeList" parseCases
        , testCase "worktreeAdd creates dir + branch; branchCheckedOutAt locates it" testWorktreeAdd
        , testCase "mergeBaseIsAncestor tracks FF-possibility" testAncestor
        , testCase "rebase moves a branch onto an advanced base" testRebase
        , testCase "worktreeRemove + worktreePrune leave only the main checkout" testRemove
        ]

-- Setup git commands never rely on process cwd — every call names the repo.
gitIn_ :: FilePath -> [String] -> IO ()
gitIn_ dir args = void (readProcess (setWorkingDir dir (proc "git" args)))

expectRight :: (Show e) => Either e a -> IO a
expectRight = either (\e -> assertFailure ("unexpected Left: " <> show e)) pure

-- =============================================================
-- Pure parser
-- =============================================================

parseCases :: [TestTree]
parseCases =
    [ testCase "main checkout only" $
        Git.parseWorktreeList mainOnly @?= [("/repo", Just "refs/heads/main")]
    , testCase "two worktrees" $
        Git.parseWorktreeList twoWorktrees
            @?= [ ("/repo", Just "refs/heads/main")
                , ("/repo/.icarium/wt/D1", Just "refs/heads/dispatch/D1")
                ]
    , testCase "detached HEAD stanza has no branch" $
        Git.parseWorktreeList detached @?= [("/repo/d", Nothing)]
    , testCase "bare stanza has no branch" $
        Git.parseWorktreeList bare @?= [("/repo/b", Nothing)]
    ]
  where
    mainOnly = "worktree /repo\nHEAD abc123\nbranch refs/heads/main\n"
    twoWorktrees =
        "worktree /repo\nHEAD abc123\nbranch refs/heads/main\n\n"
            <> "worktree /repo/.icarium/wt/D1\nHEAD def456\nbranch refs/heads/dispatch/D1\n"
    detached = "worktree /repo/d\nHEAD 789abc\ndetached\n"
    bare = "worktree /repo/b\nbare\n"

-- =============================================================
-- IO helpers against real git
-- =============================================================

testWorktreeAdd :: IO ()
testWorktreeAdd =
    withTestRepo $ \repo -> do
        let wt = repo </> "wt-a"
            branch = "feature-x" :: Text
        expectRight =<< Git.worktreeAdd repo wt branch "main"
        exists <- doesDirectoryExist wt
        assertBool "worktree dir created" exists
        -- new branch sits at the base SHA
        branchSha <- expectRight =<< Git.revParse repo branch
        baseSha <- expectRight =<< Git.revParse repo "main"
        branchSha @?= baseSha
        -- branchCheckedOutAt finds the new worktree (compare canonical paths:
        -- macOS temp dirs sit behind /var -> /private/var symlinks).
        loc <- Git.branchCheckedOutAt repo branch
        case loc of
            Nothing -> assertFailure "branch not found in any worktree"
            Just p -> do
                cp <- canonicalizePath p
                cwt <- canonicalizePath wt
                cp @?= cwt
        unknown <- Git.branchCheckedOutAt repo "no-such-branch"
        unknown @?= Nothing

testAncestor :: IO ()
testAncestor =
    withTestRepo $ \repo -> do
        base <- expectRight =<< Git.revParse repo "main"
        -- branch cut from base, one commit ahead
        gitIn_ repo ["checkout", "-b", "feat"]
        writeFile (repo </> "a") "a"
        gitIn_ repo ["add", "a"]
        gitIn_ repo ["commit", "-m", "feat commit"]
        branch <- expectRight =<< Git.revParse repo "feat"
        ffPossible <- Git.mergeBaseIsAncestor repo base branch
        assertBool "base is ancestor of descendant" ffPossible
        -- base advances independently
        gitIn_ repo ["checkout", "main"]
        writeFile (repo </> "b") "b"
        gitIn_ repo ["add", "b"]
        gitIn_ repo ["commit", "-m", "main advances"]
        newBase <- expectRight =<< Git.revParse repo "main"
        stillFF <- Git.mergeBaseIsAncestor repo newBase branch
        assertBool "advanced base no longer ancestor of branch" (not stillFF)

testRebase :: IO ()
testRebase =
    withTestRepo $ \repo -> do
        -- branch cut from base
        gitIn_ repo ["checkout", "-b", "feat"]
        writeFile (repo </> "feat.txt") "feat"
        gitIn_ repo ["add", "feat.txt"]
        gitIn_ repo ["commit", "-m", "feat work"]
        -- base advances independently (disjoint files -> no conflict)
        gitIn_ repo ["checkout", "main"]
        writeFile (repo </> "base.txt") "base"
        gitIn_ repo ["add", "base.txt"]
        gitIn_ repo ["commit", "-m", "base advance"]
        newBase <- expectRight =<< Git.revParse repo "main"
        -- rebase feat onto the advanced base
        gitIn_ repo ["checkout", "feat"]
        expectRight =<< Git.rebase repo "main"
        contains <- Git.mergeBaseIsAncestor repo newBase "feat"
        assertBool "feat contains the new base commit after rebase" contains

testRemove :: IO ()
testRemove =
    withTestRepo $ \repo -> do
        let wt = repo </> "wt-rm"
        expectRight =<< Git.worktreeAdd repo wt "tmp-branch" "main"
        expectRight =<< Git.worktreeRemove repo wt False
        Git.worktreePrune repo
        out <- expectRight =<< Git.runGit repo ["worktree", "list", "--porcelain"]
        length (Git.parseWorktreeList out) @?= 1
