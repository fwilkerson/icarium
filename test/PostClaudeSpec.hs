module PostClaudeSpec (tests) where

import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (withCurrentDirectory)
import System.Exit (ExitCode (..))
import System.Process.Typed (proc, readProcess, setWorkingDir)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Config (CategoriesConfig (..), CommandsConfig (..), Config (..), DispatchConfig (..), ProjectConfig (..))
import Icarium.Dispatch.Outcome (DispatchCtx (..), DispatchResult (..), FinishArgs (..), finishWith)
import Icarium.Dispatch.PostClaude (checkpointDirtyTree, handlePostClaude)
import Icarium.Git qualified as Git
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Task qualified as RT
import Icarium.Types
import TestHelpers (withCwdLock, withTestDb, withTestRepo)

tests :: TestTree
tests =
    testGroup
        "PostClaude"
        [ testCase "checkpointDirtyTree commits dirty tree with wip message" testCheckpointDirtyTree
        , testCase "checkpointDirtyTree is no-op on clean tree" testCheckpointCleanTree
        , testCase "handlePostClaude failure leaves wip commit on dispatch branch" testHandlePostClaudeFailureCheckpoints
        , testCase "handlePostClaude no-commit success with agent commits is failure; branch retained" testNoCommitAgentCommittedAnyway
        , testCase "finishWith OFailure checkpoints staged changes and leaves base clean" testFinishWithWipCheckpoint
        ]

-- | Run git in a specific directory; return raw stdout bytes as String.
gitIn :: FilePath -> [String] -> IO String
gitIn dir args = do
    (_, out, _) <- readProcess (setWorkingDir dir (proc "git" args))
    pure (show out)

gitIn_ :: FilePath -> [String] -> IO ()
gitIn_ dir args = do
    _ <- readProcess (setWorkingDir dir (proc "git" args))
    pure ()

minCfg :: Config
minCfg =
    Config
        { cfgProject = ProjectConfig{pcIntegrationBranch = "main"}
        , cfgCommands = CommandsConfig{ccBuild = "true", ccTest = "true"}
        , cfgDispatch =
            DispatchConfig
                { dcModel = "claude-sonnet-4-6"
                , dcEffort = Medium
                , dcTools = []
                , dcAllowedTools = []
                , dcScratchDir = "/tmp"
                , dcMaxMinutesPerDispatch = 30
                , dcHeartbeatStaleSeconds = 300
                , dcLogRetentionRuns = 25
                , dcRetryStormThreshold = 3
                }
        , cfgCategories = CategoriesConfig{catDomains = [], catDisciplines = []}
        }

testCheckpointDirtyTree :: IO ()
testCheckpointDirtyTree =
    withTestRepo $ \dir ->
        withCwdLock $ withCurrentDirectory dir $ do
            writeFile (dir <> "/dirty.txt") "uncommitted work"
            checkpointDirtyTree "TESTDID" "claude exited 1"
            logOut <- gitIn dir ["log", "--oneline", "-1"]
            assertBool "wip commit message present" ("wip: dispatch TESTDID" `isInfixOf` logOut)
            assertBool "failed: note in commit" ("failed:" `isInfixOf` logOut)

testCheckpointCleanTree :: IO ()
testCheckpointCleanTree =
    withTestRepo $ \dir ->
        withCwdLock $ withCurrentDirectory dir $ do
            before <- gitIn dir ["rev-parse", "HEAD"]
            checkpointDirtyTree "TESTDID" "some error"
            after <- gitIn dir ["rev-parse", "HEAD"]
            before @?= after

testHandlePostClaudeFailureCheckpoints :: IO ()
testHandlePostClaudeFailureCheckpoints =
    withTestRepo $ \dir ->
        withTestDb $ \conn ->
            withCwdLock $ withCurrentDirectory dir $ do
                let did = "01TESTDISPATCH0000000000AA" :: Text
                    branch = "dispatch/" <> did
                -- create dispatch branch with one commit so it diverges from main
                gitIn_ dir ["checkout", "-b", T.unpack branch]
                writeFile (dir <> "/agent-work.hs") "module A where"
                gitIn_ dir ["add", "agent-work.hs"]
                gitIn_ dir ["commit", "-m", "agent: first edit"]
                -- grab the base sha from main
                baseShaRaw <- gitIn dir ["rev-parse", "main"]
                let baseSha = T.pack (takeWhile (/= '"') (dropWhile (== '"') baseShaRaw))
                -- leave a dirty file
                writeFile (dir <> "/in-progress.hs") "module B where -- incomplete"
                -- insert task and dispatch rows
                tid <-
                    RT.insertTask
                        conn
                        RT.NewTask
                            { RT.ntTitle = "Test task"
                            , RT.ntBody = ""
                            , RT.ntState = Ready
                            , RT.ntPriority = Nothing
                            , RT.ntNoCommit = False
                            }
                RD.insertDispatch
                    conn
                    did
                    RD.NewDispatch
                        { RD.ndTaskId = tid
                        , RD.ndBranch = branch
                        , RD.ndBaseBranch = "main"
                        , RD.ndBaseSha = baseSha
                        , RD.ndModel = "claude-sonnet-4-6"
                        , RD.ndEffort = Medium
                        , RD.ndLogPath = Nothing
                        , RD.ndPid = Nothing
                        }
                let dx =
                        DispatchCtx
                            { dxConn = conn
                            , dxDid = did
                            , dxBranch = branch
                            , dxBase = "main"
                            }
                res <- handlePostClaude dx minCfg False (ExitFailure 1) baseSha "/dev/null"
                -- outcome is failure
                dresOutcome res @?= OFailure
                -- dispatch branch still exists
                branchList <- gitIn dir ["branch", "--list", T.unpack branch]
                assertBool "dispatch branch retained" (T.unpack branch `isInfixOf` branchList)
                -- a wip commit is on the dispatch branch
                logOut <- gitIn dir ["log", T.unpack branch, "--oneline"]
                assertBool "wip commit on dispatch branch" ("wip: dispatch" `isInfixOf` logOut)

{- | finishWith with OFailure must snapshot staged/unstaged changes onto the
dispatch branch before switching to base, so those changes never leak to main.
Asserts (a) base worktree is clean, (b) dispatch branch carries the WIP commit,
(c) the WIP commit SHA is embedded in dresNotes for easy recovery.
-}
testFinishWithWipCheckpoint :: IO ()
testFinishWithWipCheckpoint =
    withTestRepo $ \dir ->
        withTestDb $ \conn ->
            withCwdLock $ withCurrentDirectory dir $ do
                let did = "01TESTWIPCHECKPOINT000000A" :: Text
                    branch = "dispatch/" <> did
                gitIn_ dir ["checkout", "-b", T.unpack branch]
                -- staged change that the agent never committed
                writeFile (dir <> "/staged.hs") "module Staged where"
                gitIn_ dir ["add", "staged.hs"]
                baseShaRaw <- gitIn dir ["rev-parse", "main"]
                let baseSha = T.pack (takeWhile (/= '"') (dropWhile (== '"') baseShaRaw))
                tid <-
                    RT.insertTask
                        conn
                        RT.NewTask
                            { RT.ntTitle = "WIP checkpoint task"
                            , RT.ntBody = ""
                            , RT.ntState = Ready
                            , RT.ntPriority = Nothing
                            , RT.ntNoCommit = False
                            }
                RD.insertDispatch
                    conn
                    did
                    RD.NewDispatch
                        { RD.ndTaskId = tid
                        , RD.ndBranch = branch
                        , RD.ndBaseBranch = "main"
                        , RD.ndBaseSha = baseSha
                        , RD.ndModel = "claude-sonnet-4-6"
                        , RD.ndEffort = Medium
                        , RD.ndLogPath = Nothing
                        , RD.ndPid = Nothing
                        }
                let dx =
                        DispatchCtx
                            { dxConn = conn
                            , dxDid = did
                            , dxBranch = branch
                            , dxBase = "main"
                            }
                res <- finishWith dx FinishArgs{faOutcome = OFailure, faSha = Nothing, faNotes = "agent timed out", faRetention = 25, faLogPath = Nothing, faBaseSha = Just baseSha}
                -- (a) base branch worktree is clean after teardown
                clean <- Git.isClean
                assertBool "base branch worktree is clean after teardown" clean
                -- (b) dispatch branch carries the WIP commit
                logOut <- gitIn dir ["log", T.unpack branch, "--oneline"]
                assertBool "WIP commit on dispatch branch" ("WIP: dispatch" `isInfixOf` logOut)
                -- (c) WIP SHA is surfaced in dresNotes for dispatch show
                assertBool "notes contain wip_commit:" ("wip_commit:" `isInfixOf` T.unpack (dresNotes res))

{- | Regression: a --no-commit task whose agent committed anyway must be
treated as failure, with the dispatch branch retained for inspection. The
prior behavior silently swallowed the (failed) `git branch -d` and reported
success, orphaning the agent's commits.
-}
testNoCommitAgentCommittedAnyway :: IO ()
testNoCommitAgentCommittedAnyway =
    withTestRepo $ \dir ->
        withTestDb $ \conn ->
            withCwdLock $ withCurrentDirectory dir $ do
                let did = "01TESTNCBRANCH0000000000AA" :: Text
                    branch = "dispatch/" <> did
                -- agent commits on the dispatch branch despite no-commit flag
                gitIn_ dir ["checkout", "-b", T.unpack branch]
                writeFile (dir <> "/agent-commit.hs") "module A where"
                gitIn_ dir ["add", "agent-commit.hs"]
                gitIn_ dir ["commit", "-m", "agent: should not have committed"]
                baseShaRaw <- gitIn dir ["rev-parse", "main"]
                let baseSha = T.pack (takeWhile (/= '"') (dropWhile (== '"') baseShaRaw))
                tid <-
                    RT.insertTask
                        conn
                        RT.NewTask
                            { RT.ntTitle = "No-commit task"
                            , RT.ntBody = ""
                            , RT.ntState = Ready
                            , RT.ntPriority = Nothing
                            , RT.ntNoCommit = True
                            }
                RD.insertDispatch
                    conn
                    did
                    RD.NewDispatch
                        { RD.ndTaskId = tid
                        , RD.ndBranch = branch
                        , RD.ndBaseBranch = "main"
                        , RD.ndBaseSha = baseSha
                        , RD.ndModel = "claude-sonnet-4-6"
                        , RD.ndEffort = Medium
                        , RD.ndLogPath = Nothing
                        , RD.ndPid = Nothing
                        }
                let dx =
                        DispatchCtx
                            { dxConn = conn
                            , dxDid = did
                            , dxBranch = branch
                            , dxBase = "main"
                            }
                -- agent exited cleanly, tree is clean — but the branch has commits
                res <- handlePostClaude dx minCfg True ExitSuccess baseSha "/dev/null"
                dresOutcome res @?= OFailure
                branchList <- gitIn dir ["branch", "--list", T.unpack branch]
                assertBool "dispatch branch retained" (T.unpack branch `isInfixOf` branchList)
                logOut <- gitIn dir ["log", T.unpack branch, "--oneline"]
                assertBool "agent's commit still on branch" ("should not have committed" `isInfixOf` logOut)
