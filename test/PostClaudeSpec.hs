module PostClaudeSpec (tests) where

import Control.Exception (bracket)
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, close, open)
import System.Directory (withCurrentDirectory)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (proc, readProcess, setWorkingDir)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Config (CategoriesConfig (..), CommandsConfig (..), Config (..), DispatchConfig (..), ProjectConfig (..))
import Icarium.Db (migrateDb)
import Icarium.Dispatch.Outcome (DispatchCtx (..), dresOutcome)
import Icarium.Dispatch.PostClaude (checkpointDirtyTree, handlePostClaude)
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Task qualified as RT
import Icarium.Schema (applySchema)
import Icarium.Types

tests :: TestTree
tests =
    testGroup
        "PostClaude"
        [ testCase "checkpointDirtyTree commits dirty tree with wip message" testCheckpointDirtyTree
        , testCase "checkpointDirtyTree is no-op on clean tree" testCheckpointCleanTree
        , testCase "handlePostClaude failure leaves wip commit on dispatch branch" testHandlePostClaudeFailureCheckpoints
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

-- | Set up a bare-minimum git repo with a single initial commit on main.
initRepo :: FilePath -> IO ()
initRepo dir = do
    gitIn_ dir ["init", "-b", "main"]
    gitIn_ dir ["config", "user.email", "test@example.com"]
    gitIn_ dir ["config", "user.name", "Test"]
    writeFile (dir <> "/README") "init"
    gitIn_ dir ["add", "README"]
    gitIn_ dir ["commit", "-m", "initial"]

withTestRepo :: (FilePath -> IO a) -> IO a
withTestRepo k =
    withSystemTempDirectory "icarium-pc-test" $ \dir -> do
        initRepo dir
        k dir

withTestDb :: (Connection -> IO a) -> IO a
withTestDb act = bracket (open ":memory:") close $ \conn -> do
    applySchema conn
    migrateDb conn
    act conn

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
                }
        , cfgCategories = CategoriesConfig{catDomains = [], catDisciplines = []}
        }

testCheckpointDirtyTree :: IO ()
testCheckpointDirtyTree =
    withTestRepo $ \dir ->
        withCurrentDirectory dir $ do
            writeFile (dir <> "/dirty.txt") "uncommitted work"
            checkpointDirtyTree "TESTDID" "claude exited 1"
            logOut <- gitIn dir ["log", "--oneline", "-1"]
            assertBool "wip commit message present" ("wip: dispatch TESTDID" `isInfixOf` logOut)
            assertBool "failed: note in commit" ("failed:" `isInfixOf` logOut)

testCheckpointCleanTree :: IO ()
testCheckpointCleanTree =
    withTestRepo $ \dir ->
        withCurrentDirectory dir $ do
            before <- gitIn dir ["rev-parse", "HEAD"]
            checkpointDirtyTree "TESTDID" "some error"
            after <- gitIn dir ["rev-parse", "HEAD"]
            before @?= after

testHandlePostClaudeFailureCheckpoints :: IO ()
testHandlePostClaudeFailureCheckpoints =
    withTestRepo $ \dir ->
        withTestDb $ \conn ->
            withCurrentDirectory dir $ do
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
