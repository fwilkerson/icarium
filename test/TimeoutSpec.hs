module TimeoutSpec (tests) where

import Control.Concurrent (threadDelay)
import Data.Maybe (isJust)
import Data.Text qualified as T
import System.Directory (withCurrentDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Config (
    CategoriesConfig (..),
    CommandsConfig (..),
    Config (..),
    DispatchConfig (..),
    ProjectConfig (..),
    validateConfig,
 )
import Icarium.Dispatch.Claude (raceTimeout, timeoutSentinel)
import Icarium.Dispatch.Outcome (DispatchCtx (..), DispatchResult (..))
import Icarium.Dispatch.PostClaude (handlePostClaude)
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Task qualified as RT
import Icarium.Types (
    Dispatch (..),
    DispatchOutcome (..),
    Effort (..),
    TaskState (..),
 )
import TestHelpers (withCwdLock, withTestDb, withTestRepo)

tests :: TestTree
tests =
    testGroup
        "timeout"
        [ testCase "raceTimeout fires after deadline" testRaceTimeoutFires
        , testCase "raceTimeout completes before deadline" testRaceTimeoutPasses
        , testCase "zero max_minutes rejected by config" testZeroMaxMinutes
        , testCase "negative max_minutes rejected by config" testNegativeMaxMinutes
        , testCase "timeout sentinel → OFailure recorded" testTimeoutOutcome
        ]

testRaceTimeoutFires :: IO ()
testRaceTimeoutFires = do
    result <- raceTimeout 1_000 (threadDelay 500_000)
    result @?= Left ()

testRaceTimeoutPasses :: IO ()
testRaceTimeoutPasses = do
    result <- raceTimeout 500_000 (pure (42 :: Int))
    result @?= Right 42

testZeroMaxMinutes :: IO ()
testZeroMaxMinutes =
    case validateConfig (configWith 0) of
        Left msg ->
            assertBool
                "message names field"
                ("max_minutes_per_dispatch" `T.isInfixOf` T.pack msg)
        Right _ -> error "expected Left for max_minutes_per_dispatch = 0"

testNegativeMaxMinutes :: IO ()
testNegativeMaxMinutes =
    case validateConfig (configWith (-1)) of
        Left _ -> pure ()
        Right _ -> error "expected Left for max_minutes_per_dispatch = -1"

testTimeoutOutcome :: IO ()
testTimeoutOutcome = withTestRepo $ \dir -> withTestDb $ \conn -> withCwdLock $ withCurrentDirectory dir $ do
    tid <-
        RT.insertTask
            conn
            RT.NewTask
                { RT.ntTitle = "timeout test task"
                , RT.ntBody = "body"
                , RT.ntState = Ready
                , RT.ntPriority = Nothing
                , RT.ntNoCommit = False
                }
    let did = "01DISPATCH00000000000000000"
    RD.insertDispatch
        conn
        did
        RD.NewDispatch
            { RD.ndTaskId = tid
            , RD.ndBranch = "dispatch/" <> did
            , RD.ndBaseBranch = "main"
            , RD.ndBaseSha = "deadbeef"
            , RD.ndModel = "test"
            , RD.ndEffort = High
            , RD.ndLogPath = Nothing
            , RD.ndPid = Nothing
            }
    let dx =
            DispatchCtx
                { dxConn = conn
                , dxDbPath = ":memory:"
                , dxDid = did
                , dxBranch = "dispatch/" <> did
                , dxBase = "main"
                , dxWorkDir = dir
                }
    res <- handlePostClaude dx fakeConfig False timeoutSentinel "deadbeef" "/tmp/fake.jsonl"
    dresOutcome res @?= OFailure
    assertBool
        "result notes say timed out"
        ("timed out" `T.isInfixOf` dresNotes res)
    mDisp <- RD.getDispatch conn did
    case mDisp of
        Nothing -> error "dispatch not found in DB"
        Just d -> do
            assertBool "ended_at is set" (isJust (dispatchEndedAt d))
            assertBool
                "DB notes say timed out"
                (maybe False ("timed out" `T.isInfixOf`) (dispatchNotes d))

fakeConfig :: Config
fakeConfig =
    Config
        { cfgProject = ProjectConfig{pcIntegrationBranch = "main"}
        , cfgCommands = Just CommandsConfig{ccBuild = "", ccTest = ""}
        , cfgDispatch =
            DispatchConfig
                { dcModel = "test"
                , dcEffort = High
                , dcTools = []
                , dcAllowedTools = []
                , dcScratchDir = "/tmp"
                , dcMaxMinutesPerDispatch = 5
                , dcHeartbeatStaleSeconds = 300
                , dcLogRetentionRuns = 1
                , dcRetryStormThreshold = 3
                , dcWorktreeSetup = Nothing
                , dcWorktreeTeardown = Nothing
                , dcMcpConfig = Nothing
                }
        , cfgCategories = CategoriesConfig{catDomains = [], catDisciplines = []}
        , cfgReview = Nothing
        }

configWith :: Int -> Config
configWith n =
    fakeConfig
        { cfgDispatch = (cfgDispatch fakeConfig){dcMaxMinutesPerDispatch = n}
        }
