module TimeoutSpec (tests) where

import           Control.Concurrent    (threadDelay)
import           Data.Maybe            (isJust)
import qualified Data.Text             as T
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (assertBool, testCase, (@?=))

import           Icarium.Config        (CategoriesConfig (..), CommandsConfig (..), Config (..),
                                        DispatchConfig (..), ProjectConfig (..), validateConfig)
import           Icarium.Dispatch      (DispatchResult (..), handlePostClaude, raceTimeout,
                                        timeoutSentinel)
import qualified Icarium.Repo.Dispatch as RD
import qualified Icarium.Repo.Task     as RT
import           Icarium.Types         (Dispatch (..), DispatchOutcome (..), Effort (..),
                                        TaskState (..))
import           TestHelpers           (withTestDb)

tests :: TestTree
tests = testGroup "timeout"
    [ testCase "raceTimeout fires after deadline"        testRaceTimeoutFires
    , testCase "raceTimeout completes before deadline"   testRaceTimeoutPasses
    , testCase "zero max_minutes rejected by config"     testZeroMaxMinutes
    , testCase "negative max_minutes rejected by config" testNegativeMaxMinutes
    , testCase "timeout sentinel → OFailure recorded"    testTimeoutOutcome
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
        Left msg -> assertBool "message names field"
                        ("max_minutes_per_dispatch" `T.isInfixOf` T.pack msg)
        Right _  -> error "expected Left for max_minutes_per_dispatch = 0"

testNegativeMaxMinutes :: IO ()
testNegativeMaxMinutes =
    case validateConfig (configWith (-1)) of
        Left  _ -> pure ()
        Right _ -> error "expected Left for max_minutes_per_dispatch = -1"

testTimeoutOutcome :: IO ()
testTimeoutOutcome = withTestDb $ \conn -> do
    tid <- RT.insertTask conn RT.NewTask
        { RT.ntTitle    = "timeout test task"
        , RT.ntBody     = "body"
        , RT.ntState    = Ready
        , RT.ntPriority = Nothing
        }
    let did = "01DISPATCH00000000000000000"
    RD.insertDispatch conn did RD.NewDispatch
        { RD.ndTaskId     = tid
        , RD.ndBranch     = "dispatch/" <> did
        , RD.ndBaseBranch = "main"
        , RD.ndBaseSha    = "deadbeef"
        , RD.ndModel      = "test"
        , RD.ndEffort     = High
        , RD.ndLogPath    = Nothing
        , RD.ndPid        = Nothing
        }
    -- Use a non-existent base branch so the internal "git checkout base"
    -- on failure silently no-ops rather than mutating the real working tree.
    res <- handlePostClaude conn did ("dispatch/" <> did) "test-no-such-branch"
               fakeConfig timeoutSentinel "deadbeef" "/tmp/fake.jsonl"
    dresOutcome res @?= OFailure
    assertBool "result notes say timed out"
        ("timed out" `T.isInfixOf` dresNotes res)
    mDisp <- RD.getDispatch conn did
    case mDisp of
        Nothing -> error "dispatch not found in DB"
        Just d  -> do
            assertBool "ended_at is set" (isJust (dispatchEndedAt d))
            assertBool "DB notes say timed out"
                (maybe False ("timed out" `T.isInfixOf`) (dispatchNotes d))

fakeConfig :: Config
fakeConfig = Config
    { cfgProject    = ProjectConfig { pcIntegrationBranch = "main" }
    , cfgCommands   = CommandsConfig { ccBuild = "", ccTest = "" }
    , cfgDispatch   = DispatchConfig
        { dcModel                 = "test"
        , dcEffort                = High
        , dcTools                 = []
        , dcAllowedTools          = []
        , dcScratchDir            = "/tmp"
        , dcMaxMinutesPerDispatch = 5
        , dcMaxDispatchesPerRun   = 1
        , dcHeartbeatStaleSeconds = 300
        , dcLogRetentionRuns      = 1
        }
    , cfgCategories = CategoriesConfig { catDomains = [], catDisciplines = [] }
    }

configWith :: Int -> Config
configWith n = fakeConfig
    { cfgDispatch = (cfgDispatch fakeConfig) { dcMaxMinutesPerDispatch = n } }
