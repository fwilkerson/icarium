module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Icarium.Config (loadConfig, defaultConfigText)
import Icarium.Render (renderTaskPrompt)
import Icarium.Types

main :: IO ()
main = defaultMain $ testGroup "icarium"
    [ testGroup "TaskState round-trips"   taskStateTests
    , testGroup "EdgeKind round-trips"    edgeKindTests
    , testGroup "Effort round-trips"      effortTests
    , testGroup "CategoryAxis round-trips" categoryAxisTests
    , testCase "loadConfig succeeds on default template" loadConfigTest
    , testCase "renderTaskPrompt is non-empty for minimal task" renderTest
    ]

roundTrip :: (Show a, Eq a) => (a -> Text) -> (Text -> Maybe a) -> Text -> a -> TestTree
roundTrip toTxt fromTxt label val = testCase (T.unpack label) $ do
    toTxt val @?= label
    fromTxt label @?= Just val

taskStateTests :: [TestTree]
taskStateTests =
    [ roundTrip taskStateText parseTaskState "idea"      Idea
    , roundTrip taskStateText parseTaskState "planned"   Planned
    , roundTrip taskStateText parseTaskState "ready"     Ready
    , roundTrip taskStateText parseTaskState "done"      Done
    , roundTrip taskStateText parseTaskState "blocked"   Blocked
    , roundTrip taskStateText parseTaskState "abandoned" Abandoned
    ]

edgeKindTests :: [TestTree]
edgeKindTests =
    [ roundTrip edgeKindText parseEdgeKind "depends_on"   DependsOn
    , roundTrip edgeKindText parseEdgeKind "references"   References
    , roundTrip edgeKindText parseEdgeKind "derived_from" DerivedFrom
    , roundTrip edgeKindText parseEdgeKind "supersedes"   Supersedes
    ]

effortTests :: [TestTree]
effortTests =
    [ roundTrip effortText parseEffort "low"    Low
    , roundTrip effortText parseEffort "medium" Medium
    , roundTrip effortText parseEffort "high"   High
    ]

categoryAxisTests :: [TestTree]
categoryAxisTests =
    [ roundTrip categoryAxisText parseCategoryAxis "domain"     Domain
    , roundTrip categoryAxisText parseCategoryAxis "discipline" Discipline
    ]

loadConfigTest :: IO ()
loadConfigTest =
    withSystemTempFile "icarium.toml" $ \fp h -> do
        hClose h
        TIO.writeFile fp defaultConfigText
        result <- loadConfig fp
        case result of
            Left err -> fail ("loadConfig failed: " <> err)
            Right _  -> pure ()

renderTest :: IO ()
renderTest = do
    let t   = Task
                { taskId          = "01TEST00000000000000000000"
                , taskTitle       = "Test task"
                , taskBody        = "Body text"
                , taskState       = Ready
                , taskPriority    = Nothing
                , taskBlockReason = Nothing
                , taskCreatedAt   = "2026-01-01T00:00:00Z"
                , taskUpdatedAt   = "2026-01-01T00:00:00Z"
                }
        out = renderTaskPrompt t [] []
    assertBool "renderTaskPrompt returned empty" (not (T.null out))
