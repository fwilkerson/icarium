-- | The 'Routing' bundle: its precedence fold and its trip through storage.
module RoutingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Icarium.Repo.Task qualified as RT
import Icarium.Types

import TestHelpers

tests :: TestTree
tests =
    testGroup
        "routing"
        [ testCase "mempty inherits every field" testMemptyInherits
        , testCase "merge is left-biased per field" testMergePerField
        , testCase "insert stores routing; update sets and clears it" testStorageRoundTrip
        ]

testMemptyInherits :: IO ()
testMemptyInherits = do
    rtModel mempty @?= Nothing
    rtEffort mempty @?= Nothing

-- The dispatch precedence chain is `flag <> task <> …`, so a flag that sets
-- only one field must not shadow the task's choice for the other.
testMergePerField :: IO ()
testMergePerField = do
    let flagRouting = mempty{rtModel = Just "claude-opus-4-8"}
        taskRouting' = Routing{rtModel = Just "claude-haiku-4-5-20251001", rtEffort = Just Low}
        merged = flagRouting <> taskRouting'
    rtModel merged @?= Just "claude-opus-4-8"
    rtEffort merged @?= Just Low

testStorageRoundTrip :: IO ()
testStorageRoundTrip = withTestDb $ \c -> do
    tid <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "Routed task"
                , RT.ntBody = ""
                , RT.ntState = ReadyHeadless
                , RT.ntPriority = Nothing
                , RT.ntNoCommit = False
                , RT.ntRouting = Routing{rtModel = Just "claude-haiku-4-5-20251001", rtEffort = Just Low}
                }
    Just t0 <- RT.getTask c tid
    taskRouting t0 @?= Routing{rtModel = Just "claude-haiku-4-5-20251001", rtEffort = Just Low}

    -- A patch touches only the fields it names.
    _ <- RT.updateTask c tid RT.emptyUpdate{RT.tuRouting = \r -> r{rtEffort = Just Max}}
    Just t1 <- RT.getTask c tid
    taskRouting t1 @?= Routing{rtModel = Just "claude-haiku-4-5-20251001", rtEffort = Just Max}

    _ <- RT.updateTask c tid RT.emptyUpdate{RT.tuRouting = const mempty}
    Just t2 <- RT.getTask c tid
    taskRouting t2 @?= mempty
