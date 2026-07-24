module WorktreeSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Icarium.Dispatch.Worktree (WorktreeError (..), setupExitError, worktreePath)

tests :: TestTree
tests =
    testGroup
        "worktree"
        [ testCase "setup exit 75 is back-pressure, others are failures" testSetupExitClassifier
        , testCase "worktreePath embeds the dispatch id under .icarium/wt" testWorktreePath
        ]

testSetupExitClassifier :: IO ()
testSetupExitClassifier = do
    setupExitError "init.sh" 75 @?= WtNoCapacity "init.sh -> exit 75"
    setupExitError "init.sh" 1 @?= WtSetupFailed "init.sh -> exit 1"
    setupExitError "init.sh" 74 @?= WtSetupFailed "init.sh -> exit 74"

testWorktreePath :: IO ()
testWorktreePath =
    worktreePath "01ABCDEF" @?= ".icarium/wt/01ABCDEF"
