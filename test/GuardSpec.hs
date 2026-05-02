module GuardSpec (tests) where

import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Icarium.Dispatch.Internal (postClaudeGuard)

tests :: TestTree
tests =
    testGroup
        "postClaudeGuard"
        [ testCase "dirty tree fires dirty-tree note" testGuardDirtyTree
        , testCase "empty diff fires empty-diff note" testGuardEmptyDiff
        , testCase "dirty tree takes priority over empty diff" testGuardDirtyFirst
        , testCase "clean tree with new commit passes" testGuardPasses
        , testCase "revParse error does not fire empty-diff" testGuardRevParseError
        ]

baseSha :: Text
baseSha = "aaaa0000"

newSha :: Text
newSha = "bbbb1111"

testGuardDirtyTree :: IO ()
testGuardDirtyTree =
    postClaudeGuard "?? snapshot-test.json\n M src/Foo.hs" (Right newSha) baseSha
        @?= Just
            "agent left uncommitted changes; refusing to merge\n\
            \uncommitted:\n\
            \  ?? snapshot-test.json\n\
            \   M src/Foo.hs"

testGuardEmptyDiff :: IO ()
testGuardEmptyDiff =
    postClaudeGuard "" (Right baseSha) baseSha
        @?= Just "agent made no commits on dispatch branch"

testGuardDirtyFirst :: IO ()
testGuardDirtyFirst =
    postClaudeGuard "?? leftover.txt" (Right baseSha) baseSha
        @?= Just
            "agent left uncommitted changes; refusing to merge\n\
            \uncommitted:\n\
            \  ?? leftover.txt"

testGuardPasses :: IO ()
testGuardPasses =
    postClaudeGuard "" (Right newSha) baseSha @?= Nothing

testGuardRevParseError :: IO ()
testGuardRevParseError =
    postClaudeGuard "" (Left ("git error" :: String)) baseSha @?= Nothing
