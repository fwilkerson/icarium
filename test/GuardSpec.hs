module GuardSpec (tests) where

import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Dispatch.Claude (claudeArgs)
import Icarium.Dispatch.PostClaude (postClaudeGuard)
import Icarium.Types (Effort (..))

tests :: TestTree
tests =
    testGroup
        "postClaudeGuard"
        [ testCase "dirty tree fires dirty-tree note" testGuardDirtyTree
        , testCase "empty diff fires empty-diff note" testGuardEmptyDiff
        , testCase "dirty tree takes priority over empty diff" testGuardDirtyFirst
        , testCase "clean tree with new commit passes" testGuardPasses
        , testCase "revParse error does not fire empty-diff" testGuardRevParseError
        , testCase "claudeArgs includes --permission-mode dontAsk plus existing flags" testClaudeArgsPermissionMode
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

-- | True when @x@, @y@ appear as two consecutive elements of @xs@.
hasAdjacentPair :: (Eq a) => a -> a -> [a] -> Bool
hasAdjacentPair x y xs = (x, y) `elem` zip xs (drop 1 xs)

testClaudeArgsPermissionMode :: IO ()
testClaudeArgsPermissionMode = do
    let args = claudeArgs "claude-sonnet-4-6" Medium ["Read", "Edit"] ["Read"]
    assertBool "adjacent --permission-mode dontAsk" (hasAdjacentPair "--permission-mode" "dontAsk" args)
    assertBool "-p present" ("-p" `elem` args)
    assertBool "adjacent --output-format stream-json" (hasAdjacentPair "--output-format" "stream-json" args)
