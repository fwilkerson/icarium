{- | The shared prompt core: @task show --prompt@ previews exactly what
@dispatch run@ sends, so the CLI body must be a prefix of the dispatch prompt.
-}
module PromptSpec (tests) where

import Control.Monad (void)
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Icarium.Dispatch.Claude (claudeArgs)
import Icarium.Dispatch.Internal (buildPrompt)
import Icarium.Prompt (taskPromptBody)
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Edge qualified as RE
import Icarium.Repo.Task qualified as RT
import Icarium.Types

import TestHelpers

tests :: TestTree
tests =
    testGroup
        "prompt"
        [ testCase "CLI preview is a prefix of the dispatch prompt" testPreviewIsPrefix
        , testCase "claudeArgs includes --permission-mode dontAsk plus existing flags" testClaudeArgsPermissionMode
        ]

-- | True when @x@, @y@ appear as two consecutive elements of @xs@.
hasAdjacentPair :: (Eq a) => a -> a -> [a] -> Bool
hasAdjacentPair x y xs = (x, y) `elem` zip xs (drop 1 xs)

testClaudeArgsPermissionMode :: IO ()
testClaudeArgsPermissionMode = do
    let args = claudeArgs "claude-sonnet-4-6" Medium ["Read", "Edit"] ["Read"] Nothing
    assertBool "adjacent --permission-mode dontAsk" (hasAdjacentPair "--permission-mode" "dontAsk" args)
    assertBool "-p present" ("-p" `elem` args)
    assertBool "adjacent --output-format stream-json" (hasAdjacentPair "--output-format" "stream-json" args)
    assertBool "--strict-mcp-config present" ("--strict-mcp-config" `elem` args)
    assertBool "no --mcp-config when key absent" ("--mcp-config" `notElem` args)
    -- The gate ingests the worker's final message, so it must be constrained.
    assertBool "--json-schema present" ("--json-schema" `elem` args)
    let argsWithMcp = claudeArgs "claude-sonnet-4-6" Medium ["Read", "Edit"] ["Read"] (Just ".mcp.json")
    assertBool
        "adjacent --mcp-config <path> when key set"
        (hasAdjacentPair "--mcp-config" ".mcp.json" argsWithMcp)

testPreviewIsPrefix :: IO ()
testPreviewIsPrefix = withTestDb $ \c -> do
    domCat <- mkCat c Domain "cli"
    discCat <- mkCat c Discipline "haskell"
    tid <- mkTaskRow c "Subject"
    RC.attachTaskCategory c tid (categoryId domCat)
    RC.attachTaskCategory c tid (categoryId discCat)

    refId <- mkContext c "Explicit ref" "ref body"
    void (RE.insertEdge c References TaskNode tid ContextNode refId)

    matchedId <- mkContext c "Auto-pulled" "matched body"
    attachContextCats c matchedId [domCat, discCat]

    depId <- mkTaskRow c "Dependency"
    void (RE.insertEdge c DependsOn TaskNode tid TaskNode depId)

    Just t <- RT.getTask c tid
    preview <- taskPromptBody c t
    full <- buildPrompt c t "/tmp/scratch" Nothing Nothing

    assertBool "preview non-trivial" ("Auto-pulled" `T.isInfixOf` preview)
    assertBool "explicit ref present" ("Explicit ref" `T.isInfixOf` preview)
    assertBool "dependency present" ("Dependency" `T.isInfixOf` preview)
    assertBool "preview is a prefix of the dispatch prompt" (preview `T.isPrefixOf` full)
