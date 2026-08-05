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
import Icarium.Dispatch.Reviewer (reviewerArgs)
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
        , testCase "reviewerArgs grants read-only navigation and nothing that executes" testReviewerArgsAreReadOnlyNavigation
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

{- | The reviewer needs to find code, not just open it: Read alone left it
guessing filenames one at a time. Glob and Grep close that without widening
the boundary, which is why the negative half of this test is the load-bearing
half -- nothing here may execute, write, or spawn.
-}
testReviewerArgsAreReadOnlyNavigation :: IO ()
testReviewerArgsAreReadOnlyNavigation = do
    let args = reviewerArgs "claude-opus-5" Medium
        granted = [t | (flag, t) <- zip args (drop 1 args), flag == "--tools" || flag == "--allowedTools"]
    case granted of
        [tools, allowed] -> do
            assertBool "--tools and --allowedTools grant the same set" (tools == allowed)
            let names = splitOn ',' tools
            mapM_ (\tool -> assertBool (tool <> " granted") (tool `elem` names)) ["Read", "Glob", "Grep"]
            mapM_
                (\tool -> assertBool (tool <> " must not be granted") (tool `notElem` names))
                ["Bash", "Edit", "Write", "Task", "Agent", "Skill", "WebFetch"]
        _ -> assertBool "--tools and --allowedTools each carry a value" False
    assertBool "slash commands stay disabled" ("--disable-slash-commands" `elem` args)
    assertBool "adjacent --permission-mode dontAsk" (hasAdjacentPair "--permission-mode" "dontAsk" args)
    -- The verdict is ingested, so it must be schema-constrained.
    assertBool "--json-schema present" ("--json-schema" `elem` args)

-- | Split on a separator. @splitOn ',' "Read,Glob"@ is @["Read", "Glob"]@.
splitOn :: Char -> String -> [String]
splitOn sep s = case break (== sep) s of
    (field, []) -> [field]
    (field, _ : rest) -> field : splitOn sep rest

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
