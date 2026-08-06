{- | The shared prompt core: @task show --prompt@ previews exactly what
@dispatch run@ sends, so the CLI body must be a prefix of the dispatch prompt.
-}
module PromptSpec (tests) where

import Control.Monad (void)
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Dispatch.Claude (ClaudeInvocation (..), claudeArgs)
import Icarium.Dispatch.Internal (buildPrompt)
import Icarium.Dispatch.Payload (workerSchema)
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
        ( testCase "CLI preview is a prefix of the dispatch prompt" testPreviewIsPrefix
            : testCase "claudeArgs includes --permission-mode dontAsk plus existing flags" testClaudeArgsPermissionMode
            : reviewerArgCases
        )

-- | True when @x@, @y@ appear as two consecutive elements of @xs@.
hasAdjacentPair :: (Eq a) => a -> a -> [a] -> Bool
hasAdjacentPair x y xs = (x, y) `elem` zip xs (drop 1 xs)

testClaudeArgsPermissionMode :: IO ()
testClaudeArgsPermissionMode = do
    let inv =
            ClaudeInvocation
                { invModel = "claude-sonnet-4-6"
                , invEffort = Medium
                , invTools = ["Read", "Edit"]
                , invAllowedTools = ["Read"]
                , invMcpConfig = Nothing
                , invSchema = workerSchema
                }
        args = claudeArgs inv
    assertBool "adjacent --permission-mode dontAsk" (hasAdjacentPair "--permission-mode" "dontAsk" args)
    assertBool "-p present" ("-p" `elem` args)
    assertBool "adjacent --output-format stream-json" (hasAdjacentPair "--output-format" "stream-json" args)
    assertBool "--strict-mcp-config present" ("--strict-mcp-config" `elem` args)
    assertBool "no --mcp-config when key absent" ("--mcp-config" `notElem` args)
    -- The gate ingests the worker's final message, so it must be constrained.
    assertBool "--json-schema present" ("--json-schema" `elem` args)
    let argsWithMcp = claudeArgs inv{invMcpConfig = Just ".mcp.json"}
    assertBool
        "adjacent --mcp-config <path> when key set"
        (hasAdjacentPair "--mcp-config" ".mcp.json" argsWithMcp)

{- | The reviewer's grant is a security boundary, so the assertion is the whole
set rather than a list of tools that must be absent: a denylist only catches
the widenings someone thought to name.
-}
reviewerArgCases :: [TestTree]
reviewerArgCases =
    [ testCase "reviewerArgs grants exactly read-only navigation, both flags alike" $
        (flagValue "--tools" args, flagValue "--allowedTools" args)
            @?= (Just "Read,Glob,Grep", Just "Read,Glob,Grep")
    , testCase "reviewerArgs disables slash commands" $
        assertBool "--disable-slash-commands present" ("--disable-slash-commands" `elem` args)
    , testCase "reviewerArgs runs with --permission-mode dontAsk" $
        assertBool "adjacent" (hasAdjacentPair "--permission-mode" "dontAsk" args)
    , -- The verdict is ingested, so it must be schema-constrained.
      testCase "reviewerArgs constrains the final message" $
        assertBool "--json-schema present" ("--json-schema" `elem` args)
    , testCase "reviewerArgs grants no MCP servers" $
        assertBool "--mcp-config absent" ("--mcp-config" `notElem` args)
    ]
  where
    args = reviewerArgs "claude-opus-5" Medium

-- | The value following @flag@, if the flag is present and carries one.
flagValue :: (Eq a) => a -> [a] -> Maybe a
flagValue flag xs = lookup flag (zip xs (drop 1 xs))

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
