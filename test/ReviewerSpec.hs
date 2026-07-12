module ReviewerSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Icarium.Dispatch.Reviewer (parseReviewVerdictFromText)
import Icarium.Types (ReviewVerdict (..))

tests :: TestTree
tests = testGroup "Reviewer" [testGroup "parseReviewVerdictFromText" cases]

cases :: [TestTree]
cases =
    [ testCase "narration then block: block wins" $
        parseReviewVerdictFromText
            "status: fail would require more work\n\
            \```yaml\n\
            \status: pass\n\
            \findings: []\n\
            \```"
            @?= RVPass
    , testCase "block then narration: block still wins" $
        parseReviewVerdictFromText
            "```yaml\n\
            \status: pass\n\
            \findings: []\n\
            \```\n\
            \status: fail is not the case here"
            @?= RVPass
    , testCase "status outside any fenced block is ignored -> fail" $
        parseReviewVerdictFromText "status: pass" @?= RVFail
    , testCase "no fenced block at all -> fail" $
        parseReviewVerdictFromText "just some text, no fences" @?= RVFail
    , testCase "multiple yaml blocks: only the last counts" $
        parseReviewVerdictFromText
            "```yaml\n\
            \status: fail\n\
            \```\n\
            \some narration in between\n\
            \```yaml\n\
            \status: warn\n\
            \findings: []\n\
            \```"
            @?= RVWarn
    , testCase "unknown status value inside the last block -> fail" $
        parseReviewVerdictFromText
            "```yaml\n\
            \status: maybe\n\
            \```"
            @?= RVFail
    , testCase "bare (non-yaml) fenced block does not count" $
        parseReviewVerdictFromText
            "```\n\
            \status: pass\n\
            \```"
            @?= RVFail
    , testCase "yaml fence tolerates surrounding whitespace" $
        parseReviewVerdictFromText
            "  ```yaml  \n\
            \status: warn\n\
            \```"
            @?= RVWarn
    , testCase "valid pass in the last yaml block" $
        parseReviewVerdictFromText
            "```yaml\n\
            \status: pass\n\
            \findings: []\n\
            \```"
            @?= RVPass
    ]
