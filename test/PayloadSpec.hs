module PayloadSpec (tests) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import Icarium.Dispatch.Payload
import Icarium.Types (ReviewVerdict (..))

tests :: TestTree
tests =
    testGroup
        "Payload"
        [ testGroup "schemas" schemaCases
        , testGroup "worker payload" workerCases
        , testGroup "reviewer payload" reviewerCases
        , testGroup "verdictFromFindings" verdictCases
        , testGroup "renderFindings" renderCases
        ]

-- =============================================================
-- Schemas
-- =============================================================

schemaArg :: Schema -> Text
schemaArg s = case jsonSchemaArgs s of
    ["--json-schema", v] -> v
    other -> error ("unexpected jsonSchemaArgs shape: " <> show other)

goldenCase :: String -> FilePath -> Schema -> TestTree
goldenCase name path s = testCase name $ do
    expected <- T.strip <$> TIO.readFile path
    schemaArg s @?= expected

schemaCases :: [TestTree]
schemaCases =
    [ goldenCase "worker schema" "test/fixtures/schema/worker.json" workerSchema
    , goldenCase "reviewer schema" "test/fixtures/schema/reviewer.json" reviewerSchema
    ]

-- =============================================================
-- Payload decoding
-- =============================================================

mustRight :: (Show e) => Either e a -> IO a
mustRight = either (assertFailure . show) pure

workerCases :: [TestTree]
workerCases =
    [ testCase "submitted with no future notes" $ do
        p <- mustRight (decodeWorkerPayload "{\"status\":\"submitted\",\"for_future_agents\":[]}")
        p @?= WorkerPayload WSubmitted Nothing []
    , testCase "blocked carries its reason" $ do
        p <-
            mustRight
                ( decodeWorkerPayload
                    "{\"status\":\"blocked\",\"block_reason\":\"no network\",\"for_future_agents\":[]}"
                )
        p @?= WorkerPayload WBlocked (Just "no network") []
    , testCase "future notes decode title and body" $ do
        p <-
            mustRight
                ( decodeWorkerPayload
                    "{\"status\":\"submitted\",\"for_future_agents\":\
                    \[{\"title\":\"sqlite WAL\",\"body\":\"the store is opened WAL-mode\"}]}"
                )
        wpForFutureAgents p @?= [FutureNote "sqlite WAL" "the store is opened WAL-mode"]
    , testCase "unknown status is a decode error" $
        assertLeft (decodeWorkerPayload "{\"status\":\"done\",\"for_future_agents\":[]}")
    , testCase "missing for_future_agents is a decode error" $
        assertLeft (decodeWorkerPayload "{\"status\":\"submitted\"}")
    ]

reviewerCases :: [TestTree]
reviewerCases =
    [ testCase "empty findings" $ do
        p <- mustRight (decodeReviewerPayload "{\"findings\":[]}")
        p @?= ReviewerPayload []
    , testCase "spec finding without a file" $ do
        p <-
            mustRight
                ( decodeReviewerPayload
                    "{\"findings\":[{\"axis\":\"spec\",\"severity\":\"fail\",\"message\":\"never implemented\"}]}"
                )
        rpFindings p @?= [Finding AxisSpec SevFail Nothing "never implemented"]
    , testCase "standards finding with a file" $ do
        p <-
            mustRight
                ( decodeReviewerPayload
                    "{\"findings\":[{\"axis\":\"standards\",\"severity\":\"warn\",\"file\":\"src/Foo.hs\",\"message\":\"comment says what\"}]}"
                )
        rpFindings p @?= [Finding AxisStandards SevWarn (Just "src/Foo.hs") "comment says what"]
    , testCase "unknown axis is a decode error" $
        assertLeft
            ( decodeReviewerPayload
                "{\"findings\":[{\"axis\":\"style\",\"severity\":\"warn\",\"message\":\"x\"}]}"
            )
    , testCase "non-JSON text is a decode error" $
        assertLeft (decodeReviewerPayload "I reviewed the diff and it looks fine.")
    ]

assertLeft :: (Show a) => Either Text a -> IO ()
assertLeft = \case
    Left _ -> pure ()
    Right v -> assertFailure ("expected a decode error, got " <> show v)

-- =============================================================
-- Verdict derivation
-- =============================================================

verdictCases :: [TestTree]
verdictCases =
    [ testCase "no findings is pass" $ verdictFromFindings [] @?= RVPass
    , testCase "only warns is warn" $
        verdictFromFindings [warnF, warnF] @?= RVWarn
    , testCase "one fail among warns is fail" $
        verdictFromFindings [warnF, failF, warnF] @?= RVFail
    ]
  where
    warnF = Finding AxisStandards SevWarn (Just "src/Foo.hs") "possible Duplicated Code"
    failF = Finding AxisSpec SevFail Nothing "the task asks for X; the diff never implements it"

-- =============================================================
-- Rendering
-- =============================================================

renderCases :: [TestTree]
renderCases =
    [ testCase "table of findings" $
        renderFindings
            [ Finding AxisStandards SevWarn (Just "src/Foo.hs") "possible Duplicated Code"
            , Finding AxisSpec SevFail Nothing "asks for X; never implemented"
            ]
            @?= "| axis | severity | file | message |\n\
                \| --- | --- | --- | --- |\n\
                \| standards | warn | src/Foo.hs | possible Duplicated Code |\n\
                \| spec | fail | - | asks for X; never implemented |\n"
    , testCase "a multi-line, pipe-bearing message stays one row" $
        renderFindings [Finding AxisSpec SevWarn Nothing "a | b\nsecond line"]
            @?= "| axis | severity | file | message |\n\
                \| --- | --- | --- | --- |\n\
                \| spec | warn | - | a \\| b second line |\n"
    , testCase "no findings" $ renderFindings [] @?= "(no findings)"
    ]
