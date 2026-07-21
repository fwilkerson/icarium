module LogResultSpec (tests) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Dispatch.LogResult

tests :: TestTree
tests =
    testGroup
        "LogResult"
        [ testGroup "fmtMs" testFmtMs
        , testGroup "readLogResult" testReadLogResult
        ]

-- =============================================================
-- fmtMs
-- =============================================================

testFmtMs :: [TestTree]
testFmtMs =
    [ testCase (show ms <> " -> " <> show expected) (fmtMs ms @?= expected)
    | (ms, expected) <-
        [ (0, "0ms")
        , (1, "1ms")
        , (999, "999ms")
        , (1000, "1.0s")
        , (1500, "1.5s")
        , (10000, "10.0s")
        ]
    ]

-- =============================================================
-- readLogResult
-- =============================================================

resultLine :: Text
resultLine =
    "{\"type\":\"result\",\"num_turns\":3,\"duration_ms\":1500"
        <> ",\"duration_api_ms\":900,\"total_cost_usd\":0.0025"
        <> ",\"usage\":{\"input_tokens\":100,\"output_tokens\":50,\"cache_read_input_tokens\":10}"
        <> ",\"result\":\"Task complete\"}"

testReadLogResult :: [TestTree]
testReadLogResult =
    [ testCase "missing file returns Nothing" $ do
        r <- readLogResult "/tmp/icarium-nonexistent-fixture-xyz.jsonl"
        case r of
            Nothing -> pure ()
            Just _ -> fail "expected Nothing for missing file"
    , testCase "result line parsed: all fields round-trip" $
        withSystemTempFile "icarium-test.jsonl" $ \path h -> do
            TIO.hPutStrLn h resultLine
            TIO.hPutStrLn h "{\"type\":\"assistant\",\"message\":{}}"
            hClose h
            mr <- readLogResult path
            case mr of
                Nothing -> fail "expected Just LogResult"
                Just lr -> do
                    lrNumTurns lr @?= Just 3
                    lrDurationMs lr @?= Just 1500
                    lrDurationApiMs lr @?= Just 900
                    lrCostUsd lr @?= Just 0.0025
                    lrResultText lr @?= Just "Task complete"
                    case lrUsage lr of
                        Nothing -> fail "expected usage"
                        Just u -> do
                            luInputTokens u @?= Just 100
                            luOutputTokens u @?= Just 50
                            luCacheReads u @?= Just 10
    , testCase "empty file returns Nothing" $
        withSystemTempFile "icarium-test.jsonl" $ \path h -> do
            hClose h
            r <- readLogResult path
            case r of
                Nothing -> pure ()
                Just _ -> fail "expected Nothing for empty file"
    ]
