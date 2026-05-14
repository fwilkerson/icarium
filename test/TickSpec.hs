module TickSpec (tests) where

import Data.ByteString.Char8 qualified as BC
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Dispatch.Tick (TickAction (..), TickState, emptyTickState, summariseTick)

tests :: TestTree
tests =
    testGroup
        "summariseTick"
        [ testCase "system event emits model= session= line" testTickSystem
        , testCase "assistant tool_use emits * tool line with name" testTickAssistantToolUse
        , testCase "assistant text emits > assistant line" testTickAssistantText
        , testCase "user is_error tool_result emits x tool_result line" testTickUserError
        , testCase "user tool_result without is_error emits nothing" testTickUserNoError
        , testCase "result event emits + result and = usage lines" testTickResult
        , testCase "20th assistant event fires periodic = usage line" testTickPeriodicUsage
        , testCase "malformed JSON emits ? unknown line and does not crash" testTickMalformed
        , testCase "3 consecutive api_retry events returns TickKill" testRetryStormKills
        , testCase "assistant text between retries resets counter; no kill on next two retries" testRetryStormResetsOnSubstantive
        , testCase "threshold=5 kills at 5 retries, not 3" testRetryStormThreshold5
        ]

tickTs :: String
tickTs = "12:00:00"

tick :: BC.ByteString -> ([String], TickState, TickAction)
tick bytes = summariseTick 3 tickTs bytes emptyTickState

tickWith :: BC.ByteString -> TickState -> ([String], TickState, TickAction)
tickWith = summariseTick 3 tickTs

tickWith5 :: BC.ByteString -> TickState -> ([String], TickState, TickAction)
tickWith5 = summariseTick 5 tickTs

strIn :: String -> String -> Bool
strIn needle = T.isInfixOf (T.pack needle) . T.pack

testTickSystem :: IO ()
testTickSystem = do
    let line = "{\"type\":\"system\",\"model\":\"claude-x\",\"session_id\":\"abcd1234xyz\"}"
        (out, _, _) = tick line
    length out @?= 1
    assertBool ". system glyph" (". system" `strIn` head out)
    assertBool "model= present" ("model=claude-x" `strIn` head out)
    assertBool "session= present" ("session=abcd1234" `strIn` head out)

testTickAssistantToolUse :: IO ()
testTickAssistantToolUse = do
    let line = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"tool_use\",\"name\":\"Bash\",\"input\":{\"command\":\"ls\"}}]}}"
        (out, _, _) = tick line
    assertBool "non-empty output" (not (null out))
    assertBool "* tool glyph" ("* tool" `strIn` head out)
    assertBool "tool name in body" ("Bash" `strIn` head out)

testTickAssistantText :: IO ()
testTickAssistantText = do
    let line = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"hello world\"}]}}"
        (out, _, _) = tick line
    assertBool "non-empty output" (not (null out))
    assertBool "> assistant glyph" ("> assistant" `strIn` head out)

testTickUserError :: IO ()
testTickUserError = do
    let line = "{\"type\":\"user\",\"message\":{\"content\":[{\"type\":\"tool_result\",\"is_error\":true,\"content\":\"something failed\"}]}}"
        (out, _, _) = tick line
    length out @?= 1
    assertBool "x tool_result glyph" ("x tool_result" `strIn` head out)

testTickUserNoError :: IO ()
testTickUserNoError = do
    let line = "{\"type\":\"user\",\"message\":{\"content\":[{\"type\":\"tool_result\",\"content\":\"ok\"}]}}"
        (out, _, _) = tick line
    out @?= []

testTickResult :: IO ()
testTickResult = do
    let line = "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\",\"usage\":{\"input_tokens\":100,\"output_tokens\":50,\"cache_read_input_tokens\":20}}"
        (out, _, _) = tick line
    length out @?= 2
    assertBool "+ result line" ("+ result" `strIn` head out)
    assertBool "= usage line" ("= usage" `strIn` (out !! 1))
    assertBool "in 100" ("in 100" `strIn` (out !! 1))

testTickPeriodicUsage :: IO ()
testTickPeriodicUsage = do
    let assistantLine = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"x\"}],\"usage\":{\"input_tokens\":5,\"output_tokens\":3,\"cache_read_input_tokens\":1}}}"
        stN (_, st, _) = st
        st19 = iterate (stN . tickWith assistantLine) emptyTickState !! 19
        (out20, st20, _) = tickWith assistantLine st19
    assertBool "periodic = usage fires on 20th" (any ("= usage" `strIn`) out20)
    let (out21, _, _) = tickWith assistantLine st20
    assertBool "no periodic usage on 21st" (not (any ("= usage" `strIn`) out21))

testTickMalformed :: IO ()
testTickMalformed = do
    let line = "not valid json at all { }"
        (out, _, _) = tick line
    length out @?= 1
    assertBool "? unknown glyph" ("? unknown" `strIn` head out)

testRetryStormKills :: IO ()
testRetryStormKills = do
    let retryLine = "{\"type\":\"system\",\"subtype\":\"api_retry\",\"message\":\"overloaded\"}"
        (_, st1, a1) = tick retryLine
        (_, st2, a2) = tickWith retryLine st1
        (_, _, a3) = tickWith retryLine st2
    a1 @?= TickContinue
    a2 @?= TickContinue
    a3 @?= TickKill "retry-storm: 3 consecutive api_retry events"

testRetryStormResetsOnSubstantive :: IO ()
testRetryStormResetsOnSubstantive = do
    let retryLine = "{\"type\":\"system\",\"subtype\":\"api_retry\"}"
        assistantLine = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"continuing\"}]}}"
        (_, st1, _) = tick retryLine
        (_, st2, _) = tickWith retryLine st1
        -- substantive assistant turn resets the counter
        (_, st3, _) = tickWith assistantLine st2
        (_, st4, a4) = tickWith retryLine st3
        (_, _, a5) = tickWith retryLine st4
    a4 @?= TickContinue
    a5 @?= TickContinue

testRetryStormThreshold5 :: IO ()
testRetryStormThreshold5 = do
    let retryLine = "{\"type\":\"system\",\"subtype\":\"api_retry\",\"message\":\"overloaded\"}"
        (_, st1, a1) = tickWith5 retryLine emptyTickState
        (_, st2, a2) = tickWith5 retryLine st1
        (_, st3, a3) = tickWith5 retryLine st2
        (_, st4, a4) = tickWith5 retryLine st3
        (_, _, a5) = tickWith5 retryLine st4
    a1 @?= TickContinue
    a2 @?= TickContinue
    a3 @?= TickContinue -- would kill at threshold=3, but not at 5
    a4 @?= TickContinue
    a5 @?= TickKill "retry-storm: 5 consecutive api_retry events"
