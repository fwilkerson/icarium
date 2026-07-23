module HeartbeatSpec (tests) where

import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Icarium.Heartbeat (
    DispatchHealth (..),
    dispatchHealth,
    dispatchIsInterrupted,
    healthInterrupted,
    heartbeatStale,
 )
import Icarium.Types (Dispatch (..), Effort (..))

parseUTC :: String -> UTCTime
parseUTC s = case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" s of
    Just t -> t
    Nothing -> error ("HeartbeatSpec: bad fixture timestamp: " <> s)

now :: UTCTime
now = parseUTC "2026-04-30 12:00:00"

{- | A dispatch with no pid, so 'dispatchHealth' resolves alive=False without
signalling a real process.
-}
deadDispatch :: Text -> Dispatch
deadDispatch hb =
    Dispatch
        { dispatchId = "01AAA0000000000000000000AA"
        , dispatchTaskId = "01TTT0000000000000000000AA"
        , dispatchBranch = "dispatch/01AAA0000000000000000000AA"
        , dispatchBaseBranch = "main"
        , dispatchBaseSha = "abc"
        , dispatchPid = Nothing
        , dispatchModel = "claude-sonnet-4-6"
        , dispatchEffort = Medium
        , dispatchStartedAt = "2026-04-30 11:00:00"
        , dispatchHeartbeat = hb
        , dispatchEndedAt = Nothing
        , dispatchOutcome = Nothing
        , dispatchMergeSha = Nothing
        , dispatchLastCommit = Nothing
        , dispatchNotes = Nothing
        , dispatchLogPath = Nothing
        , dispatchTokensIn = Nothing
        , dispatchTokensOut = Nothing
        , dispatchTokensCacheRead = Nothing
        , dispatchReviewVerdict = Nothing
        , dispatchReviewerLogPath = Nothing
        , dispatchMergedAt = Nothing
        , dispatchBodyChanged = Nothing
        }

tests :: TestTree
tests =
    testGroup
        "Heartbeat"
        [ testGroup
            "heartbeatStale"
            [ testCase "unparseable text → stale" $
                assertBool "expected True" (heartbeatStale now 60 "not-a-timestamp")
            , testCase "recent heartbeat → not stale" $
                assertBool "expected False" (not (heartbeatStale now 60 "2026-04-30 11:59:30"))
            , testCase "old heartbeat → stale" $
                assertBool "expected True" (heartbeatStale now 60 "2026-04-30 11:58:00")
            ]
        , testGroup
            "healthInterrupted"
            [ testCase "alive and fresh → running" $
                assertEqual "" False (healthInterrupted (DispatchHealth True False))
            , testCase "alive but stale → interrupted" $
                assertEqual "" True (healthInterrupted (DispatchHealth True True))
            , testCase "dead but fresh → interrupted" $
                assertEqual "" True (healthInterrupted (DispatchHealth False False))
            , testCase "dead and stale → interrupted" $
                assertEqual "" True (healthInterrupted (DispatchHealth False True))
            ]
        , testGroup
            "dispatchHealth"
            [ testCase "no pid → not alive, staleness from heartbeat" $ do
                h <- dispatchHealth now 60 (deadDispatch "2026-04-30 11:59:30")
                assertEqual "alive" False (dhAlive h)
                assertEqual "stale" False (dhStale h)
            , testCase "no pid, old heartbeat → stale" $ do
                h <- dispatchHealth now 60 (deadDispatch "2026-04-30 11:58:00")
                assertEqual "stale" True (dhStale h)
            , testCase "dispatchIsInterrupted agrees with healthInterrupted" $ do
                i <- dispatchIsInterrupted now 60 (deadDispatch "2026-04-30 11:59:30")
                assertEqual "" True i
            ]
        ]
