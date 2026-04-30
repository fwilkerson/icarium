module HeartbeatSpec (tests) where

import           Data.Time         (UTCTime, defaultTimeLocale, parseTimeM)
import           Test.Tasty        (TestTree, testGroup)
import           Test.Tasty.HUnit  (assertBool, testCase)

import           Icarium.Heartbeat (heartbeatStale)

parseUTC :: String -> UTCTime
parseUTC s = case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" s of
    Just t  -> t
    Nothing -> error ("HeartbeatSpec: bad fixture timestamp: " <> s)

now :: UTCTime
now = parseUTC "2026-04-30 12:00:00"

tests :: TestTree
tests = testGroup "heartbeatStale"
    [ testCase "unparseable text → stale" $
        assertBool "expected True" (heartbeatStale now 60 "not-a-timestamp")
    , testCase "recent heartbeat → not stale" $
        assertBool "expected False" (not (heartbeatStale now 60 "2026-04-30 11:59:30"))
    , testCase "old heartbeat → stale" $
        assertBool "expected True" (heartbeatStale now 60 "2026-04-30 11:58:00")
    ]
