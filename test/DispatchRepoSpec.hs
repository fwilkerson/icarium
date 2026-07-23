{- | The @Icarium.Repo.Dispatch@ read/write surface: token columns, the stats
rollup, and merge tracking. Runtime dispatch behaviour lives in the
@CliDispatch*@ and @Dispatch.*@ specs.
-}
module DispatchRepoSpec (tests) where

import Data.Maybe (isJust)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, Query (..), execute)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Repo.Dispatch qualified as RD
import Icarium.Types

import TestHelpers

tests :: TestTree
tests =
    testGroup
        "dispatch repo"
        [ testGroup
            "token columns"
            [ testCase "FromRow reads populated token columns" testDispatchTokensPopulated
            , testCase "FromRow reads NULL token columns as Nothing" testDispatchTokensNull
            ]
        , testGroup
            "dispatch stats"
            [ testCase "empty DB: all zeros" testDispatchStatsEmpty
            , testCase "counts by outcome, sums tokens, counts NULL-token rows" testDispatchStatsAggregates
            , testCase "--since filters to dispatches started at/after the timestamp" testDispatchStatsSince
            ]
        , testGroup
            "merge tracking"
            [ testCase "setMerged round-trips merge_sha and a non-null merged_at" testSetMergedRoundTrips
            , testCase "listParkedDispatches includes success+unmerged, excludes after setMerged" testListParkedDispatches
            ]
        ]

-- =============================================================
-- Token columns
-- =============================================================

insertDispatchWithTokens :: Connection -> Text -> Text -> Maybe Int -> Maybe Int -> Maybe Int -> IO ()
insertDispatchWithTokens c did tid mIn mOut mCache =
    execute
        c
        ( Query
            "INSERT INTO dispatches \
            \(id, task_id, branch, base_branch, base_sha, model, effort, \
            \ tokens_in, tokens_out, tokens_cache_read) \
            \VALUES (?,?,?,?,?,?,?,?,?,?)"
        )
        ( did
        , tid
        , "dispatch/" <> did
        , "main" :: Text
        , "0000000000000000000000000000000000000000" :: Text
        , "claude-sonnet-4-6" :: Text
        , "medium" :: Text
        , mIn
        , mOut
        , mCache
        )

testDispatchTokensPopulated :: IO ()
testDispatchTokensPopulated = withTestDb $ \c -> do
    tid <- mkTaskRow c "Token task"
    let did = "01DISP0000000000000000001D" :: Text
    insertDispatchWithTokens c did tid (Just 1234) (Just 567) (Just 89)
    Just d <- RD.getDispatch c did
    dispatchTokensIn d @?= Just 1234
    dispatchTokensOut d @?= Just 567
    dispatchTokensCacheRead d @?= Just 89

testDispatchTokensNull :: IO ()
testDispatchTokensNull = withTestDb $ \c -> do
    tid <- mkTaskRow c "No token task"
    let did = "01DISP0000000000000000002D" :: Text
    insertDispatchWithTokens c did tid Nothing Nothing Nothing
    Just d <- RD.getDispatch c did
    dispatchTokensIn d @?= Nothing
    dispatchTokensOut d @?= Nothing
    dispatchTokensCacheRead d @?= Nothing

-- =============================================================
-- Dispatch stats
-- =============================================================

insertDispatchFull ::
    Connection -> Text -> Text -> Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> IO ()
insertDispatchFull c did tid startedAt mOutcome mIn mOut mCache = do
    execute
        c
        ( Query
            "INSERT INTO dispatches \
            \(id, task_id, branch, base_branch, base_sha, model, effort, started_at) \
            \VALUES (?,?,?,?,?,?,?,?)"
        )
        ( did
        , tid
        , "dispatch/" <> did
        , "main" :: Text
        , "0000000000000000000000000000000000000000" :: Text
        , "claude-sonnet-4-6" :: Text
        , "medium" :: Text
        , startedAt
        )
    execute
        c
        ( Query
            "UPDATE dispatches \
            \SET outcome = ?, tokens_in = ?, tokens_out = ?, tokens_cache_read = ? \
            \WHERE id = ?"
        )
        (mOutcome, mIn, mOut, mCache, did)

testDispatchStatsEmpty :: IO ()
testDispatchStatsEmpty = withTestDb $ \c -> do
    s <- RD.getDispatchStats c Nothing
    s
        @?= RD.DispatchStats
            { RD.dsTotal = 0
            , RD.dsSuccess = 0
            , RD.dsFailure = 0
            , RD.dsInterrupted = 0
            , RD.dsOpen = 0
            , RD.dsTokensIn = 0
            , RD.dsTokensOut = 0
            , RD.dsTokensCacheRead = 0
            , RD.dsMissingTokens = 0
            }

testDispatchStatsAggregates :: IO ()
testDispatchStatsAggregates = withTestDb $ \c -> do
    tid <- mkTaskRow c "Stats task"
    insertDispatchFull c "01STATS000000000000000001S" tid "2026-01-01 00:00:00" (Just "success") (Just 100) (Just 20) (Just 5)
    insertDispatchFull c "01STATS000000000000000002S" tid "2026-01-01 00:00:00" (Just "failure") (Just 50) (Just 10) (Just 1)
    insertDispatchFull c "01STATS000000000000000003S" tid "2026-01-01 00:00:00" (Just "interrupted") Nothing Nothing Nothing
    insertDispatchFull c "01STATS000000000000000004S" tid "2026-01-01 00:00:00" Nothing Nothing Nothing Nothing
    s <- RD.getDispatchStats c Nothing
    RD.dsTotal s @?= 4
    RD.dsSuccess s @?= 1
    RD.dsFailure s @?= 1
    RD.dsInterrupted s @?= 1
    RD.dsOpen s @?= 1
    RD.dsTokensIn s @?= 150
    RD.dsTokensOut s @?= 30
    RD.dsTokensCacheRead s @?= 6
    RD.dsMissingTokens s @?= 2

testDispatchStatsSince :: IO ()
testDispatchStatsSince = withTestDb $ \c -> do
    tid <- mkTaskRow c "Stats since task"
    insertDispatchFull c "01SINCE000000000000000001S" tid "2026-01-01 00:00:00" (Just "success") (Just 10) (Just 1) (Just 0)
    insertDispatchFull c "01SINCE000000000000000002S" tid "2026-01-03 00:00:00" (Just "success") (Just 20) (Just 2) (Just 0)
    s <- RD.getDispatchStats c (Just "2026-01-02 00:00:00")
    RD.dsTotal s @?= 1
    RD.dsTokensIn s @?= 20

-- =============================================================
-- Merge tracking
-- =============================================================

testSetMergedRoundTrips :: IO ()
testSetMergedRoundTrips = withTestDb $ \c -> do
    tid <- mkTaskRow c "Merge tracking task"
    let did = "01MERGE000000000000000001M" :: Text
    insertTestDispatch c did tid
    RD.finishDispatch c did OSuccess Nothing Nothing
    Just before <- RD.getDispatch c did
    dispatchMergeSha before @?= Nothing
    dispatchMergedAt before @?= Nothing

    RD.setMerged c did "deadbeef"
    Just after <- RD.getDispatch c did
    dispatchMergeSha after @?= Just "deadbeef"
    assertBool "merged_at set" (isJust (dispatchMergedAt after))

testListParkedDispatches :: IO ()
testListParkedDispatches = withTestDb $ \c -> do
    tid <- mkTaskRow c "Parked task"
    let did = "01PARK0000000000000000001P" :: Text
    insertTestDispatch c did tid
    RD.finishDispatch c did OSuccess Nothing Nothing

    parked <- RD.listParkedDispatches c
    map dispatchId parked @?= [did]

    RD.setMerged c did "cafebabe"
    parked' <- RD.listParkedDispatches c
    null parked' @?= True
