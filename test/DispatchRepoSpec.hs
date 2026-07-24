{- | The @Icarium.Repo.Dispatch@ read/write surface: token columns, the stats
rollup, and merge tracking. Runtime dispatch behaviour lives in the
@CliDispatch*@ and @Dispatch.*@ specs.
-}
module DispatchRepoSpec (tests) where

import Data.Maybe (isJust)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, Query (..), execute, (:.) (..))
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
            "column layout"
            [ testCase "every column lands in its own field" testDispatchRoundTrip
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
        , testGroup
            "updateDispatch"
            [ testCase "writes the named columns and leaves the rest alone" testUpdateDispatchPatches
            , testCase "an empty update touches nothing" testUpdateDispatchEmpty
            , testCase "the heartbeat stamp is a single-column write" testUpdateDispatchHeartbeat
            , testCase "the review stamp lands all three columns together" testUpdateDispatchReview
            ]
        ]

-- =============================================================
-- Token columns
-- =============================================================

-- | A dispatch row with the token columns left unwritten.
insertDispatchWithoutTokens :: Connection -> Text -> Text -> IO ()
insertDispatchWithoutTokens c did tid =
    execute
        c
        ( Query
            "INSERT INTO dispatches \
            \(id, task_id, branch, base_branch, base_sha, model, effort) \
            \VALUES (?,?,?,?,?,?,?)"
        )
        ( did
        , tid
        , "dispatch/" <> did
        , "main" :: Text
        , "0000000000000000000000000000000000000000" :: Text
        , "claude-sonnet-4-6" :: Text
        , "medium" :: Text
        )

{- | The order half of the invariant, for the widest row in the schema:
23 columns, most of them @Maybe Text@. Values are the column's own name so
a transposition reports @expected "merge_sha" but got "last_commit"@.
-}
testDispatchRoundTrip :: IO ()
testDispatchRoundTrip = withTestDb $ \c -> do
    tid <- mkTaskRow c "Round-trip task"
    let did = "01DISP0000000000000000001D" :: Text
    execute
        c
        ( Query
            "INSERT INTO dispatches \
            \(id, task_id, branch, base_branch, base_sha, pid, model, effort, \
            \ started_at, heartbeat_at, ended_at, outcome, merge_sha, last_commit, \
            \ notes, log_path, tokens_in, tokens_out, tokens_cache_read, \
            \ review_verdict, reviewer_log_path, merged_at, body_changed) \
            \VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
        )
        ( (did, tid, "branch" :: Text, "base_branch" :: Text, "base_sha" :: Text)
            :. (11 :: Int, "model" :: Text, "xhigh" :: Text)
            :. ("started_at" :: Text, "heartbeat_at" :: Text, "ended_at" :: Text)
            :. ("interrupted" :: Text, "merge_sha" :: Text, "last_commit" :: Text)
            :. ("notes" :: Text, "log_path" :: Text)
            :. (12 :: Int, 13 :: Int, 14 :: Int)
            :. ("warn" :: Text, "reviewer_log_path" :: Text, "merged_at" :: Text, 1 :: Int)
        )
    Just d <- RD.getDispatch c did
    dispatchId d @?= did
    dispatchTaskId d @?= tid
    dispatchBranch d @?= "branch"
    dispatchBaseBranch d @?= "base_branch"
    dispatchBaseSha d @?= "base_sha"
    dispatchPid d @?= Just 11
    dispatchModel d @?= "model"
    dispatchEffort d @?= XHigh
    dispatchStartedAt d @?= "started_at"
    dispatchHeartbeat d @?= "heartbeat_at"
    dispatchEndedAt d @?= Just "ended_at"
    dispatchOutcome d @?= Just OInterrupted
    dispatchMergeSha d @?= Just "merge_sha"
    dispatchLastCommit d @?= Just "last_commit"
    dispatchNotes d @?= Just "notes"
    dispatchLogPath d @?= Just "log_path"
    dispatchTokensIn d @?= Just 12
    dispatchTokensOut d @?= Just 13
    dispatchTokensCacheRead d @?= Just 14
    dispatchReviewVerdict d @?= Just RVWarn
    dispatchReviewerLogPath d @?= Just "reviewer_log_path"
    dispatchMergedAt d @?= Just "merged_at"
    dispatchBodyChanged d @?= Just True

testDispatchTokensNull :: IO ()
testDispatchTokensNull = withTestDb $ \c -> do
    tid <- mkTaskRow c "No token task"
    let did = "01DISP0000000000000000002D" :: Text
    insertDispatchWithoutTokens c did tid
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
    RD.finishDispatch c did OSuccess Nothing
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
    RD.finishDispatch c did OSuccess Nothing

    parked <- RD.listParkedDispatches c
    map dispatchId parked @?= [did]

    RD.setMerged c did "cafebabe"
    parked' <- RD.listParkedDispatches c
    null parked' @?= True

-- =============================================================
-- updateDispatch
-- =============================================================

-- | Backdate the heartbeat so a re-stamp is observable at second resolution.
backdateHeartbeat :: Connection -> Text -> IO ()
backdateHeartbeat c did =
    execute
        c
        (Query "UPDATE dispatches SET heartbeat_at = ? WHERE id = ?")
        ("2020-01-01 00:00:00" :: Text, did)

testUpdateDispatchPatches :: IO ()
testUpdateDispatchPatches = withTestDb $ \c -> do
    tid <- mkTaskRow c "Patch task"
    let did = "01UPD00000000000000000001U" :: Text
    insertTestDispatch c did tid
    RD.updateDispatch c did RD.emptyUpdate{RD.duNotes = Just "first"}
    RD.updateDispatch c did RD.emptyUpdate{RD.duPid = Just 4242, RD.duLastCommit = Just "abc123"}
    Just d <- RD.getDispatch c did
    dispatchNotes d @?= Just "first"
    dispatchPid d @?= Just 4242
    dispatchLastCommit d @?= Just "abc123"
    dispatchTokensIn d @?= Nothing
    dispatchOutcome d @?= Nothing

testUpdateDispatchEmpty :: IO ()
testUpdateDispatchEmpty = withTestDb $ \c -> do
    tid <- mkTaskRow c "Empty patch task"
    let did = "01UPD00000000000000000002U" :: Text
    insertTestDispatch c did tid
    RD.updateDispatch c did RD.emptyUpdate{RD.duNotes = Just "kept"}
    backdateHeartbeat c did
    RD.updateDispatch c did RD.emptyUpdate
    Just d <- RD.getDispatch c did
    dispatchNotes d @?= Just "kept"
    dispatchHeartbeat d @?= "2020-01-01 00:00:00"

testUpdateDispatchHeartbeat :: IO ()
testUpdateDispatchHeartbeat = withTestDb $ \c -> do
    tid <- mkTaskRow c "Heartbeat task"
    let did = "01UPD00000000000000000003U" :: Text
    insertTestDispatch c did tid
    RD.updateDispatch c did RD.emptyUpdate{RD.duNotes = Just "kept", RD.duPid = Just 7}
    backdateHeartbeat c did
    RD.updateDispatch c did RD.emptyUpdate{RD.duStampHeartbeat = True}
    Just d <- RD.getDispatch c did
    assertBool "heartbeat re-stamped" (dispatchHeartbeat d /= "2020-01-01 00:00:00")
    dispatchNotes d @?= Just "kept"
    dispatchPid d @?= Just 7

testUpdateDispatchReview :: IO ()
testUpdateDispatchReview = withTestDb $ \c -> do
    tid <- mkTaskRow c "Review task"
    let did = "01UPD00000000000000000004U" :: Text
    insertTestDispatch c did tid
    RD.updateDispatch
        c
        did
        RD.emptyUpdate
            { RD.duReview =
                Just
                    RD.ReviewStamp
                        { RD.rsVerdict = RVWarn
                        , RD.rsLogPath = "/tmp/reviewer.jsonl"
                        , RD.rsBodyChanged = True
                        }
            }
    Just d <- RD.getDispatch c did
    dispatchReviewVerdict d @?= Just RVWarn
    dispatchReviewerLogPath d @?= Just "/tmp/reviewer.jsonl"
    dispatchBodyChanged d @?= Just True
