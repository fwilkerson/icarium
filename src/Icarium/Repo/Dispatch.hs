module Icarium.Repo.Dispatch (
    NewDispatch (..),
    insertDispatch,
    getDispatch,
    getDispatchesByPrefix,
    resolveDispatchId,
    listOpenDispatches,
    listDispatches,
    listParkedDispatches,
    updateHeartbeat,
    updateTokens,
    setLastCommit,
    setPid,
    updateNotes,
    setReviewInfo,
    setMerged,
    finishDispatch,
    logPathsOutsideRetention,
    DispatchStats (..),
    getDispatchStats,
) where

import Data.Text (Text, pack, unpack)
import Database.SQLite.Simple (Connection, FromRow (..), Only (..), Query (..), execute, field, query, query_)

import Icarium.Repo.Internal (prefixLookup, resolveByPrefix)
import Icarium.Types (Dispatch (..), DispatchOutcome, Effort, ReviewVerdict)

data NewDispatch = NewDispatch
    { ndTaskId :: Text
    , ndBranch :: Text
    , ndBaseBranch :: Text
    , ndBaseSha :: Text
    , ndModel :: Text
    , ndEffort :: Effort
    , ndLogPath :: Maybe FilePath
    , ndPid :: Maybe Int
    }

dispatchCols :: Text
dispatchCols =
    "id, task_id, branch, base_branch, base_sha, pid, model, effort, \
    \started_at, heartbeat_at, ended_at, outcome, merge_sha, last_commit, \
    \notes, log_path, tokens_in, tokens_out, tokens_cache_read, \
    \review_verdict, reviewer_log_path, merged_at, body_changed"

{- | Insert a dispatch. The caller supplies the id so that the branch
name and log path (which both embed the id) can be computed before
the row hits the DB.
-}
insertDispatch :: Connection -> Text -> NewDispatch -> IO ()
insertDispatch conn did NewDispatch{..} =
    execute
        conn
        ( Query
            "INSERT INTO dispatches \
            \(id, task_id, branch, base_branch, base_sha, pid, \
            \ model, effort, log_path) \
            \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        )
        ( did
        , ndTaskId
        , ndBranch
        , ndBaseBranch
        , ndBaseSha
        , ndPid
        , ndModel
        , ndEffort
        , ndLogPath
        )

getDispatch :: Connection -> Text -> IO (Maybe Dispatch)
getDispatch conn did = do
    rows <-
        query
            conn
            (Query $ "SELECT " <> dispatchCols <> " FROM dispatches WHERE id = ?")
            (Only did)
    pure $ case rows of
        (d : _) -> Just d
        [] -> Nothing

-- | Dispatches whose ULID starts with @prefix@.
getDispatchesByPrefix :: Connection -> Text -> IO [Dispatch]
getDispatchesByPrefix conn = prefixLookup conn "dispatches" dispatchCols

-- | Resolve a user-supplied string to a canonical dispatch ULID via prefix match.
resolveDispatchId :: Connection -> Text -> IO (Either String Text)
resolveDispatchId conn = resolveByPrefix (getDispatchesByPrefix conn) dispatchId "dispatch"

{- | Every dispatch whose outcome column is NULL. Heartbeat freshness
is the caller's concern (recovery, status).
-}
listOpenDispatches :: Connection -> IO [Dispatch]
listOpenDispatches conn =
    query_
        conn
        ( Query $
            "SELECT "
                <> dispatchCols
                <> " FROM dispatches WHERE outcome IS NULL ORDER BY started_at ASC"
        )

listDispatches :: Connection -> Maybe Text -> IO [Dispatch]
listDispatches conn Nothing =
    query_
        conn
        ( Query $
            "SELECT "
                <> dispatchCols
                <> " FROM dispatches ORDER BY started_at DESC"
        )
listDispatches conn (Just tid) =
    query
        conn
        ( Query $
            "SELECT "
                <> dispatchCols
                <> " FROM dispatches WHERE task_id = ? ORDER BY started_at DESC"
        )
        (Only tid)

-- | Successful dispatches whose branch hasn't been merged yet ("parked").
listParkedDispatches :: Connection -> IO [Dispatch]
listParkedDispatches conn =
    query_
        conn
        ( Query $
            "SELECT "
                <> dispatchCols
                <> " FROM dispatches WHERE outcome = 'success' AND merge_sha IS NULL \
                   \ORDER BY started_at ASC"
        )

updateHeartbeat :: Connection -> Text -> IO ()
updateHeartbeat conn did =
    execute
        conn
        (Query "UPDATE dispatches SET heartbeat_at = datetime('now') WHERE id = ?")
        (Only did)

updateTokens :: Connection -> Text -> Int -> Int -> Int -> IO ()
updateTokens conn did tokIn tokOut tokCache =
    execute
        conn
        ( Query
            "UPDATE dispatches \
            \SET tokens_in = ?, tokens_out = ?, tokens_cache_read = ? \
            \WHERE id = ?"
        )
        (tokIn, tokOut, tokCache, did)

setLastCommit :: Connection -> Text -> Text -> IO ()
setLastCommit conn did sha =
    execute
        conn
        (Query "UPDATE dispatches SET last_commit = ? WHERE id = ?")
        (sha, did)

setPid :: Connection -> Text -> Int -> IO ()
setPid conn did pid =
    execute
        conn
        (Query "UPDATE dispatches SET pid = ? WHERE id = ?")
        (pid, did)

updateNotes :: Connection -> Text -> Text -> IO ()
updateNotes conn did notes =
    execute
        conn
        (Query "UPDATE dispatches SET notes = ? WHERE id = ?")
        (notes, did)

{- | The Bool is the body-changed tamper signal; stored alongside the
verdict so one can never be stamped without the other.
-}
setReviewInfo :: Connection -> Text -> ReviewVerdict -> FilePath -> Bool -> IO ()
setReviewInfo conn did verdict reviewerLogPath bodyChanged =
    execute
        conn
        ( Query
            "UPDATE dispatches \
            \SET review_verdict = ?, reviewer_log_path = ?, body_changed = ? \
            \WHERE id = ?"
        )
        (verdict, pack reviewerLogPath, bodyChanged, did)

-- | Stamp a dispatch as landed: merge sha plus the current timestamp.
setMerged :: Connection -> Text -> Text -> IO ()
setMerged conn did sha =
    execute
        conn
        ( Query
            "UPDATE dispatches \
            \SET merge_sha = ?, merged_at = datetime('now') \
            \WHERE id = ?"
        )
        (sha, did)

{- | Log paths for dispatches outside the N most recent by started_at DESC.
Includes both worker and reviewer log paths. Only returns non-NULL entries.
-}
logPathsOutsideRetention :: Connection -> Int -> IO [FilePath]
logPathsOutsideRetention conn n = do
    rows <-
        query
            conn
            ( Query
                "SELECT log_path, reviewer_log_path FROM dispatches \
                \ORDER BY started_at DESC \
                \LIMIT -1 OFFSET ?"
            )
            (Only n)
    pure [unpack p | (mW, mR) <- rows, Just p <- [mW, mR]]

-- | Aggregate spend/outcome summary for @dispatch stats@.
data DispatchStats = DispatchStats
    { dsTotal :: Int
    , dsSuccess :: Int
    , dsFailure :: Int
    , dsInterrupted :: Int
    , dsOpen :: Int
    , dsTokensIn :: Int
    , dsTokensOut :: Int
    , dsTokensCacheRead :: Int
    , dsMissingTokens :: Int
    }
    deriving (Show, Eq)

instance FromRow DispatchStats where
    fromRow =
        DispatchStats
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

statsCols :: Text
statsCols =
    "COUNT(*), \
    \COALESCE(SUM(CASE WHEN outcome = 'success' THEN 1 ELSE 0 END), 0), \
    \COALESCE(SUM(CASE WHEN outcome = 'failure' THEN 1 ELSE 0 END), 0), \
    \COALESCE(SUM(CASE WHEN outcome = 'interrupted' THEN 1 ELSE 0 END), 0), \
    \COALESCE(SUM(CASE WHEN outcome IS NULL THEN 1 ELSE 0 END), 0), \
    \COALESCE(SUM(tokens_in), 0), \
    \COALESCE(SUM(tokens_out), 0), \
    \COALESCE(SUM(tokens_cache_read), 0), \
    \COALESCE(SUM(CASE WHEN tokens_in IS NULL THEN 1 ELSE 0 END), 0)"

{- | Spend/outcome summary over dispatches started at or after @since@
(all dispatches when @Nothing@). @dsMissingTokens@ counts runs whose
token columns are still NULL (pre-token-accounting-fix rows), so a
caller knows when the token sums are incomplete.
-}
getDispatchStats :: Connection -> Maybe Text -> IO DispatchStats
getDispatchStats conn mSince = do
    -- An aggregate with no GROUP BY yields exactly one row, empty table
    -- or not, so the zero-row case cannot arise.
    [stats] <- case mSince of
        Nothing ->
            query_ conn (Query $ "SELECT " <> statsCols <> " FROM dispatches")
        Just since ->
            query
                conn
                (Query $ "SELECT " <> statsCols <> " FROM dispatches WHERE started_at >= ?")
                (Only since)
    pure stats

-- | Terminal update: outcome, ended_at, optional merge sha, optional notes.
finishDispatch ::
    Connection ->
    -- | dispatch id
    Text ->
    DispatchOutcome ->
    -- | merge sha (success only)
    Maybe Text ->
    -- | notes
    Maybe Text ->
    IO ()
finishDispatch conn did outcome mSha mNotes =
    execute
        conn
        ( Query
            "UPDATE dispatches \
            \SET outcome = ?, ended_at = datetime('now'), \
            \    merge_sha = ?, notes = ? \
            \WHERE id = ?"
        )
        (outcome, mSha, mNotes, did)
