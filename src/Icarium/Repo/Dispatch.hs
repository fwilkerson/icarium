module Icarium.Repo.Dispatch (
    NewDispatch (..),
    insertDispatch,
    getDispatch,
    getDispatchesByPrefix,
    resolveDispatchId,
    listOpenDispatches,
    listDispatches,
    updateHeartbeat,
    setLastCommit,
    setPid,
    finishDispatch,
    logPathsOutsideRetention,
) where

import Data.Text (Text, unpack)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, Only (..), Query (..), execute, query, query_)

import Icarium.Types (Dispatch (..), DispatchOutcome, Effort)

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
    \notes, log_path"

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

escapeLike :: Text -> Text
escapeLike = T.concatMap esc
  where
    esc c
        | c `elem` ['%', '_', '\\'] = T.pack ['\\', c]
        | otherwise = T.singleton c

-- | Dispatches whose ULID starts with @prefix@.
getDispatchesByPrefix :: Connection -> Text -> IO [Dispatch]
getDispatchesByPrefix conn prefix =
    query
        conn
        (Query $ "SELECT " <> dispatchCols <> " FROM dispatches WHERE id LIKE ? ESCAPE '\\'")
        (Only (escapeLike prefix <> "%"))

{- | Resolve a user-supplied string to a canonical dispatch ULID.
Tries exact-id match first, then ULID prefix match.
-}
resolveDispatchId :: Connection -> Text -> IO (Either String Text)
resolveDispatchId conn input = do
    md <- getDispatch conn input
    case md of
        Just d -> pure (Right (dispatchId d))
        Nothing -> do
            ds <- getDispatchesByPrefix conn input
            case ds of
                [d] -> pure (Right (dispatchId d))
                [] -> pure (Left $ "dispatch not found: " <> T.unpack input)
                _ ->
                    pure
                        ( Left $
                            "ambiguous id: "
                                <> T.unpack input
                                <> " (matches: "
                                <> T.unpack (T.intercalate ", " (map dispatchId ds))
                                <> ")"
                        )

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

updateHeartbeat :: Connection -> Text -> IO ()
updateHeartbeat conn did =
    execute
        conn
        (Query "UPDATE dispatches SET heartbeat_at = datetime('now') WHERE id = ?")
        (Only did)

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

{- | Log paths for dispatches outside the N most recent by started_at DESC.
Only returns paths that are non-NULL in the DB.
-}
logPathsOutsideRetention :: Connection -> Int -> IO [FilePath]
logPathsOutsideRetention conn n = do
    rows <-
        query
            conn
            ( Query
                "SELECT log_path FROM dispatches \
                \WHERE log_path IS NOT NULL \
                \ORDER BY started_at DESC \
                \LIMIT -1 OFFSET ?"
            )
            (Only n)
    pure [unpack lp | Only lp <- rows]

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
