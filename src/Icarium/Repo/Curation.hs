module Icarium.Repo.Curation (
    insertCuration,
    latestCuration,
    retiredContextIds,
    curationQueue,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (
    Connection,
    Only (..),
    Query (..),
    SQLData (..),
    execute,
    query,
    query_,
 )

import Icarium.Id (newId)
import Icarium.Repo.Internal (inClause, qualified)
import Icarium.Types (Context (..), CurationEvent (..), Disposition, contextCols, curationCols)

-- | Record one curation event. Append-only: nothing is ever updated.
insertCuration :: Connection -> Text -> Disposition -> Maybe Text -> Maybe Text -> IO Text
insertCuration conn cxid disp artifact note = do
    eid <- newId
    execute
        conn
        ( Query $
            "INSERT INTO context_curation ("
                <> qualified "" curationCols
                <> ") \
                   \VALUES (?, ?, ?, ?, ?, datetime('now'))"
        )
        (eid, cxid, disp, artifact, note)
    pure eid

-- | The winning (latest) curation event for an entry, if any.
latestCuration :: Connection -> Text -> IO (Maybe CurationEvent)
latestCuration conn cxid = do
    rows <-
        query
            conn
            ( Query $
                "SELECT " <> qualified "" curationCols <> " FROM context_latest_curation WHERE context_id = ?"
            )
            (Only cxid)
    pure $ case rows of
        (e : _) -> Just e
        [] -> Nothing

-- | Which of the given entries are retired (latest disposition not 'keep').
retiredContextIds :: Connection -> [Text] -> IO [Text]
retiredContextIds _ [] = pure []
retiredContextIds conn ids =
    map fromOnly
        <$> query
            conn
            ( Query $
                "SELECT context_id FROM retired_context WHERE context_id IN " <> inClause ids
            )
            (map SQLText ids)

{- | Entries awaiting curation, oldest first, paired with their latest
event. Always includes never-curated entries (event = Nothing);
@mOlderThanDays@ adds re-curation candidates whose latest event is at
least that many days old.
-}
curationQueue :: Connection -> Maybe Int -> IO [(Context, Maybe CurationEvent)]
curationQueue conn mOlderThanDays = do
    never <- query_ conn (Query $ selectCtx <> " WHERE " <> neverClause <> order)
    aged <- case mOlderThanDays of
        Nothing -> pure []
        Just days -> do
            cxs <-
                query
                    conn
                    (Query $ selectCtx <> " WHERE " <> agedClause <> order)
                    (Only (T.pack ("-" <> show days <> " days")))
            mapM (\cx -> (,) cx <$> latestCuration conn (contextId cx)) cxs
    pure (map (,Nothing) never <> aged)
  where
    selectCtx = "SELECT " <> qualified "" contextCols <> " FROM context"
    neverClause = "id NOT IN (SELECT context_id FROM context_curation)"
    agedClause =
        "id IN (SELECT context_id FROM context_latest_curation \
        \WHERE created_at <= datetime('now', ?))"
    order = " ORDER BY created_at ASC"
