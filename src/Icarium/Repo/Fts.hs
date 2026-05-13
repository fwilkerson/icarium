module Icarium.Repo.Fts (
    indexEntry,
    removeEntry,
    reindexAll,
) where

import Data.Text (Text)
import Database.SQLite.Simple (Connection, Only (..), execute, execute_)

import Icarium.Types (NodeKind (..), nodeKindText)

-- | Insert or replace an FTS5 entry for a task or context body.
indexEntry :: Connection -> Text -> NodeKind -> Text -> Text -> IO ()
indexEntry conn eid kind title body = do
    execute conn "DELETE FROM body_fts WHERE id = ?" (Only eid)
    execute
        conn
        "INSERT INTO body_fts (id, kind, title, body) VALUES (?, ?, ?, ?)"
        (eid, nodeKindText kind, title, body)

removeEntry :: Connection -> Text -> IO ()
removeEntry conn eid =
    execute conn "DELETE FROM body_fts WHERE id = ?" (Only eid)

reindexAll :: Connection -> IO ()
reindexAll conn = do
    execute_ conn "DELETE FROM body_fts"
    execute_
        conn
        "INSERT INTO body_fts (id, kind, title, body) \
        \SELECT id, 'task', title, body FROM tasks"
    execute_
        conn
        "INSERT INTO body_fts (id, kind, title, body) \
        \SELECT id, 'context', title, body FROM context"
