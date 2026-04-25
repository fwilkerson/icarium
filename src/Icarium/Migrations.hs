module Icarium.Migrations
    ( Migration (..)
    , migrations
    ) where

import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Database.SQLite.Simple          (Connection)
import qualified Database.SQLite.Simple.Internal as Internal
import qualified Database.SQLite3                as Direct

-- | A single forward migration. @migrationVersion@ is the schema version
-- produced by running @migrationUp@. Each @migrationUp@ manages its own
-- transaction and stamps @PRAGMA user_version@ atomically.
data Migration = Migration
    { migrationVersion :: Int
    , migrationUp      :: Connection -> IO ()
    }

-- | Ordered list of all schema migrations, oldest first.
-- To add a new migration: append @Migration N migrate_N_minus_1_to_N@.
migrations :: [Migration]
migrations =
    [ Migration 2 migrate_1_to_2
    ]

-- ---------------------------------------------------------------------------
-- 1 → 2: Add 'in_progress' to the tasks.state CHECK constraint.
-- ---------------------------------------------------------------------------
-- SQLite does not support ALTER COLUMN, so we use the standard
-- create-copy-drop-rename pattern. Foreign keys must be disabled outside the
-- transaction (PRAGMA foreign_keys is a no-op inside a transaction) to allow
-- DROP TABLE tasks while task_categories / dispatches reference it.
-- ---------------------------------------------------------------------------

migrate_1_to_2 :: Connection -> IO ()
migrate_1_to_2 conn =
    Direct.exec (Internal.connectionHandle conn) migrate_1_to_2_sql

-- SQLite (3.26+) validates all schema objects when ALTER TABLE RENAME runs.
-- Between DROP TABLE tasks and the rename, any view or trigger whose body
-- references 'tasks' is broken, causing RENAME to fail.  We drop those
-- dependents before touching the table and recreate them afterward.
--
-- Views that reference tasks: task_status, ready_tasks
-- Triggers ON edges that SELECT from tasks:
--   edges_src_task_exists, edges_dst_task_exists
-- Triggers ON tasks (auto-dropped with the table):
--   tasks_touch, edges_cascade_task_delete
migrate_1_to_2_sql :: Text
migrate_1_to_2_sql = T.unlines
    [ "PRAGMA foreign_keys = OFF;"
    , "BEGIN;"
    -- Drop all schema objects that reference 'tasks' and aren't auto-dropped.
    , "DROP VIEW  IF EXISTS task_status;"
    , "DROP VIEW  IF EXISTS ready_tasks;"
    , "DROP TRIGGER IF EXISTS edges_src_task_exists;"
    , "DROP TRIGGER IF EXISTS edges_dst_task_exists;"
    -- Rebuild tasks table with the updated CHECK constraint.
    , "CREATE TABLE tasks_new ("
    , "    id           TEXT PRIMARY KEY,"
    , "    title        TEXT NOT NULL,"
    , "    body         TEXT NOT NULL DEFAULT '',"
    , "    state        TEXT NOT NULL DEFAULT 'planned'"
    , "                 CHECK (state IN ('idea','planned','ready','in_progress','done',"
    , "                                  'blocked','abandoned')),"
    , "    priority     INTEGER,"
    , "    block_reason TEXT,"
    , "    created_at   TEXT NOT NULL DEFAULT (datetime('now')),"
    , "    updated_at   TEXT NOT NULL DEFAULT (datetime('now'))"
    , ");"
    , "INSERT INTO tasks_new SELECT * FROM tasks;"
    , "DROP TABLE tasks;"
    , "ALTER TABLE tasks_new RENAME TO tasks;"
    -- Recreate triggers that were ON tasks (dropped with the table).
    , "CREATE TRIGGER tasks_touch AFTER UPDATE ON tasks"
    , "BEGIN"
    , "    UPDATE tasks SET updated_at = datetime('now') WHERE id = NEW.id;"
    , "END;"
    , "CREATE TRIGGER edges_cascade_task_delete AFTER DELETE ON tasks"
    , "BEGIN"
    , "    DELETE FROM edges WHERE (src_kind='task' AND src_id=OLD.id)"
    , "                         OR (dst_kind='task' AND dst_id=OLD.id);"
    , "END;"
    -- Recreate triggers ON edges that SELECT from tasks.
    , "CREATE TRIGGER edges_src_task_exists"
    , "BEFORE INSERT ON edges"
    , "WHEN NEW.src_kind = 'task'"
    , "BEGIN"
    , "    SELECT RAISE(ABORT, 'edge src task missing')"
    , "    WHERE NOT EXISTS (SELECT 1 FROM tasks WHERE id = NEW.src_id);"
    , "END;"
    , "CREATE TRIGGER edges_dst_task_exists"
    , "BEFORE INSERT ON edges"
    , "WHEN NEW.dst_kind = 'task'"
    , "BEGIN"
    , "    SELECT RAISE(ABORT, 'edge dst task missing')"
    , "    WHERE NOT EXISTS (SELECT 1 FROM tasks WHERE id = NEW.dst_id);"
    , "END;"
    -- Recreate views that reference tasks.
    , "CREATE VIEW task_status AS"
    , "SELECT"
    , "    t.id,"
    , "    t.title,"
    , "    t.priority,"
    , "    CASE"
    , "        WHEN EXISTS (SELECT 1 FROM dispatches d"
    , "                     WHERE d.task_id = t.id AND d.outcome IS NULL)"
    , "        THEN 'in_progress'"
    , "        ELSE t.state"
    , "    END AS state,"
    , "    t.created_at,"
    , "    t.updated_at"
    , "FROM tasks t;"
    , "CREATE VIEW ready_tasks AS"
    , "SELECT t.*"
    , "FROM tasks t"
    , "WHERE t.state = 'ready'"
    , "  AND NOT EXISTS ("
    , "      SELECT 1"
    , "      FROM edges e"
    , "      JOIN tasks dep ON dep.id = e.dst_id"
    , "      WHERE e.kind = 'depends_on'"
    , "        AND e.src_kind = 'task' AND e.src_id = t.id"
    , "        AND e.dst_kind = 'task'"
    , "        AND dep.state <> 'done'"
    , "  )"
    , "  AND NOT EXISTS ("
    , "      SELECT 1 FROM dispatches d"
    , "      WHERE d.task_id = t.id AND d.outcome IS NULL"
    , "  )"
    , "ORDER BY COALESCE(t.priority, 0) DESC, t.created_at ASC;"
    , "PRAGMA user_version = 2;"
    , "COMMIT;"
    , "PRAGMA foreign_keys = ON;"
    ]
