{- | Column-shape concerns: the embedded base schema, @migrateDb@'s driving
of the chain, and the incremental migrations that can lose data.

Every per-migration test hand-writes the pre-migration shape. It cannot be
reached through @migrateDb@ from empty: 'migrations' starts with @Migration 1
applySchema@, which stamps @user_version@ at the current version, so the
incremental steps never run on a fresh DB.

Only migrations that /rebuild/ a table, transform rows, or settle the final
column shape are covered — a rebuild that drops a row or a column is silent,
and the constraint still looks right afterwards. A migration that adds a
column or drops a view either throws or succeeds; the base-schema and
column-layout tests already pin where it must land.

Read the SQL before deciding which class a migration is in. 0015 announces
itself as a state-CHECK widening and is in fact a full rebuild through a
backup table.
-}
module SchemaSpec (tests) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Either (isLeft)
import Data.List qualified as L
import Data.Text (Text)
import Database.SQLite.Simple (Connection, Only (..), Query (..), close, execute, execute_, fromOnly, open, query_, (:.) (..))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Db (dbSchemaVersion, migrateDb)
import Icarium.Migrations (Migration (..), migrations)
import Icarium.Migrations.Internal (mkSqlMigration)
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Edge qualified as RE
import Icarium.Repo.Task qualified as RT
import Icarium.Schema (execSql, schemaSql, schemaVersion)
import Icarium.Types

import TestHelpers

tests :: TestTree
tests =
    testGroup
        "schema"
        [ testGroup
            "base schema"
            [ testCase "deleting a context entry cascades to context_categories rows" testContextCategoriesCascade
            , testCase "no task_status view: listTasks is the one source of effective state" testNoTaskStatusView
            ]
        , testGroup
            "column layout"
            [ testCase "every column list names exactly its table's columns" testColsMatchTables
            , testCase "curationCols names columns the context_latest_curation view provides" testCurationColsInView
            ]
        , testGroup
            "migrateDb"
            [ testCase "base schema is already at schemaVersion; migrateDb is idempotent" testMigrateAdvances
            , testCase "bad SQL rolls back; user_version unchanged" testMigrateBadSqlRollback
            , testCase "user_version=0 with existing tables: stamps schemaVersion, no DDL re-run" testMigrateVersionZeroWithSchema
            , testCase "full migration chain runs from an empty DB to schemaVersion" testMigrateChainFromEmpty
            ]
        , testGroup
            "migrations"
            [ testCase "migration 13 backfills stale rows, drops the column" testMigration13Backfill
            , testCase "migration 14 widens the axis CHECK, keeping rows and FKs" testMigration14KindAxis
            , testCase "migration 15 rebuild carries every column, edges and triggers" testMigration15ReadyInteractive
            , testCase "migration 16 renames ready rows and recreates the view" testMigration16ReadyHeadless
            , testCase "migration 17 adds the column to a pre-17 context table" testMigration17AddsProvenance
            , testCase "migration 18 rebuild preserves existing edges" testMigration18PreservesEdges
            , testCase "migration 19 adds routing columns to a pre-19 tasks table" testMigration19AddsRouting
            ]
        ]

-- | The registered migration at @v@; absence is a test bug, not a failure mode.
migration :: Int -> Migration
migration v = case filter ((== v) . migrationVersion) migrations of
    (m : _) -> m
    [] -> error ("migration " <> show v <> " not registered")

-- =============================================================
-- Base schema
-- =============================================================

testContextCategoriesCascade :: IO ()
testContextCategoriesCascade = withTestDb $ \conn -> do
    domCat <- mkCat conn Domain "cli"
    kid <- mkContext conn "K" "body"
    RC.attachContextCategory conn kid domCat
    pre <- query_ conn "SELECT context_id FROM context_categories" :: IO [Only Text]
    length pre @?= 1
    execute conn (Query "DELETE FROM context WHERE id = ?") (Only kid)
    post <- query_ conn "SELECT context_id FROM context_categories" :: IO [Only Text]
    length post @?= 0

testNoTaskStatusView :: IO ()
testNoTaskStatusView = withBaseTestDb $ \conn -> do
    views <- query_ conn "SELECT name FROM sqlite_master WHERE type='view'" :: IO [Only Text]
    assertBool "task_status view is gone" (Only ("task_status" :: Text) `notElem` views)

-- =============================================================
-- Column layout
-- =============================================================

-- | Column names of a table or view, in declaration order.
tableColumns :: Connection -> Text -> IO [Text]
tableColumns conn t =
    map fromOnly
        <$> query_ conn (Query $ "SELECT name FROM pragma_table_info('" <> t <> "')")

{- | Half of the cols/'FromRow' invariant: the names must be the table's
own. Compared as a /set/ — 'taskCols' is deliberately ordered to match the
record, not the DDL. The other half (order matches the @field@ chain) is
the per-entity round-trip in the repo specs.

Equality, not subset, so that a new DDL column fails here and forces the
call: carry it on the record, or list it as write-only below.
-}
testColsMatchTables :: IO ()
testColsMatchTables = withBaseTestDb $ \conn ->
    mapM_ (check conn) layouts
  where
    -- (table, cols, columns the record deliberately does not carry)
    layouts =
        [ ("tasks", taskCols, [])
        , -- Provenance stamped at insert and read only by the bodies sweep;
          -- 'Context' has no field for it.
          ("context", contextCols, ["source_dispatch_id"])
        , ("edges", edgeCols, [])
        , ("dispatches", dispatchCols, [])
        , ("context_curation", curationCols, [])
        ]
    check conn (table, cols, writeOnly) = do
        actual <- tableColumns conn table
        (table, L.sort (cols <> writeOnly)) @?= (table, L.sort actual)

{- | @context_latest_curation@ is @SELECT cc.*@ over @context_curation@, so
it may carry columns no list names; only a missing one is a bug. Hence
subset, not equality.
-}
testCurationColsInView :: IO ()
testCurationColsInView = withBaseTestDb $ \conn -> do
    actual <- tableColumns conn "context_latest_curation"
    let missing = filter (`notElem` actual) curationCols
    missing @?= []

-- =============================================================
-- migrateDb
-- =============================================================

testMigrateAdvances :: IO ()
testMigrateAdvances = withBaseTestDb $ \conn -> do
    v0 <- dbSchemaVersion conn
    v0 @?= fromIntegral schemaVersion
    migrateDb conn
    v1 <- dbSchemaVersion conn
    v1 @?= fromIntegral schemaVersion

testMigrateBadSqlRollback :: IO ()
testMigrateBadSqlRollback = withBaseTestDb $ \conn -> do
    v0 <- dbSchemaVersion conn
    let m = mkSqlMigration 99 "THIS IS NOT VALID SQL AT ALL"
    result <- try (migrationUp m conn) :: IO (Either SomeException ())
    assertBool "bad migration should throw" (either (const True) (const False) result)
    v1 <- dbSchemaVersion conn
    v1 @?= v0

-- Simulates a DB created by an external restore script: schema applied but
-- user_version left at 0 (SQLite default). migrateDb should stamp the version
-- without re-running CREATE TABLE (which would fail with "table already exists").
testMigrateVersionZeroWithSchema :: IO ()
testMigrateVersionZeroWithSchema = do
    conn <- open ":memory:"
    execSql conn schemaSql
    v0 <- dbSchemaVersion conn
    v0 @?= 0
    migrateDb conn
    v1 <- dbSchemaVersion conn
    v1 @?= fromIntegral schemaVersion
    close conn

-- Fresh file with no schema applied: migrateDb must run the whole chain from
-- migration 1, landing at schemaVersion with a working task surface.
testMigrateChainFromEmpty :: IO ()
testMigrateChainFromEmpty =
    withSystemTempFile "icarium-chain.db" $ \fp h -> do
        hClose h
        conn <- open fp
        migrateDb conn
        v <- dbSchemaVersion conn
        v @?= fromIntegral schemaVersion
        tid <- mkTaskRow conn "Chain task"
        Just t <- RT.getTask conn tid
        taskId t @?= tid
        close conn

-- =============================================================
-- Per-migration tests
-- =============================================================

testMigration13Backfill :: IO ()
testMigration13Backfill = do
    conn <- open ":memory:"
    execSql
        conn
        "CREATE TABLE context (\
        \id TEXT PRIMARY KEY, title TEXT NOT NULL, body TEXT NOT NULL DEFAULT '', \
        \stale INTEGER NOT NULL DEFAULT 0 CHECK (stale IN (0,1)), \
        \created_at TEXT NOT NULL DEFAULT (datetime('now')), \
        \updated_at TEXT NOT NULL DEFAULT (datetime('now'))); \
        \CREATE VIEW stale_context AS SELECT k.* FROM context k WHERE k.stale = 1;"
    execute_
        conn
        "INSERT INTO context (id, title, stale, updated_at) VALUES \
        \('01MIGSTALE0000000000000001', 'Old stale', 1, '2026-01-01 00:00:00'), \
        \('01MIGLIVE00000000000000002', 'Live', 0, '2026-01-01 00:00:00')"
    migrationUp (migration 13) conn
    cols <- query_ conn "SELECT name FROM pragma_table_info('context')" :: IO [Only Text]
    assertBool "stale column dropped" (Only ("stale" :: Text) `notElem` cols)
    evs <- query_ conn "SELECT id, context_id, disposition FROM context_curation" :: IO [(Text, Text, Text)]
    evs @?= [("backfill-01MIGSTALE0000000000000001", "01MIGSTALE0000000000000001", "stale")]
    retired <- query_ conn "SELECT context_id FROM retired_context" :: IO [Only Text]
    retired @?= [Only "01MIGSTALE0000000000000001"]
    close conn

{- | Migration 14 rebuilds `categories` to widen its axis CHECK. The rebuild
must keep existing rows, keep the child link tables pointing at the new
table (FK cascade still fires), and accept the new axis.
-}
testMigration14KindAxis :: IO ()
testMigration14KindAxis = withBaseTestDb $ \conn -> do
    -- Restore the v13 shape verbatim: CHECK without 'kind'. Rebuilt from
    -- scratch (children first) so no ALTER-rename semantics are involved in
    -- the fixture itself.
    execSql
        conn
        "DROP TABLE task_categories;\n\
        \DROP TABLE context_categories;\n\
        \DROP TABLE categories;\n\
        \CREATE TABLE categories (\n\
        \  id   TEXT PRIMARY KEY,\n\
        \  axis TEXT NOT NULL CHECK (axis IN ('domain','discipline')),\n\
        \  name TEXT NOT NULL,\n\
        \  UNIQUE (axis, name));\n\
        \CREATE TABLE task_categories (\n\
        \  task_id     TEXT NOT NULL REFERENCES tasks(id) ON DELETE CASCADE,\n\
        \  category_id TEXT NOT NULL REFERENCES categories(id) ON DELETE CASCADE,\n\
        \  PRIMARY KEY (task_id, category_id));\n\
        \CREATE TABLE context_categories (\n\
        \  context_id  TEXT NOT NULL REFERENCES context(id) ON DELETE CASCADE,\n\
        \  category_id TEXT NOT NULL REFERENCES categories(id) ON DELETE CASCADE,\n\
        \  PRIMARY KEY (context_id, category_id));"
    tid <- mkTaskRow conn "Tagged"
    cid <- RC.insertCategory conn Domain "cli"
    RC.attachTaskCategory conn tid cid

    migrationUp (migration 14) conn

    survivors <- RC.listCategories conn Nothing
    map (\c -> (categoryAxis c, categoryName c)) survivors @?= [(Domain, "cli")]
    attached <- RC.taskCategoriesFor conn tid
    map categoryName attached @?= ["cli"]

    -- The widened CHECK accepts the workflow axis.
    kid <- RC.insertCategory conn Kind "bug"
    RC.attachTaskCategory conn tid kid
    both <- RC.taskCategoriesFor conn tid
    length both @?= 2

    -- Child FK still resolves to the rebuilt table: deleting the task
    -- cascades its attachments away.
    void $ RT.deleteTask conn tid
    orphaned <- RC.taskCategoriesFor conn tid
    null orphaned @?= True

{- | Migration 15 rebuilds `tasks` through an unconstrained backup table to
widen its state CHECK. The @INSERT ... SELECT@ names its columns explicitly,
so a column dropped from that list loses its data silently — hence a fixture
row with every nullable field populated, compared whole.
-}
testMigration15ReadyInteractive :: IO ()
testMigration15ReadyInteractive = withBaseTestDb $ \conn -> do
    -- Upgrading connections do not enable FK enforcement (spec/schema.sql);
    -- applySchema turned it on for this one, so match production before the
    -- rebuild drops `tasks`.
    execute_ conn "PRAGMA foreign_keys = OFF"
    -- Restore the v14 shape: state CHECK without 'ready_interactive'.
    execSql
        conn
        "DROP VIEW ready_tasks;\n\
        \DROP TABLE tasks;\n\
        \CREATE TABLE tasks (\n\
        \  id TEXT PRIMARY KEY,\n\
        \  title TEXT NOT NULL,\n\
        \  body TEXT NOT NULL DEFAULT '',\n\
        \  state TEXT NOT NULL DEFAULT 'planned'\n\
        \    CHECK (state IN ('idea','planned','ready','in_progress','done',\n\
        \                     'blocked','abandoned')),\n\
        \  priority INTEGER,\n\
        \  block_reason TEXT,\n\
        \  no_commit INTEGER NOT NULL DEFAULT 0 CHECK (no_commit IN (0,1)),\n\
        \  claimed_by TEXT,\n\
        \  claimed_at TEXT,\n\
        \  created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
        \  updated_at TEXT NOT NULL DEFAULT (datetime('now')));\n\
        \CREATE INDEX tasks_state_idx ON tasks(state);\n\
        \CREATE VIEW ready_tasks AS SELECT * FROM tasks WHERE 0;\n\
        \INSERT INTO tasks (id,title,state) VALUES ('01AAA','Dependency','ready');\n\
        \INSERT INTO tasks (id,title,body,state,priority,block_reason,no_commit,\n\
        \                   claimed_by,claimed_at,created_at,updated_at)\n\
        \  VALUES ('01BBB','Dependent','kept body','ready',7,'waiting on A',1,\n\
        \          'agent-3','2026-01-02 03:04:05','2026-01-01 00:00:00',\n\
        \          '2026-01-01 00:00:00');\n\
        \INSERT INTO edges (id,kind,src_kind,src_id,dst_kind,dst_id)\n\
        \  VALUES ('01EDGE','depends_on','task','01BBB','task','01AAA');"

    migrationUp (migration 15) conn

    -- Every column of the fully-populated row, not just its id: a name missing
    -- from the migration's INSERT list nulls that field without complaint.
    carried <-
        query_
            conn
            "SELECT id, title, body, state, priority, block_reason, no_commit, \
            \claimed_by, claimed_at, created_at, updated_at \
            \FROM tasks WHERE id = '01BBB'" ::
            IO [(Text, Text, Text, Text, Maybe Int, Maybe Text, Int) :. (Maybe Text, Maybe Text, Text, Text)]
    carried
        @?= [ ("01BBB", "Dependent", "kept body", "ready", Just 7, Just "waiting on A", 1)
                :. (Just "agent-3", Just "2026-01-02 03:04:05", "2026-01-01 00:00:00", "2026-01-01 00:00:00")
            ]
    ids <- query_ conn "SELECT id FROM tasks ORDER BY id" :: IO [Only Text]
    map fromOnly ids @?= ["01AAA", "01BBB"]
    edges <- query_ conn "SELECT id FROM edges" :: IO [Only Text]
    map fromOnly edges @?= ["01EDGE"]

    -- The new state is accepted, and the recreated view keeps the deps gate.
    execute_ conn "INSERT INTO tasks (id,title,state) VALUES ('01CCC','Interactive','ready_interactive')"
    inView <- query_ conn "SELECT id FROM ready_tasks ORDER BY id" :: IO [Only Text]
    map fromOnly inView @?= ["01AAA", "01CCC"]

    -- The edge-cascade trigger went with the old table and must be back.
    execute_ conn "DELETE FROM tasks WHERE id='01AAA'"
    afterCascade <- query_ conn "SELECT id FROM edges" :: IO [Only Text]
    map fromOnly afterCascade @?= []

{- | Migration 16 renames `ready` to `ready_headless`. The rename must not
cost a task its identity: categories and edges hang off `tasks` through the
rebuild, and the recreated view must gate on the new name.
-}
testMigration16ReadyHeadless :: IO ()
testMigration16ReadyHeadless = withBaseTestDb $ \conn -> do
    execute_ conn "PRAGMA foreign_keys = OFF"
    -- Restore the v15 shape: bare 'ready' in the CHECK and in the view.
    execSql
        conn
        "DROP VIEW ready_tasks;\n\
        \DROP TABLE tasks;\n\
        \CREATE TABLE tasks (\n\
        \  id TEXT PRIMARY KEY,\n\
        \  title TEXT NOT NULL,\n\
        \  body TEXT NOT NULL DEFAULT '',\n\
        \  state TEXT NOT NULL DEFAULT 'planned'\n\
        \    CHECK (state IN ('idea','planned','ready','ready_interactive',\n\
        \                     'in_progress','done','blocked','abandoned')),\n\
        \  priority INTEGER,\n\
        \  block_reason TEXT,\n\
        \  no_commit INTEGER NOT NULL DEFAULT 0 CHECK (no_commit IN (0,1)),\n\
        \  claimed_by TEXT,\n\
        \  claimed_at TEXT,\n\
        \  created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
        \  updated_at TEXT NOT NULL DEFAULT (datetime('now')));\n\
        \CREATE INDEX tasks_state_idx ON tasks(state);\n\
        \CREATE VIEW ready_tasks AS SELECT * FROM tasks WHERE 0;\n\
        \INSERT INTO tasks (id,title,state,priority) VALUES ('01AAA','Headless','ready',7);\n\
        \INSERT INTO tasks (id,title,state) VALUES ('01BBB','Interactive','ready_interactive');\n\
        \INSERT INTO tasks (id,title,state) VALUES ('01CCC','Planned','planned');\n\
        \INSERT INTO categories (id,axis,name) VALUES ('01CAT','domain','cli');\n\
        \INSERT INTO task_categories (task_id,category_id) VALUES ('01AAA','01CAT');\n\
        \INSERT INTO edges (id,kind,src_kind,src_id,dst_kind,dst_id)\n\
        \  VALUES ('01EDGE','depends_on','task','01BBB','task','01AAA');"

    migrationUp (migration 16) conn

    -- Every row survives, and only the ready ones are renamed.
    states <- query_ conn "SELECT id, state FROM tasks ORDER BY id" :: IO [(Text, Text)]
    states
        @?= [ ("01AAA", "ready_headless")
            , ("01BBB", "ready_interactive")
            , ("01CCC", "planned")
            ]
    priorities <- query_ conn "SELECT priority FROM tasks WHERE id='01AAA'" :: IO [Only (Maybe Int)]
    map fromOnly priorities @?= [Just 7]

    -- Categories and edges hang off the rebuilt table, not the dropped one.
    cats <- query_ conn "SELECT task_id FROM task_categories" :: IO [Only Text]
    map fromOnly cats @?= ["01AAA"]
    edges <- query_ conn "SELECT id FROM edges" :: IO [Only Text]
    map fromOnly edges @?= ["01EDGE"]

    -- The widened CHECK takes the new name; the old one is gone for good.
    execute_ conn "INSERT INTO tasks (id,title,state) VALUES ('01DDD','New','ready_headless')"
    rejected <-
        try (execute_ conn "INSERT INTO tasks (id,title,state) VALUES ('01EEE','Old','ready')") ::
            IO (Either SomeException ())
    assertBool "bare ready is no longer a legal state" (isLeft rejected)

    -- The recreated view gates on the new name and still applies the deps gate.
    inView <- query_ conn "SELECT id FROM ready_tasks ORDER BY id" :: IO [Only Text]
    map fromOnly inView @?= ["01AAA", "01DDD"]

    -- The edge-cascade trigger went with the old table and must be back.
    execute_ conn "DELETE FROM tasks WHERE id='01AAA'"
    afterCascade <- query_ conn "SELECT id FROM edges" :: IO [Only Text]
    map fromOnly afterCascade @?= []

testMigration17AddsProvenance :: IO ()
testMigration17AddsProvenance = withBaseTestDb $ \conn -> do
    execSql
        conn
        "DROP TABLE context;\n\
        \CREATE TABLE context (\n\
        \  id TEXT PRIMARY KEY,\n\
        \  title TEXT NOT NULL,\n\
        \  body TEXT NOT NULL DEFAULT '',\n\
        \  created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
        \  updated_at TEXT NOT NULL DEFAULT (datetime('now')));\n\
        \INSERT INTO context (id, title, body) VALUES ('01PRE17', 'Pre-migration', 'kept');"

    migrationUp (migration 17) conn

    -- The row survives, and reads as belonging to no run rather than vanishing.
    rows <-
        query_
            conn
            "SELECT id, body, source_dispatch_id FROM context" ::
            IO [(Text, Text, Maybe Text)]
    rows @?= [("01PRE17", "kept", Nothing)]

    -- Last migration to touch context: an upgraded DB must land on the same
    -- shape the column list expects. See the note in testMigration19AddsRouting.
    upgraded <- tableColumns conn "context"
    L.sort upgraded @?= L.sort (contextCols <> ["source_dispatch_id"])

{- | Migration 18 rebuilds the edges table to widen a CHECK. A rebuild that
drops rows is silent — the constraint still looks right afterwards.
-}
testMigration18PreservesEdges :: IO ()
testMigration18PreservesEdges = withTestDb $ \c -> do
    parent <- mkTaskRow c "Parent"
    child <- mkTaskRow c "Child"
    cx <- mkContext c "Learning" ""
    dep <- RE.insertEdge c DependsOn TaskNode child TaskNode parent
    ref <- RE.insertEdge c References TaskNode parent ContextNode cx
    der <- RE.insertEdge c DerivedFrom TaskNode child TaskNode parent

    migrationUp (migration 18) c

    survivors <- RE.listEdges c Nothing Nothing Nothing
    let allIds = map edgeId survivors
    mapM_ (\e -> assertBool "edge survived rebuild" (e `elem` allIds)) [dep, ref, der]

{- | Pre-19 tasks keep their rows and read as "inherit the config defaults",
and the new CHECK still rejects an effort outside the enum.
-}
testMigration19AddsRouting :: IO ()
testMigration19AddsRouting = withBaseTestDb $ \conn -> do
    execSql
        conn
        "DROP TABLE tasks;\n\
        \CREATE TABLE tasks (\n\
        \  id TEXT PRIMARY KEY,\n\
        \  title TEXT NOT NULL,\n\
        \  body TEXT NOT NULL DEFAULT '',\n\
        \  state TEXT NOT NULL DEFAULT 'planned',\n\
        \  priority INTEGER,\n\
        \  block_reason TEXT,\n\
        \  no_commit INTEGER NOT NULL DEFAULT 0,\n\
        \  claimed_by TEXT,\n\
        \  claimed_at TEXT,\n\
        \  created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
        \  updated_at TEXT NOT NULL DEFAULT (datetime('now')));\n\
        \INSERT INTO tasks (id, title, body) VALUES ('01PRE19', 'Pre-migration', 'kept');"

    migrationUp (migration 19) conn

    rows <-
        query_ conn "SELECT id, body, model, effort FROM tasks" ::
            IO [(Text, Text, Maybe Text, Maybe Text)]
    rows @?= [("01PRE19", "kept", Nothing, Nothing)]

    bad <-
        try (execute_ conn "UPDATE tasks SET effort = 'turbo' WHERE id = '01PRE19'") ::
            IO (Either SomeException ())
    assertBool "effort CHECK rejects a value outside the enum" (isLeft bad)

    -- 19 is the last migration to touch tasks, so an upgraded DB lands here.
    -- This is the only comparison that is not circular: the hand-written
    -- pre-19 shape plus the migration is a construction independent of
    -- schema.sql, which migrateDb-from-empty is not (migration 1 *is*
    -- applySchema, so a "migrated" fresh DB is the schema verbatim).
    upgraded <- tableColumns conn "tasks"
    L.sort upgraded @?= L.sort taskCols
