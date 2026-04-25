-- icarium schema (draft — review before implementation)
-- SQLite; all IDs are ULID TEXT; all timestamps are ISO8601 TEXT in UTC.

PRAGMA foreign_keys = ON;
PRAGMA journal_mode = WAL;

-- =============================================================
-- Core entities
-- =============================================================

CREATE TABLE tasks (
    id          TEXT PRIMARY KEY,                   -- ULID
    title       TEXT NOT NULL,
    body        TEXT NOT NULL DEFAULT '',           -- markdown
    state       TEXT NOT NULL DEFAULT 'planned'
                CHECK (state IN ('idea','planned','ready','in_progress','done',
                                 'blocked','abandoned')),
    -- 'in_progress' is stored; the dispatch program sets it before invoking
    -- the agent, then transitions to 'done' or 'blocked' after gates pass.
    priority    INTEGER,                             -- NULL = default
    block_reason TEXT,                               -- structured text when state='blocked'
    created_at  TEXT NOT NULL DEFAULT (datetime('now')),
    updated_at  TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE knowledge (
    id          TEXT PRIMARY KEY,
    title       TEXT NOT NULL,
    body        TEXT NOT NULL DEFAULT '',            -- markdown
    stale       INTEGER NOT NULL DEFAULT 0 CHECK (stale IN (0,1)),
    created_at  TEXT NOT NULL DEFAULT (datetime('now')),
    updated_at  TEXT NOT NULL DEFAULT (datetime('now'))
);

-- =============================================================
-- Categories (two independent axes, user-defined vocabulary)
-- =============================================================

CREATE TABLE categories (
    id    TEXT PRIMARY KEY,
    axis  TEXT NOT NULL CHECK (axis IN ('domain','discipline')),
    name  TEXT NOT NULL,
    UNIQUE (axis, name)
);

CREATE TABLE task_categories (
    task_id     TEXT NOT NULL REFERENCES tasks(id) ON DELETE CASCADE,
    category_id TEXT NOT NULL REFERENCES categories(id) ON DELETE CASCADE,
    PRIMARY KEY (task_id, category_id)
);

CREATE TABLE knowledge_categories (
    knowledge_id TEXT NOT NULL REFERENCES knowledge(id) ON DELETE CASCADE,
    category_id  TEXT NOT NULL REFERENCES categories(id) ON DELETE CASCADE,
    PRIMARY KEY (knowledge_id, category_id)
);

-- =============================================================
-- Typed edges between nodes
--   depends_on   : task -> task
--   references   : task -> knowledge
--   derived_from : knowledge -> task | knowledge
--   supersedes   : knowledge -> knowledge
-- Kind/endpoint rules enforced by CHECK + triggers (below).
-- =============================================================

CREATE TABLE edges (
    id         TEXT PRIMARY KEY,
    kind       TEXT NOT NULL
               CHECK (kind IN ('depends_on','references',
                               'derived_from','supersedes')),
    src_kind   TEXT NOT NULL CHECK (src_kind IN ('task','knowledge')),
    src_id     TEXT NOT NULL,
    dst_kind   TEXT NOT NULL CHECK (dst_kind IN ('task','knowledge')),
    dst_id     TEXT NOT NULL,
    created_at TEXT NOT NULL DEFAULT (datetime('now')),
    UNIQUE (kind, src_kind, src_id, dst_kind, dst_id),

    -- Endpoint typing rules per edge kind
    CHECK (
        (kind = 'depends_on'   AND src_kind = 'task'      AND dst_kind = 'task') OR
        (kind = 'references'   AND src_kind = 'task'      AND dst_kind = 'knowledge') OR
        (kind = 'derived_from' AND src_kind = 'knowledge' AND dst_kind IN ('task','knowledge')) OR
        (kind = 'supersedes'   AND src_kind = 'knowledge' AND dst_kind = 'knowledge')
    ),

    -- No self-edges
    CHECK (NOT (src_kind = dst_kind AND src_id = dst_id))
);

CREATE INDEX edges_src_idx ON edges(src_kind, src_id);
CREATE INDEX edges_dst_idx ON edges(dst_kind, dst_id);
CREATE INDEX edges_kind_idx ON edges(kind);

-- Referential integrity for polymorphic endpoints (SQLite can't express
-- a FK to a union of tables, so we use triggers).
CREATE TRIGGER edges_src_task_exists
BEFORE INSERT ON edges
WHEN NEW.src_kind = 'task'
BEGIN
    SELECT RAISE(ABORT, 'edge src task missing')
    WHERE NOT EXISTS (SELECT 1 FROM tasks WHERE id = NEW.src_id);
END;

CREATE TRIGGER edges_src_knowledge_exists
BEFORE INSERT ON edges
WHEN NEW.src_kind = 'knowledge'
BEGIN
    SELECT RAISE(ABORT, 'edge src knowledge missing')
    WHERE NOT EXISTS (SELECT 1 FROM knowledge WHERE id = NEW.src_id);
END;

CREATE TRIGGER edges_dst_task_exists
BEFORE INSERT ON edges
WHEN NEW.dst_kind = 'task'
BEGIN
    SELECT RAISE(ABORT, 'edge dst task missing')
    WHERE NOT EXISTS (SELECT 1 FROM tasks WHERE id = NEW.dst_id);
END;

CREATE TRIGGER edges_dst_knowledge_exists
BEFORE INSERT ON edges
WHEN NEW.dst_kind = 'knowledge'
BEGIN
    SELECT RAISE(ABORT, 'edge dst knowledge missing')
    WHERE NOT EXISTS (SELECT 1 FROM knowledge WHERE id = NEW.dst_id);
END;

-- Cascade delete: when a node goes away, drop its edges on either side.
CREATE TRIGGER edges_cascade_task_delete
AFTER DELETE ON tasks
BEGIN
    DELETE FROM edges WHERE (src_kind='task' AND src_id=OLD.id)
                         OR (dst_kind='task' AND dst_id=OLD.id);
END;

CREATE TRIGGER edges_cascade_knowledge_delete
AFTER DELETE ON knowledge
BEGIN
    DELETE FROM edges WHERE (src_kind='knowledge' AND src_id=OLD.id)
                         OR (dst_kind='knowledge' AND dst_id=OLD.id);
END;

-- =============================================================
-- Dispatches — one row per invocation of the headless agent.
-- A task is "in progress" iff it has a dispatch with outcome IS NULL.
-- =============================================================

CREATE TABLE dispatches (
    id             TEXT PRIMARY KEY,                 -- ULID; also used as branch suffix
    task_id        TEXT NOT NULL REFERENCES tasks(id) ON DELETE CASCADE,
    branch         TEXT NOT NULL,                    -- e.g. 'dispatch/<ulid>'
    base_branch    TEXT NOT NULL,                    -- integration branch at cut time
    base_sha       TEXT NOT NULL,                    -- HEAD sha of base at cut
    pid            INTEGER,
    model          TEXT NOT NULL,
    effort         TEXT NOT NULL CHECK (effort IN ('low','medium','high')),
    started_at     TEXT NOT NULL DEFAULT (datetime('now')),
    heartbeat_at   TEXT NOT NULL DEFAULT (datetime('now')),
    ended_at       TEXT,
    outcome        TEXT CHECK (outcome IN ('success','failure','interrupted')),
    merge_sha      TEXT,                             -- FF-merge sha on success
    last_commit    TEXT,                             -- latest commit on dispatch branch
    notes          TEXT,                             -- freeform: failure reason, etc.
    log_path       TEXT                              -- path to jsonl event log
);

CREATE INDEX dispatches_task_idx      ON dispatches(task_id);
CREATE INDEX dispatches_open_idx      ON dispatches(outcome) WHERE outcome IS NULL;
CREATE INDEX dispatches_heartbeat_idx ON dispatches(heartbeat_at) WHERE outcome IS NULL;

-- =============================================================
-- Views
-- =============================================================

-- Current effective status of a task (materializes 'in_progress').
CREATE VIEW task_status AS
SELECT
    t.id,
    t.title,
    t.priority,
    CASE
        WHEN EXISTS (SELECT 1 FROM dispatches d
                     WHERE d.task_id = t.id AND d.outcome IS NULL)
        THEN 'in_progress'
        ELSE t.state
    END AS state,
    t.created_at,
    t.updated_at
FROM tasks t;

-- Tasks eligible for dispatch: state='ready' AND every depends_on target is done.
-- NB: a dependency counts as satisfied when its stored state is 'done'.
-- In-progress deps are NOT satisfied.
CREATE VIEW ready_tasks AS
SELECT t.*
FROM tasks t
WHERE t.state = 'ready'
  AND NOT EXISTS (
      SELECT 1
      FROM edges e
      JOIN tasks dep ON dep.id = e.dst_id
      WHERE e.kind = 'depends_on'
        AND e.src_kind = 'task' AND e.src_id = t.id
        AND e.dst_kind = 'task'
        AND dep.state <> 'done'
  )
  AND NOT EXISTS (
      SELECT 1 FROM dispatches d
      WHERE d.task_id = t.id AND d.outcome IS NULL
  )
ORDER BY COALESCE(t.priority, 0) DESC, t.created_at ASC;

-- Dispatches currently considered live (outcome null, heartbeat fresh).
-- Threshold is applied at query time; the wrapper can parameterize.
CREATE VIEW open_dispatches AS
SELECT *
FROM dispatches
WHERE outcome IS NULL;

-- Knowledge marked stale OR derived from anything stale/superseded.
-- The `stale` column is the persisted flag; a separate recompute routine
-- (triggered by supersedes inserts or source updates) sets it.
CREATE VIEW stale_knowledge AS
SELECT k.*
FROM knowledge k
WHERE k.stale = 1;

-- =============================================================
-- updated_at maintenance
-- =============================================================

CREATE TRIGGER tasks_touch AFTER UPDATE ON tasks
BEGIN
    UPDATE tasks SET updated_at = datetime('now') WHERE id = NEW.id;
END;

CREATE TRIGGER knowledge_touch AFTER UPDATE ON knowledge
BEGIN
    UPDATE knowledge SET updated_at = datetime('now') WHERE id = NEW.id;
END;
