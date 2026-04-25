-- icarium schema v1 — for migration tests only.
-- Identical to spec/schema.sql except tasks.state CHECK does not include 'in_progress'.
-- The 1→2 migration rebuilds the tasks table to add that value.

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
                CHECK (state IN ('idea','planned','ready','done',
                                 'blocked','abandoned')),
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

    CHECK (
        (kind = 'depends_on'   AND src_kind = 'task'      AND dst_kind = 'task') OR
        (kind = 'references'   AND src_kind = 'task'      AND dst_kind = 'knowledge') OR
        (kind = 'derived_from' AND src_kind = 'knowledge' AND dst_kind IN ('task','knowledge')) OR
        (kind = 'supersedes'   AND src_kind = 'knowledge' AND dst_kind = 'knowledge')
    ),

    CHECK (NOT (src_kind = dst_kind AND src_id = dst_id))
);

CREATE INDEX edges_src_idx ON edges(src_kind, src_id);
CREATE INDEX edges_dst_idx ON edges(dst_kind, dst_id);
CREATE INDEX edges_kind_idx ON edges(kind);

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
-- Dispatches
-- =============================================================

CREATE TABLE dispatches (
    id             TEXT PRIMARY KEY,
    task_id        TEXT NOT NULL REFERENCES tasks(id) ON DELETE CASCADE,
    branch         TEXT NOT NULL,
    base_branch    TEXT NOT NULL,
    base_sha       TEXT NOT NULL,
    pid            INTEGER,
    model          TEXT NOT NULL,
    effort         TEXT NOT NULL CHECK (effort IN ('low','medium','high')),
    started_at     TEXT NOT NULL DEFAULT (datetime('now')),
    heartbeat_at   TEXT NOT NULL DEFAULT (datetime('now')),
    ended_at       TEXT,
    outcome        TEXT CHECK (outcome IN ('success','failure','interrupted')),
    merge_sha      TEXT,
    last_commit    TEXT,
    notes          TEXT,
    log_path       TEXT
);

CREATE INDEX dispatches_task_idx      ON dispatches(task_id);
CREATE INDEX dispatches_open_idx      ON dispatches(outcome) WHERE outcome IS NULL;
CREATE INDEX dispatches_heartbeat_idx ON dispatches(heartbeat_at) WHERE outcome IS NULL;

-- =============================================================
-- Views
-- =============================================================

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

CREATE VIEW open_dispatches AS
SELECT *
FROM dispatches
WHERE outcome IS NULL;

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
