-- icarium schema
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
                CHECK (state IN ('idea','planned','ready_headless','ready_interactive',
                                 'in_progress','done','blocked','abandoned')),
    -- Two ready queues, same specification bar: 'ready_headless' is work the
    -- dispatch program may take unattended, 'ready_interactive' is work a
    -- human must do. Neither is named bare 'ready' — an unqualified name let
    -- every surface inherit a default queue. See
    -- docs/adr/0007-task-state-semantics.md.
    -- 'in_progress' is stored; the dispatch program sets it before invoking
    -- the agent, then transitions to 'done' or 'blocked' after gates pass.
    priority    INTEGER,                             -- NULL = default
    block_reason TEXT,                               -- structured text when state='blocked'
    no_commit   INTEGER NOT NULL DEFAULT 0 CHECK (no_commit IN (0,1)),
    -- `task claim` ownership stamp; meaningful only while state='in_progress'.
    -- Any other state transition clears both, so a claim cannot outlive the work.
    claimed_by  TEXT,
    claimed_at  TEXT,
    created_at  TEXT NOT NULL DEFAULT (datetime('now')),
    updated_at  TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX tasks_state_idx ON tasks(state);

CREATE TABLE context (
    id          TEXT PRIMARY KEY,
    title       TEXT NOT NULL,
    body        TEXT NOT NULL DEFAULT '',            -- markdown
    created_at  TEXT NOT NULL DEFAULT (datetime('now')),
    updated_at  TEXT NOT NULL DEFAULT (datetime('now')),
    -- Which dispatch produced this entry. NULL = hand-written. Provenance is
    -- an attribute of the row, not a graph edge: nothing traverses ctx→run,
    -- and a run is an execution event rather than a peer of task/context.
    -- SET NULL rather than CASCADE for if FKs are ever enforced: the learning
    -- outlives the run. Declarative only today, like the other FKs here.
    source_dispatch_id TEXT REFERENCES dispatches(id) ON DELETE SET NULL
);

CREATE INDEX context_source_dispatch_idx ON context(source_dispatch_id);

-- Append-only record of curation decisions (ADR 0001). An entry's
-- visibility (current/retired) is derived from its latest event; there is
-- no stored staleness flag.
CREATE TABLE context_curation (
    id          TEXT PRIMARY KEY,                    -- ULID
    context_id  TEXT NOT NULL REFERENCES context(id),
    disposition TEXT NOT NULL
                CHECK (disposition IN ('guidance','rule','refactor','keep','stale')),
    artifact    TEXT,   -- where the content went: doc/skill path (guidance),
                        -- rule/test name (rule), task id (refactor),
                        -- superseding entry id (stale, optional)
    note        TEXT,
    created_at  TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX context_curation_context_idx ON context_curation(context_id);

-- =============================================================
-- Categories (three independent axes, user-defined vocabulary)
--   domain, discipline : retrieval axes. Carried by tasks and context;
--                        they exist to match the two, so both narrow the
--                        auto-pull in categoryMatchedContexts.
--   kind               : workflow axis. Task-only, never on context; it
--                        describes the shape of the work, not when a
--                        learning is relevant, so it must not narrow the
--                        pull. Enforced in code (Types.retrievalAxes).
-- =============================================================

CREATE TABLE categories (
    id    TEXT PRIMARY KEY,
    axis  TEXT NOT NULL CHECK (axis IN ('domain','discipline','kind')),
    name  TEXT NOT NULL,
    UNIQUE (axis, name)
);

CREATE TABLE task_categories (
    task_id     TEXT NOT NULL REFERENCES tasks(id) ON DELETE CASCADE,
    category_id TEXT NOT NULL REFERENCES categories(id) ON DELETE CASCADE,
    PRIMARY KEY (task_id, category_id)
);

CREATE TABLE context_categories (
    context_id  TEXT NOT NULL REFERENCES context(id) ON DELETE CASCADE,
    category_id TEXT NOT NULL REFERENCES categories(id) ON DELETE CASCADE,
    PRIMARY KEY (context_id, category_id)
);

-- =============================================================
-- Typed edges between nodes
--   depends_on   : task -> task
--   references   : task -> context | context -> context
--   derived_from : context -> task | context
--   supersedes   : context -> context
-- Kind/endpoint rules enforced by CHECK + triggers (below).
-- =============================================================

CREATE TABLE edges (
    id         TEXT PRIMARY KEY,
    kind       TEXT NOT NULL
               CHECK (kind IN ('depends_on','references',
                               'derived_from','supersedes')),
    src_kind   TEXT NOT NULL CHECK (src_kind IN ('task','context')),
    src_id     TEXT NOT NULL,
    dst_kind   TEXT NOT NULL CHECK (dst_kind IN ('task','context')),
    dst_id     TEXT NOT NULL,
    created_at TEXT NOT NULL DEFAULT (datetime('now')),
    UNIQUE (kind, src_kind, src_id, dst_kind, dst_id),

    -- Endpoint typing rules per edge kind
    CHECK (
        (kind = 'depends_on'   AND src_kind = 'task'    AND dst_kind = 'task') OR
        (kind = 'references'   AND src_kind = 'task'    AND dst_kind = 'context') OR
        (kind = 'references'   AND src_kind = 'context' AND dst_kind = 'context') OR
        (kind = 'derived_from' AND src_kind = 'context' AND dst_kind IN ('task','context')) OR
        -- task→task: a task discovered while working another one. depends_on
        -- inverts it (the child does not block on its parent).
        (kind = 'derived_from' AND src_kind = 'task'    AND dst_kind = 'task') OR
        (kind = 'supersedes'   AND src_kind = 'context' AND dst_kind = 'context')
    ),

    -- No self-edges
    CHECK (NOT (src_kind = dst_kind AND src_id = dst_id))
);

CREATE INDEX edges_src_idx  ON edges(src_kind, src_id);
CREATE INDEX edges_dst_idx  ON edges(dst_kind, dst_id);
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

CREATE TRIGGER edges_src_context_exists
BEFORE INSERT ON edges
WHEN NEW.src_kind = 'context'
BEGIN
    SELECT RAISE(ABORT, 'edge src context missing')
    WHERE NOT EXISTS (SELECT 1 FROM context WHERE id = NEW.src_id);
END;

CREATE TRIGGER edges_dst_task_exists
BEFORE INSERT ON edges
WHEN NEW.dst_kind = 'task'
BEGIN
    SELECT RAISE(ABORT, 'edge dst task missing')
    WHERE NOT EXISTS (SELECT 1 FROM tasks WHERE id = NEW.dst_id);
END;

CREATE TRIGGER edges_dst_context_exists
BEFORE INSERT ON edges
WHEN NEW.dst_kind = 'context'
BEGIN
    SELECT RAISE(ABORT, 'edge dst context missing')
    WHERE NOT EXISTS (SELECT 1 FROM context WHERE id = NEW.dst_id);
END;

-- Cascade delete: when a node goes away, drop its edges on either side.
CREATE TRIGGER edges_cascade_task_delete
AFTER DELETE ON tasks
BEGIN
    DELETE FROM edges WHERE (src_kind='task' AND src_id=OLD.id)
                         OR (dst_kind='task' AND dst_id=OLD.id);
END;

CREATE TRIGGER edges_cascade_context_delete
AFTER DELETE ON context
BEGIN
    DELETE FROM edges WHERE (src_kind='context' AND src_id=OLD.id)
                         OR (dst_kind='context' AND dst_id=OLD.id);
END;

-- Connections do not enable PRAGMA foreign_keys, so the context_curation
-- FK does not cascade on its own.
CREATE TRIGGER context_curation_cascade_delete
AFTER DELETE ON context
BEGIN
    DELETE FROM context_curation WHERE context_id = OLD.id;
END;

-- =============================================================
-- Dispatches — one row per invocation of the headless agent.
-- A task is "in progress" iff it has a dispatch with outcome IS NULL.
-- =============================================================

CREATE TABLE dispatches (
    id                TEXT PRIMARY KEY,                 -- ULID; also used as branch suffix
    task_id           TEXT NOT NULL REFERENCES tasks(id) ON DELETE CASCADE,
    branch            TEXT NOT NULL,                    -- e.g. 'dispatch/<ulid>'
    base_branch       TEXT NOT NULL,                    -- integration branch at cut time
    base_sha          TEXT NOT NULL,                    -- HEAD sha of base at cut
    pid               INTEGER,
    model             TEXT NOT NULL,
    effort            TEXT NOT NULL CHECK (effort IN ('low','medium','high','xhigh','max')),
    started_at        TEXT NOT NULL DEFAULT (datetime('now')),
    heartbeat_at      TEXT NOT NULL DEFAULT (datetime('now')),
    ended_at          TEXT,
    outcome           TEXT CHECK (outcome IN ('success','failure','interrupted')),
    merge_sha         TEXT,                             -- FF-merge sha on success
    last_commit       TEXT,                             -- latest commit on dispatch branch
    notes             TEXT,                             -- freeform: failure reason, etc.
    log_path          TEXT,                             -- path to jsonl event log
    tokens_in         INTEGER,
    tokens_out        INTEGER,
    tokens_cache_read INTEGER,
    review_verdict    TEXT
                      CHECK (review_verdict IS NULL OR review_verdict IN ('pass','warn','fail')),
    reviewer_log_path TEXT,
    merged_at         TEXT,                            -- set when merge_sha is stamped
    body_changed      INTEGER                          -- reviewer tamper signal; NULL = no review ran
                      CHECK (body_changed IS NULL OR body_changed IN (0, 1))
);

CREATE INDEX dispatches_task_idx      ON dispatches(task_id);
CREATE INDEX dispatches_open_idx      ON dispatches(outcome) WHERE outcome IS NULL;
CREATE INDEX dispatches_heartbeat_idx ON dispatches(heartbeat_at) WHERE outcome IS NULL;

-- =============================================================
-- Full-text search
-- =============================================================

CREATE VIRTUAL TABLE body_fts USING fts5(
    id    UNINDEXED,
    kind  UNINDEXED,
    title,
    body
);

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

-- Claimable tasks: a ready-ish state AND every depends_on target's work is
-- in base. A dependency is satisfied when it is 'done' AND has no
-- successful-but-unmerged (parked) dispatch — under park-by-default, done
-- alone means parked, not landed. Manually-completed tasks (no dispatch
-- rows) and no-commit successes (merge-stamped at finish) satisfy as before.
--
-- The view spans both ready states so the deps gate lives in exactly one
-- place; each caller narrows to its own queue by state (dispatch takes
-- 'ready_headless', the interactive CLI takes 'ready_interactive').
CREATE VIEW ready_tasks AS
SELECT t.*
FROM tasks t
WHERE t.state IN ('ready_headless','ready_interactive')
  AND NOT EXISTS (
      SELECT 1
      FROM edges e
      JOIN tasks dep ON dep.id = e.dst_id
      WHERE e.kind = 'depends_on'
        AND e.src_kind = 'task' AND e.src_id = t.id
        AND e.dst_kind = 'task'
        AND (dep.state <> 'done'
             OR EXISTS (
                 SELECT 1 FROM dispatches pd
                 WHERE pd.task_id = dep.id
                   AND pd.outcome = 'success'
                   AND pd.merge_sha IS NULL
             ))
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

-- Latest curation event per entry; history is never rewritten, the latest
-- event wins. created_at has second resolution; ULID ids break same-second
-- ties (ids embed a ms timestamp; same-ms writes for one entry don't
-- happen — curation is one CLI call per entry).
CREATE VIEW context_latest_curation AS
SELECT cc.*
FROM context_curation cc
WHERE cc.id = (
    SELECT id FROM context_curation
    WHERE context_id = cc.context_id
    ORDER BY created_at DESC, id DESC
    LIMIT 1
);

-- Effective retirement: current = never curated or latest 'keep';
-- retired = latest is guidance/rule/refactor/stale.
CREATE VIEW retired_context AS
SELECT context_id, disposition
FROM context_latest_curation
WHERE disposition <> 'keep';

-- =============================================================
-- updated_at maintenance
-- =============================================================

CREATE TRIGGER tasks_touch AFTER UPDATE ON tasks
BEGIN
    UPDATE tasks SET updated_at = datetime('now') WHERE id = NEW.id;
END;

CREATE TRIGGER context_touch AFTER UPDATE ON context
BEGIN
    UPDATE context SET updated_at = datetime('now') WHERE id = NEW.id;
END;
