-- Split the single ready queue: add 'ready_interactive' for fully specified
-- work that a human must do, so it stops hiding in 'planned' (which means
-- under-specified) or leaking into the dispatch queue. `ready_tasks` then
-- spans both states and each caller narrows by state.
-- Semantics: docs/adr/0007-task-state-semantics.md.
--
-- CHECK constraints can't be ALTERed in place, so `tasks` is rebuilt. It is
-- rebuilt through an unconstrained backup rather than the usual
-- rename-into-place, because no form of ALTER TABLE RENAME works here:
-- RENAME reparses every trigger in the schema, and `edges_src_task_exists`
-- names `tasks`, which does not exist at the moment of the rename. Going via
-- a backup table means the name `tasks` is only ever dropped and recreated,
-- never renamed — so children (`task_categories`, `dispatches`) keep naming
-- it correctly throughout and `legacy_alter_table`, which this file does not
-- control, never comes up.
--
-- DROP TABLE would fire ON DELETE CASCADE against those children if FK
-- enforcement were on. It is not: connections do not set
-- `PRAGMA foreign_keys` (see spec/schema.sql), and this migration only ever
-- runs on a connection that opened an existing DB — a fresh DB is stamped
-- straight to the current schema version and skips it.
--
-- Everything attached to the table goes with it and must come back: the
-- state index and both triggers (`tasks_touch`, `edges_cascade_task_delete`).

DROP VIEW ready_tasks;

CREATE TABLE tasks_backup AS SELECT * FROM tasks;
DROP TABLE tasks;

CREATE TABLE tasks (
    id          TEXT PRIMARY KEY,
    title       TEXT NOT NULL,
    body        TEXT NOT NULL DEFAULT '',
    state       TEXT NOT NULL DEFAULT 'planned'
                CHECK (state IN ('idea','planned','ready','ready_interactive',
                                 'in_progress','done','blocked','abandoned')),
    priority    INTEGER,
    block_reason TEXT,
    no_commit   INTEGER NOT NULL DEFAULT 0 CHECK (no_commit IN (0,1)),
    claimed_by  TEXT,
    claimed_at  TEXT,
    created_at  TEXT NOT NULL DEFAULT (datetime('now')),
    updated_at  TEXT NOT NULL DEFAULT (datetime('now'))
);

INSERT INTO tasks (id, title, body, state, priority, block_reason,
                   no_commit, claimed_by, claimed_at, created_at, updated_at)
SELECT id, title, body, state, priority, block_reason,
       no_commit, claimed_by, claimed_at, created_at, updated_at
FROM tasks_backup;

DROP TABLE tasks_backup;

CREATE INDEX tasks_state_idx ON tasks(state);

CREATE TRIGGER tasks_touch AFTER UPDATE ON tasks
BEGIN
    UPDATE tasks SET updated_at = datetime('now') WHERE id = NEW.id;
END;

CREATE TRIGGER edges_cascade_task_delete
AFTER DELETE ON tasks
BEGIN
    DELETE FROM edges WHERE (src_kind='task' AND src_id=OLD.id)
                         OR (dst_kind='task' AND dst_id=OLD.id);
END;

CREATE VIEW ready_tasks AS
SELECT t.*
FROM tasks t
WHERE t.state IN ('ready','ready_interactive')
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
