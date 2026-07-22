-- Allow task→task edges for the `derived_from` kind: a task discovered while
-- working another one. `depends_on` inverts the meaning (the new task does not
-- block on its parent) and `references` is task→context, so the shape had no
-- legal spelling and follow-up tasks were being filed unlinked.
--
-- Rebuilds the edges table to update the CHECK constraint
-- (SQLite does not support ALTER COLUMN for constraints).

PRAGMA legacy_alter_table = 1;
DROP TRIGGER edges_cascade_task_delete;
DROP TRIGGER edges_src_task_exists;
DROP TRIGGER edges_src_context_exists;
DROP TRIGGER edges_dst_task_exists;
DROP TRIGGER edges_dst_context_exists;
DROP TRIGGER edges_cascade_context_delete;
DROP INDEX edges_src_idx;
DROP INDEX edges_dst_idx;
DROP INDEX edges_kind_idx;

CREATE TABLE edges_new (
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

    CHECK (
        (kind = 'depends_on'   AND src_kind = 'task'    AND dst_kind = 'task') OR
        (kind = 'references'   AND src_kind = 'task'    AND dst_kind = 'context') OR
        (kind = 'references'   AND src_kind = 'context' AND dst_kind = 'context') OR
        (kind = 'derived_from' AND src_kind = 'context' AND dst_kind IN ('task','context')) OR
        (kind = 'derived_from' AND src_kind = 'task'    AND dst_kind = 'task') OR
        (kind = 'supersedes'   AND src_kind = 'context' AND dst_kind = 'context')
    ),

    CHECK (NOT (src_kind = dst_kind AND src_id = dst_id))
);

INSERT INTO edges_new (id, kind, src_kind, src_id, dst_kind, dst_id, created_at)
SELECT id, kind, src_kind, src_id, dst_kind, dst_id, created_at
FROM edges;

DROP TABLE edges;
ALTER TABLE edges_new RENAME TO edges;

CREATE INDEX edges_src_idx  ON edges(src_kind, src_id);
CREATE INDEX edges_dst_idx  ON edges(dst_kind, dst_id);
CREATE INDEX edges_kind_idx ON edges(kind);

CREATE TRIGGER edges_cascade_task_delete
AFTER DELETE ON tasks
BEGIN
    DELETE FROM edges WHERE (src_kind='task' AND src_id=OLD.id)
                         OR (dst_kind='task' AND dst_id=OLD.id);
END;

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

CREATE TRIGGER edges_cascade_context_delete
AFTER DELETE ON context
BEGIN
    DELETE FROM edges WHERE (src_kind='context' AND src_id=OLD.id)
                         OR (dst_kind='context' AND dst_id=OLD.id);
END;

PRAGMA legacy_alter_table = 0;
