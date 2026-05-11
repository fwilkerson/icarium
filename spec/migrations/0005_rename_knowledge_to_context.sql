-- Rename `knowledge` → `context` across schema. Single transaction
-- (driver wraps this file in BEGIN/COMMIT and stamps user_version).
--
-- Tables that need rebuilding rather than ALTERing:
--   * `knowledge_categories` — column rename + FK now points at `context`.
--     Auto-rewrite of the child FK on RENAME TO depends on
--     `legacy_alter_table=0`, which we don't control from here, so we
--     rebuild explicitly.
--   * `edges` — CHECK constraints can't be ALTERed in place, and the
--     stored `src_kind`/`dst_kind` values need `'knowledge'` rewritten
--     to `'context'`.

ALTER TABLE knowledge RENAME TO context;

CREATE TABLE context_categories (
    context_id  TEXT NOT NULL REFERENCES context(id) ON DELETE CASCADE,
    category_id TEXT NOT NULL REFERENCES categories(id) ON DELETE CASCADE,
    PRIMARY KEY (context_id, category_id)
);

INSERT INTO context_categories (context_id, category_id)
SELECT knowledge_id, category_id FROM knowledge_categories;

DROP TABLE knowledge_categories;

DROP TRIGGER edges_src_knowledge_exists;
DROP TRIGGER edges_dst_knowledge_exists;
DROP TRIGGER edges_cascade_knowledge_delete;
DROP TRIGGER knowledge_touch;
DROP VIEW   stale_knowledge;

-- `edges_cascade_task_delete` (trigger on tasks) and `ready_tasks` (view)
-- both reference `edges` in their bodies. With the modern default
-- (legacy_alter_table=0), DROP TABLE edges below would refuse rather
-- than orphan those references. Switch to legacy mode for the rebuild;
-- the offending trigger is dropped and recreated explicitly anyway.
PRAGMA legacy_alter_table = 1;
DROP TRIGGER edges_cascade_task_delete;

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
        (kind = 'derived_from' AND src_kind = 'context' AND dst_kind IN ('task','context')) OR
        (kind = 'supersedes'   AND src_kind = 'context' AND dst_kind = 'context')
    ),

    CHECK (NOT (src_kind = dst_kind AND src_id = dst_id))
);

INSERT INTO edges_new (id, kind, src_kind, src_id, dst_kind, dst_id, created_at)
SELECT
    id,
    kind,
    CASE src_kind WHEN 'knowledge' THEN 'context' ELSE src_kind END,
    src_id,
    CASE dst_kind WHEN 'knowledge' THEN 'context' ELSE dst_kind END,
    dst_id,
    created_at
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

CREATE TRIGGER context_touch AFTER UPDATE ON context
BEGIN
    UPDATE context SET updated_at = datetime('now') WHERE id = NEW.id;
END;

CREATE VIEW stale_context AS
SELECT k.*
FROM context k
WHERE k.stale = 1;

PRAGMA legacy_alter_table = 0;
