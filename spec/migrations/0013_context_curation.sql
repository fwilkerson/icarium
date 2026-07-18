-- ADR 0001: retirement is derived from append-only curation events; the
-- stored context.stale flag goes away. Backfill one 'stale' event per
-- flagged row, then drop the column so recording a curation event is the
-- only way to retire or revive an entry.

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

-- Connections do not enable PRAGMA foreign_keys; cascade via trigger,
-- matching the edges table.
CREATE TRIGGER context_curation_cascade_delete
AFTER DELETE ON context
BEGIN
    DELETE FROM context_curation WHERE context_id = OLD.id;
END;

-- Deterministic backfill ids: re-running against a copy of the same DB
-- yields identical rows.
INSERT INTO context_curation (id, context_id, disposition, created_at)
SELECT 'backfill-' || id, id, 'stale', updated_at FROM context WHERE stale = 1;

DROP VIEW stale_context;
ALTER TABLE context DROP COLUMN stale;

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
