-- Create FTS5 body index; populate from existing rows.
-- Body files are written by the CLI going forward.
-- The body column stays as a DB cache synced by the mtime sweep.

CREATE VIRTUAL TABLE body_fts USING fts5(
    id    UNINDEXED,
    kind  UNINDEXED,
    title,
    body
);

INSERT INTO body_fts (id, kind, title, body)
SELECT id, 'task', title, body FROM tasks;

INSERT INTO body_fts (id, kind, title, body)
SELECT id, 'context', title, body FROM context;
