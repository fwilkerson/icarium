-- Add 'kind' to the category axis vocabulary: a third, task-only workflow
-- axis (bug/enhancement/chore/…) alongside the domain+discipline retrieval
-- axes. CHECK constraints can't be ALTERed in place, so `categories` is
-- rebuilt.
--
-- `task_categories` and `context_categories` both carry an FK to
-- categories(id), and whether RENAME TO rewrites that clause in the child
-- depends on `legacy_alter_table`, which this file does not control (same
-- reasoning as 0005). So the children are rebuilt explicitly against the
-- final table name instead of relying on either behaviour.
--
-- Ordering matters: each child is replaced before `categories_old` is
-- dropped, so no child still references the outgoing table when it goes and
-- ON DELETE CASCADE cannot fire against live rows. No data is rewritten —
-- the new CHECK is a superset of the old one.

ALTER TABLE categories RENAME TO categories_old;

CREATE TABLE categories (
    id    TEXT PRIMARY KEY,
    axis  TEXT NOT NULL CHECK (axis IN ('domain','discipline','kind')),
    name  TEXT NOT NULL,
    UNIQUE (axis, name)
);

INSERT INTO categories (id, axis, name)
SELECT id, axis, name FROM categories_old;

CREATE TABLE task_categories_new (
    task_id     TEXT NOT NULL REFERENCES tasks(id) ON DELETE CASCADE,
    category_id TEXT NOT NULL REFERENCES categories(id) ON DELETE CASCADE,
    PRIMARY KEY (task_id, category_id)
);
INSERT INTO task_categories_new (task_id, category_id)
SELECT task_id, category_id FROM task_categories;
DROP TABLE task_categories;
ALTER TABLE task_categories_new RENAME TO task_categories;

CREATE TABLE context_categories_new (
    context_id  TEXT NOT NULL REFERENCES context(id) ON DELETE CASCADE,
    category_id TEXT NOT NULL REFERENCES categories(id) ON DELETE CASCADE,
    PRIMARY KEY (context_id, category_id)
);
INSERT INTO context_categories_new (context_id, category_id)
SELECT context_id, category_id FROM context_categories;
DROP TABLE context_categories;
ALTER TABLE context_categories_new RENAME TO context_categories;

DROP TABLE categories_old;
