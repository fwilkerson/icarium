-- Per-task routing overrides for dispatch. NULL = inherit the [dispatch]
-- defaults from icarium.toml, which is every task written before this
-- migration. The dispatches table keeps recording what actually ran.
ALTER TABLE tasks ADD COLUMN model TEXT;

ALTER TABLE tasks ADD COLUMN effort TEXT
    CHECK (effort IN ('low', 'medium', 'high', 'xhigh', 'max'));
