-- `task claim` ownership stamp; lifecycle documented in spec/schema.sql.
ALTER TABLE tasks ADD COLUMN claimed_by TEXT;
ALTER TABLE tasks ADD COLUMN claimed_at TEXT;
