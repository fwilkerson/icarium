-- Provenance for gate-written context: which dispatch produced this entry.
-- Replaces a created_at window heuristic that could only separate runs while
-- dispatches on a task stayed serial. NULL = hand-written, or older than this
-- migration.

-- SET NULL, not CASCADE, for if this is ever enforced: the learning outlives
-- the run that produced it, so losing provenance is the right casualty and
-- losing the entry is not. Inert today — `openDb` does not set
-- `PRAGMA foreign_keys`, and nothing deletes a dispatch row anyway.
ALTER TABLE context ADD COLUMN source_dispatch_id TEXT
    REFERENCES dispatches(id) ON DELETE SET NULL;

CREATE INDEX context_source_dispatch_idx ON context(source_dispatch_id);
