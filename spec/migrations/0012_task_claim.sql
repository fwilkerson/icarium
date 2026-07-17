-- Ownership stamp for `task claim`: who took the task off the ready queue
-- and when. Meaningful only while state='in_progress'; any other state
-- transition clears both (see updateTask). NULL for tasks moved to
-- in_progress by `task start` or `dispatch run`, which claim nothing.
ALTER TABLE tasks ADD COLUMN claimed_by TEXT;
ALTER TABLE tasks ADD COLUMN claimed_at TEXT;
