-- The task_status view materialized 'in_progress' from open dispatches, but
-- nothing ever read it: Repo.Task.listTasks selects from `tasks` and derives
-- effective state client-side. Two sources of truth, one of them dead — drop
-- the view rather than grow a second consumer.

DROP VIEW IF EXISTS task_status;
