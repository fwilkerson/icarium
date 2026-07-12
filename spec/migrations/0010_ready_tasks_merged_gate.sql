-- Park-by-default moved task 'done' to park time, so done no longer implies
-- the dependency's work is in base. A dependency now also blocks dependents
-- while it has a successful-but-unmerged (parked) dispatch. Manually-completed
-- tasks (no dispatch rows) and no-commit successes (merge-stamped at finish)
-- satisfy dependents as before.
DROP VIEW ready_tasks;
CREATE VIEW ready_tasks AS
SELECT t.*
FROM tasks t
WHERE t.state = 'ready'
  AND NOT EXISTS (
      SELECT 1
      FROM edges e
      JOIN tasks dep ON dep.id = e.dst_id
      WHERE e.kind = 'depends_on'
        AND e.src_kind = 'task' AND e.src_id = t.id
        AND e.dst_kind = 'task'
        AND (dep.state <> 'done'
             OR EXISTS (
                 SELECT 1 FROM dispatches pd
                 WHERE pd.task_id = dep.id
                   AND pd.outcome = 'success'
                   AND pd.merge_sha IS NULL
             ))
  )
  AND NOT EXISTS (
      SELECT 1 FROM dispatches d
      WHERE d.task_id = t.id AND d.outcome IS NULL
  )
ORDER BY COALESCE(t.priority, 0) DESC, t.created_at ASC;
