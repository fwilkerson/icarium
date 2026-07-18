# Recovering a failed dispatch by hand

When a dispatch fails because claude's API stream stalled (`Stream idle
timeout`, retry storms), the underlying claude session can usually be resumed
with `claude --resume <session_id>`. Icarium does not automate this.

**Use this only when** the dispatch ended `failure` with retry-storm /
stream-idle notes **and** the API issue has cleared (a fresh `claude -p`
works). If the agent's logic was wrong, don't resume — re-ready the task and
let a fresh dispatch retry from the top.

## Steps

1. Find the session id — first line of the dispatch JSONL log
   (path shown by `icarium dispatch show <did>`):

       grep '"session_id"' .icarium/logs/<did>.jsonl | head -1

2. Check out the dispatch branch:

       git checkout dispatch/<did>

   Failed dispatches carry a `wip: dispatch <did> (failed: ...)` commit with
   the agent's working tree at kill time; the sha is also in the dispatch
   notes as `wip_commit:`.

3. Resume with a short continuation prompt (the session already carries the
   task prompt and tool context):

       claude --resume <session_id> -p "continue from your last edits, run the build/tests, then commit any remaining changes"

4. If the resumed run lands clean commits, merge manually. `icarium dispatch
   merge` refuses non-success dispatches, so plain git is correct here —
   run the build/test gates yourself first:

       git checkout <base-branch>
       git merge --ff-only dispatch/<did>
       git branch -d dispatch/<did>

5. Reconcile: `icarium task update <task-id> --state done` (or whatever
   fits). The dispatch row stays `failure` — correct; the success belongs to
   the manual recovery.

## Why not automatic

Auto-respawning on retry-storm kills burns credits during a sustained outage
and masks agent bugs (a confused agent stays confused on resume). If this
runbook gets used more than two or three times, file a task for
`icarium dispatch resume <did>` — it needs session_id persisted on the
dispatches row and safety gates around the merge.
