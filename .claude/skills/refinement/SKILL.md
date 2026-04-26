---
name: refinement
description: Run a refinement session on an icarium project — verify done work, curate the task and knowledge backlogs, prune/prioritize, and write dispatch-ready task bodies. Use when the working directory contains an `.icarium/` dir or `icarium.toml`. Invoke at session start to catch up on prior context, or any time the user wants to plan/curate before the next batch of headless dispatches.
---

# Icarium refinement session

Icarium is a CLI for managing tasks, knowledge, and headless-Claude dispatches. Headless Sonnet agents execute one task per dispatch on a branch, with merge gates. The human-driven work that happens *between* dispatch batches is **refinement** — and that's what this skill is for.

## First moves

Catch up on the prior session's handoff:

```
./bin/icarium know list --discipline planning
./bin/icarium know show <id-of-top-entry>
```

That entry is the prior session's handoff — what shipped, what's queued, what design questions are open, and the recommended dispatch order for the next batch.

## Then survey state

```
./bin/icarium task list                    # ready/planned/idea backlog
./bin/icarium dispatch list | head         # most recent runs
```

If something looks off (interrupted dispatches, stale knowledge, surprising state), investigate before adding new work.

## What refinement covers

Standard agile-style backlog refinement, applied to both tasks and knowledge:

- **Verify done work.** Inspect the most recent merged dispatches. Read the diff. Check that knowledge was recorded for non-obvious decisions.
- **Curate knowledge.** Mark stale entries `--stale`, supersede outdated ones, recategorize, prune dead lineage.
- **Refine the backlog.** Promote `idea` → `planned` → `ready` as designs settle. Re-prioritize. Drop ideas that no longer fit.
- **Write dispatch-ready task bodies.** A headless agent can execute reliably only when product/design choices are *already made*. If a task body lists "options" or asks "should we …", it's not ready — resolve the choice in this session and write the answer into the body. Otherwise the agent picks, and the agent optimizes for shipping, not for what you'd pick.
- **Brainstorm and queue new work.** Discoveries from verification and use go in as `idea` tasks; promote later.

## Workflow primitives

```
./bin/icarium task add 'TITLE' --body-stdin --state planned --priority N
./bin/icarium task update ID --state ready
./bin/icarium task show ID
./bin/icarium know add 'TITLE' --body-stdin --derived-from ID
./bin/icarium know show ID
./bin/icarium dispatch show DISPATCH_ID
./bin/icarium drain                        # process the ready queue
./bin/icarium dispatch run TASK_ID         # one-shot a single task
```

State machine: `idea | planned | ready | in_progress | done | blocked | abandoned`. `in_progress` is derived from an open dispatch row. `drain` only runs `ready` tasks in priority order.

## Closing the session

End with a handoff knowledge entry:

```
./bin/icarium know add 'Handoff: <short context>' \
    --discipline planning \
    --body-stdin <<'EOF'
... what shipped, what's queued, design questions open, recommended next moves ...
EOF
```

The `discipline: planning` tag isolates handoffs from the auto-pull category-matching in dispatched-agent prompts (handoffs are for the next refinement session, not for headless executors). The next session will find this entry via `know list --discipline planning`.
