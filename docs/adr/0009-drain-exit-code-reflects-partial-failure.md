# A drain's exit code reflects partial failure

Status: accepted (2026-07-24)

`icarium dispatch run` exits 3 when any dispatch in the drain failed, or when
anything stayed parked. Exit 0 means every task selected was dispatched,
succeeded, and landed. The drain and the single named dispatch answer alike.

## Context

Agents are the callers here. An agent that runs a drain over five tasks, all
of which fail, and reads exit 0 has been told the run was fine — the failures
being discoverable only by parsing the summary printed on the way past.

## Decision

**The exit code is derived from the accumulated results, not asserted per
branch.** Exit 3 when any dispatch failed or any dispatch stayed parked; exit 0
only for a clean full drain. Selection errors keep their own codes — a *named*
task that does not resolve is exit 1, because that is the selector failing
rather than the work.

This mirrors how `Icarium.Dispatch.Decide` concludes an outcome from signals:
the terminal cases report what happened, and one function reads them together.

Exit 3 already means "incomplete, here is the fixing command" throughout the
CLI — `mergeAll`'s "not all parked dispatches landed", the post-drain "land with
`icarium dispatch merge --all`". A failed dispatch fits that reading: the task
is blocked with a reason and wants a human or a retry.

## Considered options

- **Keep the drain at exit 0 for failures.** The argument: a drain's job is to
  empty the queue, and a failed dispatch is a *recorded outcome* — the task
  moved to blocked with a block reason, the dispatch row holds the notes,
  nothing was lost. On that reading a failure is data, not an error, and exit 0
  is honest. Rejected because it reintroduces per-mode policy through the back
  door: "a named dispatch exits 3 on failure but a drain does not" is exactly
  the kind of branch-local rule the unification exists to delete, and it leaves
  the caller with no non-parsing way to learn that anything went wrong.
- **A distinct exit code for failures vs. parked.** Rejected as a distinction
  without a consequence: both mean "this run needs attention before the queue is
  clean", and both are followed up by reading `dispatch list`. Splitting them
  asks every caller to encode a difference it will not act on.

## Consequences

- Breaking for anything scripting a bare `dispatch run` on exit status. Pre-1.0,
  and CLI tests cover it.
- A drain that dispatches nothing (empty queue) still exits 0 — an empty queue
  is a clean drain, not a failure.
- SIGINT and cap-reached are stop *reasons*, not failures; they do not by
  themselves raise the exit code. What was dispatched before them still does.
