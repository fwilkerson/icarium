# Task state semantics, and two ready queues

Status: accepted (2026-07-19), revised (2026-07-20)

`TaskState` is the task lifecycle axis. Each value means one thing:

| State              | Meaning                                                                    |
| ------------------ | -------------------------------------------------------------------------- |
| `idea`             | Captured, not evaluated. A maintainer still has to decide if it's real.     |
| `planned`          | Accepted, **under-specified**. Details may rot; not safe to hand to anyone. |
| `ready_headless`   | Fully specified, headless: a dispatched agent may take it unattended.       |
| `ready_interactive`| Fully specified, but a human at a keyboard must do it.                      |
| `in_progress`      | Claimed. `claimed_by`/`claimed_at` say by whom, since when.                 |
| `done`             | The work is **accepted**. It is not necessarily *merged* — see below.       |
| `blocked`          | Cannot proceed; `block_reason` says why.                                    |
| `abandoned`        | Will not be actioned. An explicit dead end, not a backlog item.             |

## Context

The five triage roles the skills speak (`docs/agents/triage-labels.md`) map
onto states, and `ready-for-human` needs one of its own. Without it that work
hides in `planned`, which corrupts `planned`'s meaning (under-specified,
details may rot) and makes the ready surfaces under-report real work.

Two states carry non-obvious meaning worth writing down: `planned` is
*under-specified* (not "specified but not yet queued"), and `done` is
*accepted*, which is weaker than what dependents need.

## Decision

- Two ready states, `ready_headless` and `ready_interactive`. They share one
  specification bar and differ only in who does the work. **Neither name is
  bare** — an unqualified `ready` is a parse error naming both, and no alias
  resolves it (an alias is the same trap, silently). Symmetry is the point:
  leave the unqualified name on one queue and every unqualified surface
  inherits an arbitrary default. The concrete failure that shape produces —
  `--state ready` selecting the headless queue while a `--ready` selector
  selected the interactive one, two opposite answers to "which queue" from
  one word in adjacent flags of the same command. Documenting it in help
  text did not make it usable.
- `blocked` stays singular. Its two flavours (dispatch failed vs. waiting on
  information) are distinguishable from the `dispatches` row, and both agree
  on the only thing a state must encode: not proceeding, don't hand it out.
- The `ready_tasks` view spans both ready states. The deps-satisfaction gate
  lives there and nowhere else; each caller narrows to its own queue by
  state. `dispatch run` takes `ready_headless`; `task next` and bare `task
  claim` take `ready_interactive`.
- **Queue and state are separate surfaces.** `task queue` is the ordered
  worklist: both ready states, dependency-gated, one interleaved list in
  priority order, narrowed by `--headless` / `--interactive`. `task list` is
  how you *find* work — a pure filter, no gate, no queue semantics, so it
  cannot re-acquire a default queue. There is no `--ready` selector: queue
  selection is what `--state` does, and the dependency gate is `task queue`.
- `task next` and bare `task claim` stay interactive with no qualifier.
  Views have no natural default, but these are actions, and a human can only
  perform interactive work; headless selection is in-process via
  `claimNextTask` and never through the CLI.
- `task claim <id>` claims a *named* task in either ready state, through the
  same `BEGIN IMMEDIATE` path. It ignores the deps gate: naming the task is
  the selection. It refuses any other state, and says so. Permissive across
  both ready states on purpose — the dangerous direction is dispatch taking
  human work, enforced in `claimNextTask`; a human taking headless work is
  harmless and serves a real workflow (dispatch keeps failing, do it by
  hand). Forcing a state edit first would make the state lie.

### `done` means accepted, not landed

A dependent needs its dependency's *code in base*, which `done` alone does
not give: under park-by-default a successful dispatch is `done` while still
parked (`merge_sha IS NULL`). The `ready_tasks` deps gate therefore requires
`done` **and** no successful-but-unmerged dispatch. Tasks completed manually
(no dispatch rows) and no-commit successes (merge-stamped at finish) satisfy
it as before.

This is deliberate: collapsing "accepted" and "landed" into one state would
either park acceptance behind a merge a human may never run, or let
dependents build on a base missing their dependency.

## Consequences

- Triage's `ready-for-human` is a real state transition, so the ready
  surfaces do not under-report it.
- Every surface naming a ready queue must qualify which one, including new
  ones. That is the invariant the two-name split exists to hold.
