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
onto states, but `ready-for-human` had no state to map to. It lived as a
`Triage: ready-for-human` line in the body — a convention no Haskell read.
Since `ready_tasks` keyed on `state = 'ready'` plus a satisfied deps gate,
such work sitting in `ready` was picked up by `dispatch run`. So in practice
it hid in `planned`, which corrupted `planned`'s meaning (under-specified,
details may rot) and made the ready surfaces under-report real work.

The original split kept `ready` bare, reasoning that `CONTEXT.md` defines the
project as headless-agent development, so `ready` = the headless queue is the
aligned reading and only the exception needs a qualified name. That was wrong
in practice — see the 2026-07-20 revision below.

Separately, the states were only ever documented in scattered help text.
Two of them carry non-obvious meaning worth writing down: `planned` is
*under-specified* (not "specified but not yet queued"), and `done` is
*accepted*, which is weaker than what dependents need.

## Decision

- Two ready states, `ready_headless` and `ready_interactive`. They share one
  specification bar and differ only in who does the work. Neither name is
  bare: an unqualified `ready` is a parse error naming both.
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
  cannot re-acquire a default queue.
- `task next` and bare `task claim` stay interactive with no qualifier.
  Views have no natural default, but these are actions, and a human can only
  perform interactive work; headless selection is in-process via
  `claimNextTask` and never through the CLI.
- `task claim <id>` claims a *named* task in either ready state, through the
  same `BEGIN IMMEDIATE` path. It ignores the deps gate: naming the task is
  the selection. It refuses any other state, and says so.

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

- Migration 0015 rebuilds `tasks` to widen the state CHECK and recreates the
  view. Existing `ready` rows keep their meaning — the split adds a state,
  it does not reinterpret one.
- Triage's `ready-for-human` becomes a real state transition, so the body
  line goes away and the ready surfaces stop under-reporting.
- Pre-1.0: bare `task claim` changed meaning rather than gaining a
  qualifier. No compatibility shim.

## Revision, 2026-07-20

The bare `ready` name did not survive contact. It meant the *headless* queue
on the state axis (`--state ready`) and the *interactive* queue on the
selector axis (`--ready`) — two opposite answers to "which queue" from one
word, in adjacent flags of the same command. Help text documented it; that
did not make it usable.

The root cause was asymmetry, not the flag: one queue held the unqualified
name, so every unqualified surface inherited an arbitrary default, and
`--ready` was merely where it was noticed first.

Revised accordingly:

- `ready` → `ready_headless`, in both the CLI and storage. Bare `ready` is
  now an unknown-value error listing the valid states. No alias — an alias
  is the same trap, silently resolved.
- `--ready` is removed. Its two jobs split: queue selection is what `--state`
  already does, and the dependency gate — previously implicit in the flag —
  becomes explicit as `task queue`.
- `task claim <id>` stays permissive across both ready states. The dangerous
  direction is dispatch taking human work, enforced in `claimNextTask`; a
  human taking headless work is harmless and serves a real workflow
  (dispatch keeps failing, do it by hand). Forcing a state edit first would
  make the state lie.
- Migration 0016 renames existing rows. All were agent-safe, so the rename
  is unconditional.

Amended in place rather than superseded: these ADRs are read mid-task to
learn current semantics, and the surviving content (why `ready_interactive`
exists, why `blocked` stays singular, why `done` ≠ landed) is most of the
file, so a successor could not stand alone.
