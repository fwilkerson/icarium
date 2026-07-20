# Task state semantics, and two ready queues

Status: accepted (2026-07-19)

`TaskState` is the task lifecycle axis. Each value means one thing:

| State              | Meaning                                                                    |
| ------------------ | -------------------------------------------------------------------------- |
| `idea`             | Captured, not evaluated. A maintainer still has to decide if it's real.     |
| `planned`          | Accepted, **under-specified**. Details may rot; not safe to hand to anyone. |
| `ready`            | Fully specified, headless: a dispatched agent may take it unattended.       |
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

`ready` itself was never ambiguous. `CONTEXT.md` defines the project as
headless-agent development, so `ready` = the headless queue is the aligned
reading; the exception is what needs the qualified name.

Separately, the states were only ever documented in scattered help text.
Two of them carry non-obvious meaning worth writing down: `planned` is
*under-specified* (not "specified but not yet queued"), and `done` is
*accepted*, which is weaker than what dependents need.

## Decision

- Add `ready_interactive`. It shares `ready`'s specification bar and differs
  only in who does the work.
- `blocked` stays singular. Its two flavours (dispatch failed vs. waiting on
  information) are distinguishable from the `dispatches` row, and both agree
  on the only thing a state must encode: not proceeding, don't hand it out.
- The `ready_tasks` view spans both ready states. The deps-satisfaction gate
  lives there and nowhere else; each caller narrows to its own queue by
  state. `dispatch run` takes `ready`; `task next` and bare `task claim`
  take `ready_interactive`.
- **The headless queue has no CLI surface.** Dispatch selects in-process via
  `claimNextTask`, and nothing mechanical reads the CLI, so `task list
  --ready`, `task next` and bare `task claim` all serve the human. Exposing
  a qualified headless form would be a surface with no caller. `task list
  --state ready` still finds the work — that is a state filter, not a queue.
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
  line goes away and `--ready` stops under-reporting.
- Pre-1.0: `--ready` and bare `task claim` changed meaning rather than
  gaining a qualifier. No compatibility shim.
