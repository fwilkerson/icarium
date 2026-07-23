# Triage Labels

The skills speak in five canonical triage roles. icarium has no label mechanism
(yet — native support is a future consideration), so roles are realized as task
states where a natural mapping exists.

| Role              | In icarium                                 | Meaning                                 |
| ----------------- | ------------------------------------------ | --------------------------------------- |
| `needs-triage`    | state `idea`                               | Maintainer needs to evaluate this       |
| `needs-info`      | state `blocked` with `--block-reason`      | Waiting on more information             |
| `ready-for-agent` | state `ready_headless` (dispatch picks it up) | Fully specified, ready for an AFK agent |
| `ready-for-human` | state `ready_interactive`                  | Requires human implementation           |
| `wontfix`         | state `abandoned`                          | Will not be actioned                    |

The two ready states share a specification bar and differ only in who does
the work: `dispatch run` takes `ready_headless` and never `ready_interactive`,
while `task next` and bare `task claim` serve the interactive queue and never
`ready_headless`. Naming a task — `task claim <id>` — takes it from either.
Neither is named bare `ready`; `icarium task queue` shows both, dependency-gated.
See [ADR 0007](../adr/0007-task-state-semantics.md) for every state's meaning.

When a skill says "apply label X", make the corresponding state transition
(`icarium task update <id> --state <s>`) or body edit.

## Roles that are not lifecycle

The five roles above all describe *where a task is* in its lifecycle, which is
why states carry them. A role describing *what the work is* — `bug`,
`enhancement`, `chore` — is not a lifecycle position and lands on the `kind`
category axis instead:

    icarium task update <id> --kind bug
    icarium task list --kind enhancement

`kind` is task-only and does not affect which ctx a dispatch prompt pulls in
(see CONTEXT.md on retrieval vs workflow axes). Register the vocabulary per
repo in `icarium.toml` under `[categories] kinds`.

## Out-of-scope knowledge base

Skills that speak of an `.out-of-scope/` directory mean, in icarium, a set of
ctx entries: one per rejected **concept**, not per request. They serve
institutional memory (why it was rejected) and deduplication (surface the
prior decision instead of re-litigating it).

    icarium ctx add "Out of scope: dark mode" --body-stdin <<'EOF'
    ...markdown body...
    EOF

Two invariants:

- **Title `Out of scope: <concept>`.** The prefix is what makes the set
  enumerable and the concept is what makes it recognizable without opening
  the entry.
- **No `--domain`, no `--discipline`.** An uncategorized entry can never
  match the category auto-pull, so a rejection never leaks into an unrelated
  dispatch prompt. This is load-bearing, not an oversight — do not "fix" it.
  (`ctx add` inherits a task's categories when `ICARIUM_TASK_ID` is set;
  triage runs interactively, where it is not.)

The body is a relaxed short design document, not a database entry — what is
out of scope, and the durable why. Good reasons cite project scope, a
technical constraint, or a strategic choice; "we're busy" is a deferral, not
a rejection. No `## Prior requests` list: the `references` edges from the
rejected tasks *are* that list.

### Checking, during "gather context"

Enumerate with `icarium ctx list` (uncategorized entries carry no filter to
narrow by) or `icarium search "<concept>"`. Match by concept similarity, not
keyword — "night theme" matches "Out of scope: dark mode". On a match,
surface it to the maintainer with the recorded reason before triaging
further. They may confirm (record and close), reconsider (see below), or
judge the requests distinct (proceed with normal triage).

### Writing, on `wontfix`

Only when an **enhancement** is *rejected*. Never for a bug, and never when
the close reason is "already implemented" — recording a built feature would
poison the dedup check with a false rejection; point at where it lives
instead.

Create the concept entry if it does not exist, otherwise reuse it, then:

    icarium link add <task> references <ctx>
    icarium task update <task> --state abandoned

Also append the decision and the ctx id under `## Triage notes` in the task
body — that is this repo's comment (see [issue tracker](issue-tracker.md)).

### Reconsidering

    icarium ctx curate <ctx> stale

`stale` retires the entry: it drops out of `ctx list` (without `--all`) and
out of every prompt. Old tasks stay `abandoned` — they are historical record.
The request that triggered the reconsideration proceeds through normal triage.
