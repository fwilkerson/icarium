# Triage Labels

The skills speak in five canonical triage roles. icarium has no label mechanism
(yet — native support is a future consideration), so roles are realized as task
states where a natural mapping exists.

| Role              | In icarium                                 | Meaning                                 |
| ----------------- | ------------------------------------------ | --------------------------------------- |
| `needs-triage`    | state `idea`                               | Maintainer needs to evaluate this       |
| `needs-info`      | state `blocked` with `--block-reason`      | Waiting on more information             |
| `ready-for-agent` | state `ready` (dispatch picks it up)       | Fully specified, ready for an AFK agent |
| `ready-for-human` | state `ready_interactive`                  | Requires human implementation           |
| `wontfix`         | state `abandoned`                          | Will not be actioned                    |

The two ready states share a specification bar and differ only in who does
the work: `dispatch run` takes `ready` and never `ready_interactive`, while
`task next` and bare `task claim` serve the interactive queue and never
`ready`. Naming a task — `task claim <id>` — takes it from either.
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
