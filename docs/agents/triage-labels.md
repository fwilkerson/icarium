# Triage Labels

The skills speak in five canonical triage roles. icarium has no label mechanism
(yet — native support is a future consideration), so roles are realized as task
states where a natural mapping exists.

| Role              | In icarium                                 | Meaning                                 |
| ----------------- | ------------------------------------------ | --------------------------------------- |
| `needs-triage`    | state `idea`                               | Maintainer needs to evaluate this       |
| `needs-info`      | state `blocked` with `--block-reason`      | Waiting on more information             |
| `ready-for-agent` | state `ready` (dispatch picks it up)       | Fully specified, ready for an AFK agent |
| `ready-for-human` | `Triage: ready-for-human` line in the body | Requires human implementation           |
| `wontfix`         | state `abandoned`                          | Will not be actioned                    |

When a skill says "apply label X", make the corresponding state transition
(`icarium task update <id> --state <s>`) or body edit.
