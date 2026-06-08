# icarium

icarium gives you three things in one local CLI:

- **Tasks** — a small backlog with states, priorities, and dependencies.
- **Knowledge** — context entries (domain notes) you attach to tasks; the dispatcher pulls relevant ones into the agent's prompt.
- **Dispatch** — runs a headless Claude agent against a ready task on its own branch; if `build`/`test` (and an optional reviewer) pass, the orchestrator fast-forward-merges into the integration branch.

Storage is a local SQLite DB. It's a tool for one developer, or a small team.

IDs are ULIDs; any unique prefix is accepted. Run `icarium agents` for the agent-facing quickstart.

## Getting started

```sh
icarium init                              # create DB, apply schema, write icarium.toml
icarium task add "Wire up the parser" --priority 7
# stdout: <new-id>\n<body-path>
#   Write your markdown to <body-path> — the file isn't pre-created.
icarium task update <id> --state ready    # mark it dispatchable
icarium dispatch run <id>                 # run one task now
icarium dispatch run                      # …or drain the whole ready queue
```

Bodies are plain markdown files on disk at `.icarium/bodies/{tasks,contexts}/<ulid>.md`. Metadata lives in the DB; edit bodies with your normal editor.

## Concepts

**Task lifecycle.** `idea → planned → ready → in-progress → done`, plus `blocked` (needs a
`--block-reason`) and `abandoned`. Dispatch only picks up `ready` tasks whose `depends-on`
upstreams are all `done`.

**Context entries** (`ctx`). Reusable knowledge attached to tasks via links. Supersession chains
(`--derived-from` / `--supersedes`) let a newer note replace an older one; `ctx list` shows only
current heads by default.

**Links** (`link`). Typed edges between nodes: `depends-on` (task→task), `references` (task→ctx),
`derived-from` / `supersedes` (ctx→ctx).

**Categories.** A controlled `domain` / `discipline` vocabulary for tagging and filtering. Edit the
`[categories]` lists in `icarium.toml`, then `icarium category sync` to reconcile the DB
(`--prune` to drop removed ones).

## Configuration

`icarium init` writes an `icarium.toml` at the project root. The fields:

```toml
[project]
# Branch dispatches FF-merge into.
integration_branch = "main"

[commands]
# Gate commands run after the agent commits. A failing gate marks the
# dispatch failed and moves the task to blocked.
build = "cabal build all"
test  = "cabal test all"

[dispatch]
# Defaults for the headless agent; override per-run with
# `dispatch run --model / --effort`.
model  = "claude-sonnet-4-6"
effort = "high"

# `tools` is what claude is told about; `allowed_tools` is the permission
# allowlist. Keep `allowed_tools` to read-only Bash plus the safe icarium
# mutation subset.
tools = ["Read", "Edit", "Write", "Grep", "Glob", "Bash"]
allowed_tools = [
  "Read", "Edit", "Write", "Grep", "Glob",
  "Bash(icarium:*)", "Bash(git:*)", "Bash(cabal:*)",
]
scratch_dir = ".icarium/scratch"

# Wall-clock timeout per dispatch (minutes, must be a positive integer).
max_minutes_per_dispatch = 30
heartbeat_stale_seconds  = 300
log_retention_runs       = 25
# Retry-storm watchdog: kill a dispatch after this many consecutive api_retry events.
retry_storm_threshold    = 3

[categories]
# Controlled vocabulary for tagging tasks and context entries. After editing,
# run `icarium category sync` (add `--prune` to remove deleted ones).
domains     = ["core"]
disciplines = ["development"]

# Optional reviewer gate (see "Reviewers" below). Omit the section to disable.
# [review]
# enabled      = true
# model        = "claude-sonnet-4-6"    # defaults to dispatch.model
# max_attempts = 2                      # total attempts incl. first
# prompt_path  = ".icarium/reviewer.md" # defaults to built-in prompt
```

## Reviewers

A reviewer is an optional post-commit gate. When `[review] enabled = true`, after the worker
commits, the orchestrator runs a second Claude agent — given **only the `Read` tool** — with the
task title, task body, and the git diff, and asks it to judge the work.

```
worker → diff → reviewer ─┬─ pass / warn → FF-merge
                          └─ fail        → retry (findings injected) or block
```

**Contract.** The reviewer must emit **only** a YAML block. icarium reads the `status:` line; a
non-zero exit, a timeout, or an unparseable result all count as `fail`.

```yaml
status: pass | warn | fail
findings:
  - severity: warn
    file: src/Foo.hs
    message: "Description of the concern"
```

**Verdicts.**

- **`pass`** — merge proceeds.
- **`warn`** — merge proceeds, but the findings are captured as a context entry titled
  `reviewer warn: <task title>` (body = the YAML), tagged with the task's categories and linked
  back to it with a `references` edge for provenance. Surfaces via `icarium ctx list`,
  `icarium search`, and `icarium link list --from <task>` so the concern isn't lost.
- **`fail`** — this attempt fails. If `max_attempts` remain, the worker retries with the findings
  injected into its prompt under a `## Reviewer findings from previous attempt` heading — address
  them directly. Out of attempts → dispatch `failure`, task `blocked`.

The default reviewer prompt is built in; override it with `prompt_path`. See
`.icarium/reviewer.md` in this repo for a worked example. Reviewer output is stored per dispatch —
`icarium dispatch logs <id>` prints both the worker and reviewer logs.

## Dispatch & drain semantics

A dispatch cuts a task branch, runs the agent, then runs the `build`/`test` gates and the optional
reviewer; on success it fast-forward-merges into `integration_branch` and deletes the branch.

`dispatch run` (no task ID) drains the ready queue in priority order. "Ready" is stricter than
`state='ready'`: the `ready_tasks` view also requires every `depends_on` upstream to be `done`.
In-progress or blocked upstreams are not satisfied.

**Failure quarantine.** When a dispatch fails, the task is moved to `state='blocked'`. Because
`blocked` is not `done`, any task depending on it drops out of the ready queue — quarantined with no
explicit action. Independent tasks keep draining.

To release a quarantined task, resolve the upstream failure and move it back to `ready` (or `done`
if completed out-of-band). `dispatch run <TASK_ID>` overrides the quarantine: naming a task skips
the ready-queue check.

## Command reference

Every command has `--help`; agents should start with `icarium agents`.

| Command | Purpose |
| --- | --- |
| `init` | Create the DB, apply schema, write `icarium.toml`. |
| `task` | Manage tasks: `add`, `list`, `show`, `update`, `rm`, `next`, `path`, `cat`, `exists`. |
| `ctx` | Manage context entries: `add`, `list`, `show`, `update`, `rm`, `path`, `cat`, `children`, `tree`, `exists`. |
| `link` | Typed edges between nodes: `add`, `list`, `rm`. |
| `category` | Manage the domain/discipline vocabulary: `list`, `sync`. |
| `dispatch` | Run and inspect dispatches: `run`, `list`, `show`, `logs`, `recover`. |
| `search` | FTS5 search over titles and bodies (`--kind`, `--domain`, phrase/`OR` queries). |
| `reindex` | Rebuild the FTS5 body index. |
| `doctor` | Health check: config, DB, schema version, `claude`/`git` binaries, orphaned dispatches. |
| `agents` | Print the agent quickstart. |

## Development

### Build

```sh
make build   # cabal build all
make install # copy binary to bin/ (add to PATH via direnv)
make test    # cabal test all
```

### Lint

```sh
make lint    # runs hlint src/ app/
```

Install HLint if not present:

```sh
# via cabal (one-time, slow first build due to ghc-lib-parser dep):
cabal install hlint --installdir=~/.local/bin

# or via brew (prebuilt, faster):
brew install hlint
```

HLint settings live in `.hlint.yaml` at the repo root.
