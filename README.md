# icarium

icarium gives you three things in one local CLI:

- **Tasks** — a small backlog with states, priorities, and dependencies.
- **Knowledge** — domain notes you can attach to tasks; the dispatcher pulls relevant ones into the agent's prompt.
- **Dispatch** — runs a headless Claude agent against a ready task on its own branch; if `build` and `test` pass, the orchestrator fast-forward-merges into the integration branch.

Storage is a local SQLite DB. It's a tool for one developer, or a small team.

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
  "Bash(./bin/icarium:*)", "Bash(git:*)", "Bash(cabal:*)", "Bash(make:*)",
]

scratch_dir = ".icarium/scratch"

# Wall-clock timeout per dispatch (minutes, must be a positive integer).
max_minutes_per_dispatch = 30
# Cap on dispatches when draining the queue in one run.
max_dispatches_per_run   = 20

heartbeat_stale_seconds  = 300
log_retention_runs       = 25

[categories]
# Controlled vocabulary for tagging tasks and knowledge. After editing,
# run `icarium category sync` (add `--prune` to remove deleted ones) to
# reconcile with the DB.
domains     = ["cli", "dispatch", "storage", "workflow"]
disciplines = ["haskell", "ops", "refinement"]
```

## Development

### Build

```sh
make build   # cabal build all
make install # copy binary to ./bin/icarium
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
