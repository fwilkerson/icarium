# icarium

icarium gives you three things in one local CLI:

- **Tasks** — a small backlog with states, priorities, and dependencies.
- **Knowledge** — domain notes you can attach to tasks; the dispatcher pulls relevant ones into the agent's prompt.
- **Dispatch** — runs a headless Claude agent against a ready task on its own branch; if `build` and `test` pass, the orchestrator fast-forward-merges into the integration branch.

Storage is a local SQLite DB. It's a tool for one developer, or a small team.

## For agents helping the user

### Config levers (`icarium.toml`)

- `[project] integration_branch` — branch dispatches FF-merge into.
- `[commands] build` / `test` — gate commands run after the agent commits. A failing gate marks the dispatch failed and moves the task to blocked.
- `[dispatch] model` / `effort` — defaults for the headless agent; override per-run with `dispatch run --model / --effort`.
- `[dispatch] tools` / `allowed_tools` — `tools` is what claude is told about; `allowed_tools` is the permission allowlist. Keep `allowed_tools` to read-only Bash plus the safe `icarium` mutation subset.
- `[dispatch] max_minutes_per_dispatch` / `max_dispatches_per_run` — guardrails for `dispatch run` draining the queue.
- `[categories] domains / disciplines` — controlled vocabulary for tagging. After editing, run `icarium category sync` (add `--prune` to remove deleted ones) to reconcile with the DB.

### CLI orientation

`icarium --help` and `<subcommand> --help` cover the surface. Non-obvious bits:

- The DB path defaults to `.icarium/icarium.db`; override with the top-level `--db`.
- `task add`, `know add`, and `task update` all accept `--body-file -` to read from stdin — use this for any non-trivial body to avoid shell quoting.
- `task show <id> --format prompt` renders the exact prompt the dispatcher will send. Use it to sanity-check a task before dispatching.

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
