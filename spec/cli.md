# icarium CLI surface (draft — review before implementation)

All commands take `--db <path>` (default: `./.icarium/icarium.db`) and `--config <path>` (default: `./icarium.toml`).

List commands support `--json` for machine-readable output. Default is human-readable table.

Exit codes: `0` success, `1` not-found / no-op (e.g. `next` with empty queue), `2` validation error, `3` gate failure during dispatch, `4` interrupted / recovery needed.

## Project lifecycle

```
icarium init [--force]
    Create ./.icarium/icarium.db, apply schema, write ./icarium.toml
    with sensible defaults. --force overwrites existing config.

icarium doctor
    Validate config, schema version, git state, claude CLI presence,
    allowed-tools list. Prints a checklist; exit 2 on any failure.

icarium export [--out <file>]
    Dump tasks/knowledge/edges/categories/dispatches as JSON. Used for
    backup and hand-off. Default: stdout.

icarium import <file>
    Apply a JSON export. Refuses if DB is non-empty without --merge.
```

## Tasks

```
icarium task add <title>
    [--body <text> | --body-file <path>]    (use --body-file - for stdin)
    [--state idea|planned|ready]           (default: planned)
    [--priority <int>]
    [--domain <name> ...] [--discipline <name> ...]
    [--depends-on <task-id> ...]
    [--references <knowledge-id> ...]
    Prints the new task id on success.

icarium task
    [--state <s> ...] [--ready] [--blocked]
    [--domain <name> ...] [--discipline <name> ...]
    [--json]

icarium task show <id> [--format human|json|prompt]
    Human view of task + linked knowledge + category-matched knowledge
    + deps. With --format prompt, prints the exact prompt the dispatcher
    would build. With --format json, machine-readable output.

icarium task update <id>
    [--state ...] [--priority ...] [--title ...]
    [--body ... | --body-file ...]     (use --body-file - for stdin)
    [--block-reason <text>]           (required iff --state blocked)

icarium task rm <id> [--force]
    Refuses if task has dispatches unless --force.
```

## Knowledge

```
icarium know add <title>
    [--body <text> | --body-file <path>]    (use --body-file - for stdin)
    [--domain <name> ...] [--discipline <name> ...]
    [--derived-from <id> ...]         (task or knowledge id)
    [--supersedes <knowledge-id>]

icarium know
    [--domain ...] [--discipline ...] [--stale] [--json]

icarium know show <id> [--format human|json]

icarium know update <id>
    [--title ...] [--body ...] [--stale BOOL]

icarium know rm <id> [--force]

icarium know prune
    List knowledge flagged stale (transitively via derived_from or
    supersedes). With --delete, remove them. Default: list only.
```

## Links and categories

```
icarium link add <src-id> <kind> <dst-id>
    kind ∈ depends_on | references | derived_from | supersedes
    Endpoint types validated against kind rules.

icarium link list [--from <id>] [--to <id>] [--kind ...] [--json]

icarium link rm <edge-id>

icarium category add <axis> <name>
    axis ∈ domain | discipline

icarium category list [--axis ...] [--json]

icarium category rm <axis> <name>
    Refuses if attached to any task/knowledge unless --force.
```

## Dispatch and run loop

```
icarium task next
    Print the id of the next ready task (priority DESC, created_at ASC).
    Exit 1 if queue empty.

icarium dispatch run <task-id>
    [--model <id>] [--effort low|medium|high]
    [--base-branch <name>]            (default: from config)
    [--dry-run]                       (print prompt + plan, do nothing)
    Creates dispatch row, cuts branch, invokes claude -p with streaming
    output. Prints live heartbeat lines to stderr. On success, FF-merges
    to base branch. Exit code reflects dispatch outcome.

icarium drain
    [--max <n>]                       (default: from config max_dispatches_per_run)
    [--until-empty]                   (loop until `task next` returns empty)
    [--model ...] [--effort ...]      (override defaults for this run)
    Pulls tasks until queue empty, max dispatches reached, or budget
    tripped. Prints one-line status per event (dispatch id, task, elapsed,
    current tool, short arg). Graceful shutdown on SIGINT: finish current
    dispatch, exit.

icarium status [--watch]
    Show open dispatches: id, task, branch, elapsed, heartbeat age,
    current tool. --watch refreshes every 2s.

icarium dispatch list [--task <id>] [--outcome success|failure|interrupted]
    Tabular view of dispatches, newest first.

icarium dispatch show <id>
    All columns of the dispatch row plus the linked task title.

icarium dispatch logs <id> [--tail N]
    Cat the event jsonl log for a dispatch. --tail prints last N lines.
```

## Recovery

```
icarium dispatch recover [DISPATCH_ID]
    Scan for dispatches with outcome IS NULL and (dead pid OR stale
    heartbeat). For each: mark outcome=interrupted, inspect branch
    state, move task to blocked with structured reason. Never discards
    uncommitted work; stashes with `git stash -u -m icarium:dispatch:<id>`
    if needed. With DISPATCH_ID, reconciles a single dispatch (prefix ok).

    Prints, for each reconciled dispatch:
      dispatch:<id> task:<id> branch:<b> uncommitted:<y|n>
      last_commit:<sha> action: blocked
```

## Agent-facing subset (called from inside a dispatch)

The headless agent is prompted to use these, and only these, for mutation:

```
icarium task update <id> --state in_progress|done|blocked [--block-reason ...]
icarium task show <id>
icarium know add <title> --body-file - [--domain ...] [--discipline ...] [--derived-from <id>]
icarium link add <src> <kind> <dst>
```

Reads (`task show`, `know show`, `know`) are always permitted. Everything else is discouraged via the system prompt and hard-blocked by `allowed_tools` in `icarium.toml` (icarium CLI is exposed as `Bash(icarium:*)`; we rely on the CLI itself to refuse destructive verbs when run inside a dispatch by checking an env var `ICARIUM_DISPATCH_ID`).

## Output conventions

- Human output: aligned columns, no colors unless stdout is a TTY.
- `--json`: stable schemas; fields documented alongside the schema doc.
- Errors: stderr, prefix `icarium: error:`, exit non-zero.
- Heartbeat / event lines: stderr, prefix `[dispatch <short-id>]`.

## Open points to confirm

1. DB path default `.icarium/icarium.db` — OK, or prefer `./icarium.db` flat?
2. Body input: do we want an `$EDITOR` flow on `task add` with no body flags (like `git commit`)? Nice ergonomics for humans, easy to skip in v0.
3. `--json` on every list or opt-in per command? I'm proposing universal.
4. Should `icarium drain` default to `--until-empty`, or require the flag? Safer to require, I think.
5. Agent-visible `task show --format prompt` — do we want this to also be what the dispatcher actually sends, or keep them separate paths? I'd make them literally the same code.
