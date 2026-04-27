# icarium CLI surface

All commands take `--db <path>` (default: `./.icarium/icarium.db`) and `--config <path>` (default: `./icarium.toml`).

Exit codes: `0` success, `1` not-found / no-op (e.g. `next` with empty queue), `2` validation error, `3` gate failure during dispatch.

## Project lifecycle

```
icarium init [--force]
    Create ./.icarium/icarium.db, apply schema, write ./icarium.toml
    with sensible defaults. --force overwrites existing config.

icarium doctor
    Validate config, schema version, git state, claude CLI presence,
    allowed-tools list. Prints a checklist; exit 2 on any failure.
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
    [--state <s> ...] [--ready]
    [--domain <name>] [--discipline <name>]
    Lists tasks grouped by state.

icarium task show <id> [--format human|prompt]
    Human view of task + linked knowledge + category-matched knowledge
    + deps. With --format prompt, prints the exact prompt the dispatcher
    would build.

icarium task update <id>
    [--state ...] [--priority ...] [--title ...]
    [--body ... | --body-file ...]     (use --body-file - for stdin)
    [--block-reason <text>]           (required iff --state blocked)

icarium task rm <id>

icarium task next
    Print the id of the next ready task (priority DESC, created_at ASC).
    Exit 1 if queue empty.
```

## Knowledge

```
icarium know add <title>
    [--body <text> | --body-file <path>]    (use --body-file - for stdin)
    [--domain <name> ...] [--discipline <name> ...]
    [--derived-from <id> ...]         (task or knowledge id)
    [--supersedes <knowledge-id>]

icarium know
    [--domain <name>] [--discipline <name>] [--stale] [--all]
    Lists knowledge entries (stale hidden by default; --all includes them).

icarium know show <id>

icarium know update <id>
    [--title ...] [--body ...] [--stale | --not-stale]

icarium know rm <id>
```

## Links and categories

```
icarium link add <src-id> <kind> <dst-id>
    kind ∈ depends-on | references | derived-from | supersedes
    Endpoint types validated against kind rules.

icarium link [--from <id>] [--to <id>] [--kind <kind>]
    Lists edges. Bare `link` is the same as the old `link list`.

icarium link rm <edge-id>

icarium category add <axis> <name>
    axis ∈ domain | discipline

icarium category [--axis <axis>]
    Lists categories. Bare `category` is the same as the old `category list`.

icarium category rm <axis> <name>
```

## Dispatch and run loop

```
icarium dispatch run [<task-id>]
    [--model <id>] [--effort low|medium|high]
    [--base-branch <name>]            (default: from config)
    [--dry-run]                       (print prompt + plan, do nothing)
    [--max <n>]                       (queue mode: cap dispatches)
    With TASK_ID: dispatch one task. Without: drain the ready queue
    in priority order until empty or --max reached.

icarium dispatch [--task <task-id>] [--outcome success|failure|interrupted]
    Tabular view of dispatches, newest first.
    Bare `dispatch` is the same as the old `dispatch list`.

icarium dispatch show <id>
    All columns of the dispatch row plus the linked task title.

icarium dispatch logs <id> [--tail N]
    Cat the event jsonl log for a dispatch. --tail prints last N lines.

icarium dispatch recover [DISPATCH_ID]
    Scan for dispatches with outcome IS NULL and (dead pid OR stale
    heartbeat). For each: mark outcome=interrupted, inspect branch
    state, move task to blocked with structured reason.
    With DISPATCH_ID, reconciles a single dispatch (prefix ok).
```

## Edge kinds

Edge kind display uses hyphens (`depends-on`, `derived-from`). This is the
same form `link add` accepts, so output from `link` can be round-tripped
back into `link add` without transformation.

## Agent-facing subset (called from inside a dispatch)

```
icarium task update <id> --state in_progress|done|blocked [--block-reason ...]
icarium task show <id>
icarium know add <title> --body-file - [--domain ...] [--discipline ...] [--derived-from <id>]
icarium link add <src> <kind> <dst>
```

Reads (`task show`, `know show`, `know`) are always permitted. Everything
else is discouraged via the system prompt and hard-blocked by `allowed_tools`
in `icarium.toml`.

## Output conventions

- Human output: aligned columns, no colors unless stdout is a TTY.
- Errors: stderr, prefix `icarium: error:`, exit non-zero.
- Heartbeat / event lines: stderr, prefix `[dispatch <short-id>]`.
