#!/usr/bin/env bash
# Seed the icarium task DB with the initial dogfood task list.
#
# Assumptions:
#   - ./bin/icarium exists (run: make install — or `cabal install ...`).
#   - ./bin/icarium init has been run (creates .icarium/icarium.db).
#
# This script is NOT idempotent: running it twice creates duplicate
# tasks. It's the bootstrap until `icarium export`/`import` lands and
# we can version-control snapshots instead.
#
# Note: bodies live in temp files because macOS ships bash 3.2 which
# mis-parses heredocs inside $() when the body contains unbalanced
# parens. Heredocs here feed `cat`, not command substitutions.

set -euo pipefail
I=./bin/icarium

T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

# -------------------------------------------------------------
# Categories — also listed in icarium.toml for reference.
# `category add` is idempotent; it prints the existing id if present.
# -------------------------------------------------------------
$I category add domain cli         >/dev/null
$I category add domain dispatch    >/dev/null
$I category add domain storage     >/dev/null
$I category add domain workflow    >/dev/null
$I category add discipline haskell >/dev/null
$I category add discipline ops     >/dev/null

# -------------------------------------------------------------
# Knowledge — only what's non-obvious and load-bearing.
# -------------------------------------------------------------

cat > "$T/k_pinned" <<'EOF'
The headless agent invoked by `icarium dispatch` must call `./bin/icarium`
from the project root, NOT `cabal run icarium --` or the raw dist-newstyle
path. `./bin/icarium` is the pinned install produced by
`cabal install --installdir=./bin --install-method=copy --overwrite-policy=always exe:icarium`.

Rationale: if the agent's changes break the live build, the pinned binary
keeps working — the agent can still mark its own dispatch done/blocked.
We never want the tool running the workflow to depend on the code being
modified.

Reinstall the pinned binary after landing changes to icarium itself.
EOF
K_PINNED=$($I know add "Dispatched agents use ./bin/icarium" \
    --body-file "$T/k_pinned" \
    --domain workflow --discipline ops)

cat > "$T/k_prompt_lockstep" <<'EOF'
`renderTaskPrompt` in `src/Icarium/Render.hs` is used by BOTH:
  - `icarium task show --format prompt` (preview)
  - `Icarium.Dispatch.buildPrompt` (what actually ships to claude)

Changing it affects both. If you adjust the working-agreement text, the
section headers, or the ordering, test with `task show --format prompt` first —
that's the cheapest way to see the exact bytes the dispatcher will send.
EOF
K_PROMPT_LOCKSTEP=$($I know add "Prompt rendering is shared code" \
    --body-file "$T/k_prompt_lockstep" \
    --domain dispatch --discipline haskell)

cat > "$T/k_edge_typing" <<'EOF'
Edge kinds and their allowed endpoints:
  depends-on   : task -> task
  references   : task -> knowledge
  derived-from : knowledge -> task or knowledge
  supersedes   : knowledge -> knowledge

Self-edges are forbidden. These rules are enforced by CHECK constraints
and triggers in spec/schema.sql AND by Icarium.Commands.Link.checkTyping
so users get a friendly error instead of SQLITE_CONSTRAINT.

If you add a new edge kind, update BOTH the schema and Link.checkTyping,
or pick up a friendlier error than the raw constraint.
EOF
K_EDGE_TYPING=$($I know add "Typed edge rules" \
    --body-file "$T/k_edge_typing" \
    --domain storage --discipline haskell)

# -------------------------------------------------------------
# Tasks
# Priority convention: 10 = foundation, 5 = normal, 1 = low.
# -------------------------------------------------------------

cat > "$T/t_makefile" <<'EOF'
Add a `Makefile` at the project root. Targets:

- build    -> cabal build all
- install  -> cabal install --installdir=./bin --install-method=copy --overwrite-policy=always exe:icarium
- test     -> cabal test all  (no-op fine until a suite exists)
- clean    -> cabal clean

Declare `.PHONY: build install test clean`. After `make install`, `./bin/icarium`
must be a working binary. Verify with `./bin/icarium doctor`.
EOF
T_MAKEFILE=$($I task add \
    "Add Makefile with build/install/test/clean targets" \
    --state ready --priority 10 \
    --domain workflow --discipline ops --discipline haskell \
    --references "$K_PINNED" \
    --body-file "$T/t_makefile")

cat > "$T/t_dispatch_sub" <<'EOF'
Currently `icarium dispatch TASK_ID` is the only form. We need inspection
commands, which requires nesting.

Rename the existing invocation to `icarium dispatch run TASK_ID` and add:

- icarium dispatch list [--task ID] [--outcome success|failure|interrupted]
  Tabular view using Icarium.Repo.Dispatch.listDispatches. Columns:
  id, task_id, branch, outcome, started_at. Order by started_at DESC.

- icarium dispatch show <id>
  All columns of the dispatch row. Include the linked task title for
  context.

- icarium dispatch logs <id>
  cat .icarium/logs/<id>.jsonl basically. Bonus: --tail N.

Update:
- src/Icarium/Commands/Dispatch.hs to use subparser
- app/Main.hs if needed
- spec/cli.md to reflect the new surface

Smoke test each subcommand with ./bin/icarium dispatch ...
EOF
T_DISPATCH_SUB=$($I task add \
    "Restructure dispatch into subcommands (run, list, show, logs)" \
    --state ready --priority 9 \
    --domain cli --domain dispatch --discipline haskell \
    --references "$K_PINNED" \
    --body-file "$T/t_dispatch_sub")

cat > "$T/t_claude_flags" <<'EOF'
Icarium.Dispatch.runClaudeStreaming passes:
  claude -p --model <m> --output-format stream-json --verbose
         --allowedTools "<comma-list>"

These flag names are my best guess. Verify against `claude --help` on
the installed CLI version. If any flag is wrong, fix the args list in
runClaudeStreaming and leave a knowledge entry recording:
  - the claude CLI version tested
  - the exact flag names we ended up using
  - any flags the CLI has for reasoning effort see "Wire effort" task

Do NOT change the behavior shape: prompt on stdin, stream-json stdout
to log file, ICARIUM_DISPATCH_ID / ICARIUM_TASK_ID env set. Only the
flag names.

Acceptance: a real `./bin/icarium dispatch <trivial-task>` completes
end-to-end on a scratch branch.
EOF
T_CLAUDE_FLAGS=$($I task add \
    "Validate claude -p flags against real invocation" \
    --state ready --priority 9 \
    --domain dispatch --discipline ops --discipline haskell \
    --references "$K_PINNED" --references "$K_PROMPT_LOCKSTEP" \
    --body-file "$T/t_claude_flags")

cat > "$T/t_export" <<'EOF'
Add `icarium export [FILE]`. Default: stdout.

Dump all tasks, knowledge, edges, categories, and dispatches as a single
JSON object:

    { "schema_version": 1,
      "tasks":       [ ... ],
      "knowledge":   [ ... ],
      "edges":       [ ... ],
      "categories":  [ ... ],
      "dispatches":  [ ... ] }

Field names in each element match the DB column names in snake_case,
so the output is stable and greppable. Use aeson; add `aeson` to
build-depends.

This unblocks committing a reviewable JSON snapshot of the dogfood
DB to version control, and is a prerequisite for `icarium import`.
EOF
T_EXPORT=$($I task add \
    "Add icarium export command (JSON dump)" \
    --state ready --priority 8 \
    --domain storage --discipline haskell \
    --references "$K_EDGE_TYPING" \
    --body-file "$T/t_export")

cat > "$T/t_import" <<'EOF'
Read the JSON produced by `icarium export` and insert everything into
the DB.

- Refuse to run against a non-empty DB unless --merge is passed.
- Validate schema_version; refuse unknown versions.
- On any insert failure, roll back the whole import. Use a SQLite
  transaction — `withTransaction` from sqlite-simple.
- Preserve the original ULIDs so edges keep resolving.

Depends on the export command landing first.
EOF
T_IMPORT=$($I task add \
    "Add icarium import command" \
    --state planned --priority 8 \
    --domain storage --discipline haskell \
    --depends-on "$T_EXPORT" \
    --body-file "$T/t_import")

cat > "$T/t_tests" <<'EOF'
Add a tasty or hspec test suite covering at least:

- Round-trip for parseTaskState / taskStateText
- Round-trip for parseEdgeKind / edgeKindText
- Round-trip for parseEffort / effortText
- Round-trip for parseCategoryAxis / categoryAxisText
- Icarium.Config.loadConfig succeeds on the default template
- Icarium.Render.renderTaskPrompt produces something non-empty for a
  minimal task + empty refs + empty deps

Wire into icarium.cabal as a test-suite stanza. Then update icarium.toml
in this project to `test = "cabal test all"` so the dispatch test gate
starts being real.

Acceptance: cabal test all passes; ./bin/icarium doctor still ok.
EOF
T_TESTS=$($I task add \
    "Add minimal test suite and enable test gate" \
    --state ready --priority 7 \
    --domain workflow --discipline ops --discipline haskell \
    --body-file "$T/t_tests")

cat > "$T/t_log_retention" <<'EOF'
After a dispatch finishes — success, failure, or interrupted — delete
.icarium/logs/<id>.jsonl files beyond the N most recent dispatches,
where N = config's log_retention_runs default 25.

Ordering: dispatches.started_at DESC. Delete files whose dispatch row
is outside the top N. For v0 just the log files; row retention can
be a follow-up.

Call it from Icarium.Dispatch.finishWith after RD.finishDispatch.
EOF
T_LOG_RETENTION=$($I task add \
    "Prune dispatch logs per log_retention_runs" \
    --state ready --priority 4 \
    --domain storage --domain dispatch --discipline haskell \
    --body-file "$T/t_log_retention")

cat > "$T/t_effort" <<'EOF'
The Effort value low|medium|high flows from config through
DispatchRequest and is stored on the dispatch row. It is NOT passed
to the claude CLI yet, because we did not know the correct flag.

Once T_CLAUDE_FLAGS — the claude-flag validation task — is done and
has left a knowledge entry documenting the actual flag, add it to
the args list in Icarium.Dispatch.runClaudeStreaming.

Verify with ./bin/icarium dispatch <task> --effort high — the flag
should appear in the claude invocation.
EOF
T_EFFORT=$($I task add \
    "Wire effort flag into claude invocation" \
    --state planned --priority 6 \
    --domain dispatch --discipline haskell \
    --depends-on "$T_CLAUDE_FLAGS" \
    --body-file "$T/t_effort")

cat > "$T/t_sigint" <<'EOF'
icarium dispatch run should catch SIGINT. On first Ctrl-C: finish the
current dispatch, then exit cleanly without picking a new task. On
second Ctrl-C: propagate hard kill.

Use System.Posix.Signals.installHandler with a shared MVar flag the
loop checks between dispatches. Do not try to kill an in-flight
claude process; let it finish the task it started.

Test: start ./bin/icarium dispatch run --max 5 --dry-run, Ctrl-C
during the loop, verify it stops cleanly.
EOF
T_SIGINT=$($I task add \
    "Graceful SIGINT handling in run loop" \
    --state planned --priority 3 \
    --domain workflow --discipline haskell \
    --body-file "$T/t_sigint")

cat > "$T/t_staleness" <<'EOF'
When a knowledge row is superseded — new knowledge supersedes old —
mark the old as stale and cascade: any knowledge derived-from the
old also becomes stale, transitively.

When a knowledge's source task has its body edited, all knowledge
derived-from that task should be marked stale.

Open questions before implementing:
  - Trigger-based vs an explicit `icarium know recompute-stale` pass?
  - Should cascade be bounded to prevent runaway updates?
  - How do we re-un-stale an entry once a human has verified it's
    still accurate? manual `know update --stale false` — already exists.

This is marked idea deliberately: think before implementing.
EOF
T_STALENESS=$($I task add \
    "Knowledge staleness propagation" \
    --state idea \
    --domain storage --domain workflow --discipline haskell \
    --references "$K_EDGE_TYPING" \
    --body-file "$T/t_staleness")

cat > "$T/t_know_prune" <<'EOF'
Depends conceptually on the staleness-propagation work.

icarium know prune [--dry-run]:
  - With --dry-run: lists knowledge rows where stale=1.
  - Without: prompts for confirmation, then deletes them.
  - Cascade: the schema's edges_cascade_knowledge_delete trigger will
    handle edge cleanup.

Also marked idea — decide workflow interactive vs scripted before
implementing.
EOF
T_KNOW_PRUNE=$($I task add \
    "icarium know prune deletes stale knowledge" \
    --state idea \
    --domain storage --discipline haskell \
    --body-file "$T/t_know_prune")

echo ""
echo "--- seeded ---"
$I task
