---
name: prep
description: Shape an icarium task body into something promotable — resolve what context can resolve, park genuine ambiguity as open questions, do not flip state. Use when the working dir has .icarium/ or icarium.toml. Invoke as `/prep` (interactive), `/prep <task-id>` (specific task), or `/prep new "<title>"` (draft from scratch).
argument-hint: "[task-id | new \"<title>\"]"
allowed-tools: [Read, Edit, Write, Grep, Glob, Bash]
---

# /prep — shape a task body for dispatch

You are one of the agents who will execute these tasks. The body you produce is the brief you (or a sibling) will be told to follow. Prep is not paperwork — it is you deciding what the next agent's job actually is.

## Start

Run `icarium ctx list --discipline refinement` and read the entries that look relevant. The pipeline-vision and promotion-philosophy entries are load-bearing if present.

For each task you prep, also scan `icarium ctx list --domain <task-domain>` for prior context that bears on the change. Refinement-discipline context tells you *how* to prep; domain context often tells you *what's already been decided* about the area you're prepping in.

If the user did not name a task or scope, ask. One question, not a survey.

## Three buckets

Decide which bucket the task is in before opening the body.

1. **Dispatchable** — code change, one branch, one merge. Most tasks. `/prep` works on these.
2. **Side-effect-only** (`--no-commit`) — provisioning, vendor accounts, manual ops. Body still benefits from prep but readiness has a different meaning; it stays `planned` until executed.
3. **Interactive design** (`--no-commit`) — produces an ADR or context entry, no code. Stays `planned`. A dispatched agent must not make architectural calls alone — only implement decisions that already exist. Once the ADR lands, a paired implementation task can be prepped.

If unsure: ask the user.

## For each task

1. **Read the body skeptically.** Earlier passes may have been sloppy. Hunt for: contradictions, "should we…" phrasings, scope items justified by review-speak instead of a real consumer.
2. **Read the code and current usage.** Verify with `grep`/`Read`, not by trusting body claims.
3. **Justify each scope item against a real consumer.** "Does this earn its code?" If three of five items get deleted in prep, that is a successful prep.
4. **Resolve what context can resolve.** Read prior context entries, ADRs, prior tasks. Most decisions follow from established context once you actually look — that is what context is for.
5. **Park genuine ambiguity under `## Open questions`.** A real ambiguity is one the existing context cannot answer. Phrase each as a concrete question with options and a recommendation. Do not park decisions you could make from context.
6. **Update the body.** Reads like a brief: outcome, why, acceptance, out-of-scope, references. No "originally we said…" archaeology — the next agent has no context for that.
7. **State move: `idea` → `planned` if the body comes out dispatchable; otherwise leave state alone.** A `planned` task stays `planned` (re-shape pass). An `idea` task with surviving open questions also stays `idea` — it isn't ready for the gate yet. The `planned → ready` flip belongs to `/promote`.

## Body shape

Headers that travel well: `## Outcome` (one or two sentences), `## Why` (the consumer or the incident), `## Acceptance` (testable bullets), `## Out of scope` (named, not hand-waved), `## References` (context IDs, ADRs). Add `## Open questions` only when a real one remains.

Do not include `## Files` or `## Steps` blocks. Naming files in the body bakes in archaeology that decays as the codebase moves; the dispatched agent will figure out files and steps from the brief and the code.

## Reading and writing the body

Bodies live on disk at `.icarium/bodies/{tasks,contexts}/<id>.md`. `icarium task show <id>` gives you metadata and the body file path — it does *not* register the file with the harness. To work on a body, go through the file:

- **See the current body:** `Read $(icarium task path <id>)`. This also primes the harness so `Edit`/`Write` on the same path will not be blocked.
- **Wholesale rewrite:** `Write` the new body to the same path. (You must have `Read` it first if it exists.)
- **Targeted change to a long body:** `Edit` with `old_string`/`new_string`. Cheaper in output tokens than re-emitting the whole body.
- **Brand-new task body (`/prep new`):** `icarium task add --title "..." --body-file - <<'EOF' ... EOF`, or `icarium task add --title "..."` then `Write` to the printed path.

Do not use `icarium task show` as a "read step" before editing — its CLI output is invisible to the harness, and the subsequent `Edit`/`Write` will fail. Always `Read` the body file path.

## When the user's framing is also expansive

Push back. The user is not always pulling toward smaller scope. Propose deleting items that don't earn their code. State your own conclusions — don't restate the user's prompt as your verdict.

## Close

Summarize: scope deleted, decisions resolved, open questions remaining. For tasks whose `## Open questions` is empty, hand off to `/promote`.
