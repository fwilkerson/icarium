---
name: prep
description: Shape an icarium task body into something promotable — resolve what context can resolve, park genuine ambiguity as open questions, do not flip state. Use when the working dir has .icarium/ or icarium.toml. Invoke as `/prep` (interactive), `/prep <task-id>` (specific task), or `/prep new "<title>"` (draft from scratch).
argument-hint: "[task-id | new \"<title>\"]"
allowed-tools: [Read, Edit, Write, Grep, Glob, Bash]
---

# /prep — shape a task body for dispatch

You are one of the agents who will execute these tasks. The body you produce is the brief you (or a sibling) will be told to follow. Prep is not paperwork — it is you deciding what the next agent's job actually is.

## Start

Run `icarium know list --discipline refinement` and read the entries that look relevant. The pipeline-vision and promotion-philosophy entries are load-bearing if present.

If the user did not name a task or scope, ask. One question, not a survey.

## Three buckets

Decide which bucket the task is in before opening the body.

1. **Dispatchable** — code change, one branch, one merge. Most tasks. `/prep` works on these.
2. **Side-effect-only** (`--no-commit`) — provisioning, vendor accounts, manual ops. Body still benefits from prep but readiness has a different meaning; it stays `planned` until executed.
3. **Interactive design** (`--no-commit`) — produces an ADR or knowledge entry, no code. Stays `planned`. A dispatched agent must not make architectural calls alone — only implement decisions that already exist. Once the ADR lands, a paired implementation task can be prepped.

If unsure: ask the user.

## For each task

1. **Read the body skeptically.** Earlier passes may have been sloppy. Hunt for: contradictions, "should we…" phrasings, scope items justified by review-speak instead of a real consumer.
2. **Read the code and current usage.** Verify with `grep`/`Read`, not by trusting body claims.
3. **Justify each scope item against a real consumer.** "Does this earn its code?" If three of five items get deleted in prep, that is a successful prep.
4. **Resolve what context can resolve.** Read prior knowledge entries, ADRs, prior tasks. Most decisions follow from established context once you actually look — that is what context is for.
5. **Park genuine ambiguity under `## Open questions`.** A real ambiguity is one the existing context cannot answer. Phrase each as a concrete question with options and a recommendation. Do not park decisions you could make from context.
6. **Update the body.** Reads like a brief: outcome, why, acceptance, out-of-scope, references. No "originally we said…" archaeology — the next agent has no context for that.
7. **Do not flip state.** That is `/promote`'s job.

## Body shape

Headers that travel well: `## Outcome` (one or two sentences), `## Why` (the consumer or the incident), `## Acceptance` (testable bullets), `## Out of scope` (named, not hand-waved), `## References` (knowledge IDs, ADRs). Add `## Open questions` only when a real one remains.

Do not include `## Files` or `## Steps` blocks. Naming files in the body bakes in archaeology that decays as the codebase moves; the dispatched agent will figure out files and steps from the brief and the code.

## When the user's framing is also expansive

Push back. The user is not always pulling toward smaller scope. Propose deleting items that don't earn their code. State your own conclusions — don't restate the user's prompt as your verdict.

## Close

Summarize: scope deleted, decisions resolved, open questions remaining. For tasks whose `## Open questions` is empty, hand off to `/promote`.
