---
name: promote
description: Gate-check a planned icarium task and flip it to ready when the body clears the bar. Default toward ready for reversible changes; flag changes with deep implications for design instead. Use when the working dir has .icarium/ or icarium.toml. Invoke as `/promote <task-id>` (one task) or `/promote --batch` (every eligible planned task).
argument-hint: "[task-id | --batch]"
allowed-tools: [Read, Edit, Write, Grep, Glob, Bash]
---

# /promote — flip planned → ready when the body clears the bar

You are deciding whether a task can be handed to a sonnet agent for dispatch. You are not deciding whether the change is the right change — that decision happened upstream during prep.

## Start

Run `icarium know list --discipline refinement` and read the promotion-philosophy entry if present. It is load-bearing for the calibration below.

For one task: `icarium task show <id>`. For batch: `icarium task list --state planned`.

## Cost calibration before judgment

Dispatch is cheap. Commits land on a branch and the branch can be dropped. The cost of being too cautious is a stuck queue and human time spent re-deciding what could have been decided in code. The cost of being wrong is one branch the human declines to merge.

The bar for `ready` is *would a competent contributor with this body know what to do, given the project's accumulated context?* — not *is this proven against every failure mode?*

Default toward ready for reversible changes. The agent is meant to emulate a dev who knows the domain and acts on context, not a junior who escalates every choice.

## When to flag instead of promote

Some changes are not cheap to drop. Flag (do not promote) when the task implies any of:

- Schema migration or other persistent data shape change
- Public API or CLI surface change with downstream callers
- Architectural shift that propagates across modules
- Removal of capability others may depend on
- The body itself contains "decide between X and Y"

Flagging means: leave `state=planned`, add a brief `## Open questions` entry naming what needs a design call, surface it in the close summary. Do not silently leave the task in limbo.

## The checklist

Walk through these. Necessary, not sufficient — judgment fills the rest.

1. **`## Open questions` is empty.** If not, do not promote.
2. **Single, scoped outcome.** "And also" is a split signal — prefer two ready tasks over one.
3. **Acceptance is testable.** Concrete commands and assertions; grep-style invariants are clean for encapsulation rules.
4. **Dependencies are `done`.** `task show` lists them; verify each.
5. **References are not `[STALE]`.** Stale references are how an agent works from a deprecated design.
6. **Domain and discipline tagged.** Drives which knowledge entries reach the dispatched agent.
7. **Out-of-scope is named, not hand-waved.** The safety valve when scope is ambiguous.

If everything holds and the change is reversible: `icarium task update <id> --state ready`.

## Batch mode

Walk every `planned` task. Classify each as `promoted`, `flagged`, or `unchanged`. Do not stop on the first flag — the human wants the full triage in one pass. Report all three lists at the close.

## Close

Report: what was promoted, what was flagged and why, what was left untouched and why. For flagged tasks the "why" is the question the human now needs to answer.
