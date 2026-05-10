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

## Reading the shape of the change

Some changes are cheap to drop on a branch. Others propagate — a schema migration, a public API or CLI surface change, an architectural shift across modules, a removal of capability downstream code may depend on. The categorical fact "this is a schema migration" or "this changes a public API" is *not* the flag signal. The signal is whether the design pass for that propagation has happened.

For a propagating change, ask:

- **Are the touch points enumerated?** Files, modules, callers, data shapes, protocol versions — whatever the change reaches.
- **Are the load-bearing decisions locked?** Names, value mappings, ordering, cutover behavior — the calls a competent contributor would otherwise have to make and might guess wrong on.
- **Is the migration or rollout sketched?** How does old → new transition? What happens to in-flight state? Is there a rollback path that's actually a path?
- **Are downstream effects named — even just to say "no downstream callers"?**

If yes to all: the change is propagating but the design is done. Promote — the dispatched agent is executing a plan, not making product calls.

If any answer is no: flag. Cost of being too cautious is one round-trip with the human; cost of being too aggressive is a poorly-designed change that has to be unwound.

The body itself sometimes signals incompleteness — phrases like "decide between X and Y", "TBD", or "we should figure out…" mean the design pass is unfinished regardless of category. Flag those.

## The checklist

Walk through these. Necessary, not sufficient — judgment fills the rest.

1. **`## Open questions` is empty.** If not, do not promote.
2. **Single, scoped outcome.** "And also" is a split signal — prefer two ready tasks over one.
3. **Acceptance is testable.** Concrete commands and assertions; grep-style invariants are clean for encapsulation rules.
4. **Dependencies are `done`.** `task show` lists them; verify each.
5. **References are not `[STALE]`.** Stale references are how an agent works from a deprecated design.
6. **Domain and discipline tagged.** Drives which knowledge entries reach the dispatched agent.
7. **Out-of-scope is named, not hand-waved.** The safety valve when scope is ambiguous.

If everything holds: `icarium task update <id> --state ready`.

## When you flag

Leave `state=planned`. Surface the unanswered questions — the goal is a design pass, not silent limbo. Two ways to surface, by session shape:

- **Interactive (default):** ask the human directly via `AskUserQuestion` with a short proposal and the trade-offs you see. Faster, no body churn, the conversation captures the resolution.
- **Headless or hand-off:** write a brief `## Open questions` section into the task body so the questions travel with the task. Use when you can't get a synchronous answer — not as the default.

## After the human answers

Once a parked question has an answer, the design is complete enough to promote. Follow-ups, in order:

1. **Lock the decisions into the body.** Fold answers into Outcome / Why or add a short locked-decisions block. The body should read as if the design call were always settled — future agents shouldn't have to reconstruct the conversation.
2. **Split if the answer revealed a sub-task.** If the resolution introduces separable work (e.g. "do the migration interactively, dispatch the doc sweep"), create the follow-up with `--depends-on` and scope this task down to match.
3. **Promote.** Run `icarium task update <id> --state ready`.

Keep this light. Capture the resolution and adjust scope; don't over-document.

## Batch mode

Walk every `planned` task. Classify each as `promoted`, `flagged`, or `unchanged`. Do not stop on the first flag — the human wants the full triage in one pass. Report all three lists at the close.

## Close

Report: what was promoted, what was flagged and why, what was left untouched and why. For flagged tasks the "why" is the question the human now needs to answer.
