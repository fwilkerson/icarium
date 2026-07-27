# Slash commands in dispatched workers follow the Skill tool gate

Status: accepted (2026-07-19)

Dispatch passes `--disable-slash-commands` to headless workers **unless**
`"Skill"` appears in the `[dispatch] tools` list. No separate config knob:
adding `Skill` to `tools` *is* the opt-in. Default scaffold omits it, so
workers ship with skills off.

## Decision

Derive the flag rather than hardcoding it: `claudeArgs` emits
`--disable-slash-commands` iff `"Skill" ∉ tools`. One canonical control — the
tools list — no second boolean that can disagree with it. The flag hardens
against an over-broad tool surface; it is not a verdict against worker skills
(`docs/research/slash-command-archaeology.md`).

## Why the tools list is the whole gate

Verified empirically (headless runs under `dontAsk`):

- With the flag present, the Skill tool does not exist for the worker even
  when listed in `--tools` — the flag wins.
- Without the flag, `Skill ∈ --tools` alone suffices: **skill invocation is
  permission-exempt** — it runs without an `allowed_tools` entry and records
  no permission denial.

Because the permission layer never sees Skill calls, the tools list (plus the
derived flag) is the *only* gate on skill loading — hence default-off. What a
loaded skill *instructs* the worker to do remains bounded by
`tools`/`allowed_tools`/`dontAsk` as usual.

## Consequences

- Opting a project into worker skills is one edit: add `"Skill"` to
  `[dispatch] tools`. The `icarium init` scaffold comment documents this.
- Weigh that edit against what a loaded skill can do with the worker's turn:
  an unattended worker with skills available has burned minutes invoking
  `fewer-permission-prompts` and rewriting `.claude/settings.json` — on-task
  by the skill's own lights, not by the dispatch's. Skills that edit repo
  config are the sharp edge; `tools`/`allowed_tools` still bound what the
  skill can reach.
- Whether dispatch prompts *drive* skills is a separate policy question,
  settled in ADR 0004.
