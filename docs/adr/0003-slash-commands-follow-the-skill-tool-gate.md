# Slash commands in dispatched workers follow the Skill tool gate

Status: accepted (2026-07-19)

Dispatch passes `--disable-slash-commands` to headless workers **unless**
`"Skill"` appears in the `[dispatch] tools` list. No separate config knob:
adding `Skill` to `tools` *is* the opt-in. Default scaffold omits it, so
workers ship with skills off.

## Context

`--disable-slash-commands` was hardcoded in `52f2798` (2026-04-26) after a
post-mortem: config passed only `--allowedTools` (auto-approve, not a gate),
so workers retained Skill/Agent/Cron* and one run invoked
`fewer-permission-prompts`, thrashing ~6 min rewriting `.claude/settings.json`.
The same commit added the real gate (`tools` vs `allowed_tools`). The flag was
hardening against an over-broad surface, not a verdict against worker skills
(see `docs/research/slash-command-archaeology.md`).

## Decision

Derive the flag instead of hardcoding it: `claudeArgs` emits
`--disable-slash-commands` iff `"Skill" ∉ tools`. One canonical control — the
tools list — no second boolean that can disagree with it.

## Why the tools list is the whole gate

Verified empirically (2026-07-19, headless runs under `dontAsk`):

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
- Whether dispatch prompts should *drive* skills (e.g. `/implement`) is a
  separate, now-unblocked policy decision.
