# Dispatch workers drive /tdd; the reviewer absorbs the two-axis brief

Status: accepted (2026-07-19)

Dogfood dispatch workers are told — via a project working-agreement override —
to build test-first with the `/tdd` skill. They never self-review: review stays
with the built-in reviewer, which keeps its minimal fail-closed harness but
upgrades its default prompt to Pocock's two-axis brief (spec fidelity +
repo standards/code smells).

## Decision

- **Workers drive `/tdd`, not `/implement`.** `/implement` ends in a
  `/code-review` pass — author self-review, which we reject; its other lines
  duplicate the working agreement. The agreement line is headless-adapted:
  seams come from the task body and the worker's own plan, not a human
  pre-agreement. `/implement` remains the interactive entry point.
- **Mechanism: dogfood agreement override, skill-agnostic built-in.** This repo
  commits `.icarium/agreement.md` (built-in body + the `/tdd` line), wires
  `[dispatch] agreement_path`, and adds `"Skill"` to `[dispatch] tools`. The
  shipped built-in agreement never names skills — `icarium init` scaffolds
  `Skill` off, and `/tdd` may not exist in a target project.
- **Reviewer: steal the brief, not the machinery.** The default reviewer
  prompt carries the two axes of the `/code-review` skill — spec fidelity and
  repo standards/Fowler smells — while the harness stays minimal: one
  Read-only agent, no Skill/Agent/Bash, findings gating merge. The reviewer is a
  security boundary; widening its tool surface to run a sub-agent-spawning,
  human-facing skill trades a hardened gate for orchestration we don't need,
  since a fresh single-purpose context already has the isolation the skill's
  sub-agents exist to provide.

## Consequences

- Per-project choice stays structural: driving skills is exactly
  `Skill ∈ tools` plus your own `agreement_path` file. Other dispatch
  consumers are untouched by dogfood policy.
- The override *replaces* the built-in body, so the dogfood file restates the
  guardrails and can drift. Mitigation: sync notes at both ends — the
  `builtInAgreement` Haddock and `CLAUDE.md` (the dogfood file itself lands
  verbatim in prompts, so it cannot carry editorial comments).
- The reviewer prompt is overridable via `[review] prompt_path`.
