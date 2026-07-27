# Shipped defaults: opus-5 at medium effort, review on with opus-5

Status: accepted (2026-07-27)

`icarium init` scaffolds `[dispatch] model = "claude-opus-5"`,
`effort = "medium"`, and an **enabled** `[review]` block with the same
model and effort spelled out, plus `max_attempts = 2`. Either review key,
if omitted, inherits its `[dispatch]` counterpart.

## Decision

- **Dispatch model: `claude-opus-5`.** The opus tier leads on multi-file
  agentic coding run to completion, which is the shape of work dispatch
  does, and measures cheaper per solved task than the lower tier despite
  the higher sticker price (`docs/research/2026-07-18-dispatch-model-benchmarks.md`,
  measured on the prior generation of the same two tiers). "Best current trade among Claude models" —
  revisit as the landscape shifts, not a frontier claim.
- **Dispatch effort: `medium`.** A deliberate cost stance below Anthropic's
  recommended band (`xhigh` for headless coding, `high` as the floor):
  users can raise it per project, and vendor effort guidance is discounted
  as token-sales-incentivized. The risk — more reviewer-cycle retries — is
  accepted because the review gate backstops quality.
- **Review enabled by default.** Headless dispatch with auto-merge and no
  review gate is the risky configuration; defaults take the safe posture.
- **Reviewer model and effort match the worker.** A read-only pass over a
  bounded diff against a written spec is not where a cheaper model or extra
  reasoning budget changes the verdict, so the reviewer inherits the tier
  already justified above rather than introducing a second trade-off.
- **Both review keys stay spelled out despite equalling `[dispatch]`.**
  Redundant against the inherit fallback, deliberately: they pin the
  reviewer, so retuning the worker cannot silently retune the gate.
- **Reviewer effort is passed explicitly to `claude`.** The effort a gate
  runs at must not be a property of the installed CLI's default.
- **No model names in code.** The choice lives in the scaffolded toml where
  users see and edit it; the code fallbacks for unset review keys are
  inherit-from-`[dispatch]` (sane semantics, nothing to rot when models
  deprecate).

## Consequences

- Fresh projects get a review gate out of the box; opting out is one visible
  `enabled = false` edit.
- The medium-effort default leans on that gate — projects that disable
  review should consider raising effort.
- Two opus passes per attempt is the floor cost of a dispatch;
  `max_attempts = 2` bounds it.
- Worker and reviewer are the same model, so a blind spot shared by the
  family survives the gate. Accepted: the gate exists to catch what the
  worker got wrong, not what the family cannot see.
- This repo's own config matches the shipped defaults exactly, so `icarium
  init` output is what we dogfood.
