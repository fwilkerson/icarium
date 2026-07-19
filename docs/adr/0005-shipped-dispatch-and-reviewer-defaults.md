# Shipped defaults: opus-4-8 at medium effort, review on with sonnet-5

Status: accepted (2026-07-19)

`icarium init` scaffolds `[dispatch] model = "claude-opus-4-8"`,
`effort = "medium"`, and an **enabled** `[review]` block with
`model = "claude-sonnet-5"`, `max_attempts = 2`. The code fallback for an
unset `review.model` stays inherit-`dispatch.model`.

## Context

The scaffold shipped sonnet-5 / high for dispatch and the whole `[review]`
block commented out (review disabled). Benchmark research
(`docs/research/2026-07-18-dispatch-model-benchmarks.md`):

- opus-4-8 leads SWE-bench Verified 88.6% vs 82.1% and, despite the higher
  sticker price, sonnet-5 measures ~15% *more* expensive per solved task at
  high/max effort — sonnet is only the value pick at low/medium effort.
- Anthropic recommends `xhigh` for headless coding with `high` as the floor.
- No evidence sonnet-5 is inadequate as a reviewer/judge.

## Decision

- **Dispatch model: `claude-opus-4-8`.** Better and effectively cheaper per
  solved task for headless coding. "Best current trade among Claude models" —
  revisit as the model landscape shifts, not a frontier claim.
- **Dispatch effort: `medium`.** A deliberate cost stance below Anthropic's
  recommended band: deep-SWE benchmarks justify medium as the floor, users
  can raise it per project, and vendor effort guidance is discounted as
  token-sales-incentivized. The risk (more reviewer-cycle retries) is
  accepted because the review gate backstops quality.
- **Review enabled by default, reviewer `claude-sonnet-5`.** Headless
  dispatch with auto-merge and no review gate is the risky configuration;
  defaults take the safe posture. Review is a bounded read-only pass, so the
  per-solved-task economics that ruled sonnet out for dispatch don't apply —
  sticker price ($2/$10 vs $5/$25 per MTok) dominates, and a cross-model
  reviewer avoids opus grading opus.
- **No model names in code.** The sonnet-5 choice lives in the scaffolded
  toml where users see and edit it; the code fallback for unset
  `review.model` remains inherit-`dispatch.model` (sane semantics, nothing
  to rot when models deprecate).

## Consequences

- Fresh projects get a review gate out of the box; opting out is one visible
  `enabled = false` edit.
- The medium-effort default leans on that gate — projects that disable
  review should consider raising effort.
- This repo's own config (opus-4-8 / high) already exceeds the shipped
  defaults; no dogfood change.
