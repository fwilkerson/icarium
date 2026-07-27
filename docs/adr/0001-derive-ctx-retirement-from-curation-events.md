# Derive context retirement from curation events

Status: accepted (2026-07-17)

A ctx entry's visibility is *derived* from an append-only `context_curation`
table (disposition + artifact + note per event): current = never curated or
latest disposition `keep`; retired = latest disposition `guidance`/`rule`/
`refactor`/`stale`. There is no stored staleness flag, so recording a
curation event is the only way to retire or revive an entry — the schema
itself pushes users toward the curation sweep.

## Considered options

- A stored flag alongside a curation record: rejected — dual-write coupling,
  two sources of truth for visibility.
- Columns on `context` (`curated_at`, `disposition`): rejected — each sweep
  overwrites the last; no history, no reversal trail.

## Consequences

- Un-retiring is a later `keep` event, not a flag flip; reversals leave
  history.
- Prompt injection: category-based injection uses current entries only;
  explicit `references` edges deliver an entry regardless of retirement —
  except disposition `stale`, which is never injected (known-wrong content).
  A `refactor`-retired entry thus still travels with the refactor task that
  references it.
