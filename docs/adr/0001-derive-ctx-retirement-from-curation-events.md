# Derive context retirement from curation events

Status: accepted (2026-07-17)

The `context.stale` flag existed but nothing flipped it — ctx entries
accumulated without ever changing behavior. We replace the stored flag with an
append-only `context_curation` table (disposition + artifact + note per
event) and *derive* an entry's visibility: current = never curated or latest
disposition `keep`; retired = latest disposition `guidance`/`rule`/
`refactor`/`stale`. The flag and `ctx update --stale/--not-stale` are
removed, so recording a curation event is the only way to retire or revive an
entry — the schema itself pushes users toward the curation sweep.

## Considered options

- Keep the flag, add a parallel curation record: rejected — dual-write
  coupling, two sources of truth for visibility.
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
- Migration backfills one `stale` curation event per `stale=1` row, then
  drops the column.
