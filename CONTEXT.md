# Icarium

A workflow store for headless-agent development: tasks, durable context, and
dispatch state in one sqlite DB, so dispatched agents inherit accumulated
project reasoning instead of starting cold.

A dispatch prompt is built from the task body plus its explicitly
`references`-linked ctx entries, ctx entries matching the task's
domain/discipline categories (capped, deduped), and its dependency tasks —
so durable rationale captured as tagged ctx entries reaches future agents in
that space automatically. Storage split: bodies are files, the DB keeps the
relational layer (ADR 0002).

## Language

### Context lifecycle

**Context entry (ctx)**:
A durable knowledge record (markdown body + categories + edges) injected into
dispatch prompts by category match or explicit reference.
_Avoid_: knowledge entry (renamed in migration 0005)

**Curation event**:
An append-only record of one disposition decision for a context entry. History
is never rewritten; the latest event wins.
_Avoid_: triage (claimed by issue intake), audit (the subsumed older name)

**Disposition**:
The outcome of curating an entry: `guidance` (promoted to doc/skill), `rule`
(codified as lint/invariant/test), `refactor` (friction filed as a task),
`keep` (stays useful as retrieval context), or `stale` (no longer true or
useful).

**Current / Retired**:
Derived visibility of a context entry. Current = never curated, or latest
disposition is `keep`. Retired = latest disposition is `guidance`, `rule`,
`refactor`, or `stale`. There is no stored staleness flag; the state is
derived from curation events.
_Avoid_: stale flag (the dropped `context.stale` column)

**Artifact**:
The pointer on a curation event to where the content went: a task id
(`refactor`), rule/test name (`rule`), or doc/skill path (`guidance`).
Distinct from the freeform note.

**Sweep**:
A periodic session that curates accumulated candidates — entries never
curated or aged since their last curation event.
