# Body-append is the comment primitive — no comments table

Status: accepted (2026-07-19)

Per-issue discussion (triage notes, briefs, resolutions) lives in the task
body as **appended H2 sections**. Icarium ships no comments mechanism, and
none is planned.

## Context

The Pocock skills assume per-issue comments: triage notes, agent briefs,
wontfix explanations, resolution threads. Icarium has only markdown bodies;
body-append was the undocumented de facto convention.

Adding a comments table would mean new schema, new CLI surface, and a second
place agents must read. A single body read is the more agent-friendly
contract, and icarium's users are agents.

The convention was already load-bearing: `Icarium.Dispatch.BodyDiff` diffs
bodies section-by-section on H2 headings as the reviewer's tamper signal —
workers may *append* `## Proof` or `## Notes`; any edit to an existing
section is flagged. Append-only H2 sections are therefore already the
mechanical append unit; this ADR names what exists.

## Decision

- A "comment" is a newly appended H2 section. Existing sections are
  immutable once written — enforced for workers by BodyDiff, followed by
  convention everywhere else.
- Repeat entries under one concern get dated bullets inside a single
  section (`## Triage notes`, `- 2026-07-19: …`), not a new H2 per note.
- Blessed section names, only those in active use: `## Question` /
  `## Answer` (wayfinder), `## Proof` / `## Notes` (workers, in code),
  `## Triage notes` (triage skill). No `## Brief` — a brief *is* the task
  body in icarium's model.
- No attribution syntax: dated bullets carry the when; the tracker stamps
  created/updated per node. (`.icarium/` is gitignored, so git history is
  *not* a fallback for bodies; richer attribution is moot while solo.)
- Bodies stay pure markdown — no XML section tags. XML delimiting has
  value only at dispatch **prompt assembly** (wrapping the agreement,
  future structured-output stanzas); that belongs to the prompt builder,
  not stored bodies. Structured data, if ever needed, is the
  frontmatter/sidecar ticket (`01KRMPRMK6`), not inline tags.

## Consequences

- `docs/agents/issue-tracker.md` documents the convention for skills that
  say "post a comment".
- The `icarium agents` quickstart gains a line stating the append-only
  convention (shipped-surface exit task).
- "Fetch body + comments" collapses to a single body read; degrades to
  nothing external reporters would miss (dogfood has none).
