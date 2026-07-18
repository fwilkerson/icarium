# Bodies as files; the DB keeps the relational layer

Status: accepted (2026-07-18, recording decisions from 2026-05)

Task and context bodies live as markdown files under
`.icarium/bodies/{tasks,contexts}/<ulid>.md`; files are the source of truth
for editing, and agents Read/Edit the path directly (`icarium task path` /
`ctx path`). The sqlite DB keeps everything *around* the body content.

## Why files for bodies

1. **Agent ergonomics.** A path goes straight into Read/Edit tools — no
   round-trip through `show` stdout, temp files, and `--body-stdin`.
2. **Token efficiency.** `show` stays compact; agents read the body only
   when needed, so large bodies stop eating context budget repeatedly.
3. **Native diffability.** Real files get diffs, blame, and editor support
   for free.

## Why the DB stays load-bearing

- **Relational edges** — `depends-on`, `references`, `derived-from`,
  `supersedes` as joins, not file conventions.
- **Categories** — domain × discipline tagging is the filter axis dispatch
  uses to pull relevant context into a headless prompt in one query.
- **FTS5** over body + title — content match combined with metadata filter
  in a single query; agents discover prior context via `icarium search`, not
  by listing files. Non-negotiable: search must keep working over file-backed
  bodies.
- **Dispatch state** — heartbeats, pids, tokens, lifecycle.
- **Atomicity** — task + categories + edges in one transaction.
- **Migrations** — adding a column is a migration, not a sed sweep.

Take either side away and the dispatch loop degrades: files alone lose the
relevant-context query; DB alone loses the agent's native editing ergonomics.

## Consequences

- `updated_at` must stay honest under out-of-band file edits; an mtime sweep
  reconciles it so freshness and search ranking stay meaningful. The sweep
  currently runs synchronously in `withDb`; only *eventual* reconciliation is
  required — relaxing this is tracked as a separate task.
