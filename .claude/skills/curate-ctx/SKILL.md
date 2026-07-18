---
name: curate-ctx
description: Walk the ctx curation queue — vet each entry against the current code, decide a disposition, record via `icarium ctx curate`, and hand the human a reviewable ledger. Use when the user says "curate ctx", "run the ctx sweep", "sweep the context entries", or invokes /curate-ctx. Requires a working dir with .icarium/ or icarium.toml.
argument-hint: "[--older-than DAYS]"
allowed-tools: [Read, Edit, Write, Grep, Glob, Bash, AskUserQuestion]
---

# /curate-ctx — sweep accumulated context entries

You are running the curation sweep (ADR 0001, vocabulary in CONTEXT.md). Vet each entry against the code, decide, record; the human reviews the ledger at the end. A wrong call is cheap to reverse (a later `keep` revives), so a decided sweep beats a queue of questions.

## Dispositions

- **guidance** — content graduates into a doc or skill. Artifact: the doc/skill path.
- **rule** — claim becomes checkable: a lint rule, invariant, or test. Artifact: the rule/test name.
- **refactor** — the entry describes friction the code should absorb. Artifact: a task id.
- **keep** — still earns its place as retrieval context. No artifact.
- **stale** — no longer true or useful. Optional artifact: the superseding entry id.

guidance/rule/refactor/stale retire the entry (category injection stops; explicit references still deliver, except stale). keep leaves it current and resets its re-curation clock.

## Loop

1. `icarium ctx curate --json` — bare lists never-curated entries. Pass through `--older-than DAYS` if given; if the bare queue is empty, run `--older-than 90` before declaring the sweep done.
2. Per entry, oldest first:
   a. `Read $(icarium ctx path <id>)` and `icarium ctx show <id>`. Check who points at it (`linked_count`, `icarium ctx children <id>`) — a well-referenced entry leans keep.
   b. **Vet**: check every claim the code can settle — grep the named functions, flags, files, tests; run the commands the entry describes if cheap. Done when each claim is marked *holds*, *contradicted*, or *unsettleable*. A contradicted or code-absorbed claim is the strongest stale/graduated signal; `keep` is only available to an entry whose claims hold today.
   c. Decide one disposition. Signals: repeated in dispatch prompts but never actionable → guidance; states a "never/always" the code could enforce → rule; complains about code shape → refactor; contradicted or superseded → stale; still the best short answer to a live question → keep.
   d. Escalate to AskUserQuestion only when: vetting left a load-bearing claim unsettleable; the disposition would create or reshape a human-owned document (new ADR, CONTEXT.md); or the call hinges on the human's plans or priorities rather than code truth. Everything else: record without asking.
   e. Do the paperwork the disposition implies, then record:
      - guidance: move the content into the doc/skill (write the edit now), then
        `icarium ctx curate <id> guidance --artifact <path>`
      - rule: encode it if small (lint config, test, invariant); else park as refactor instead.
        `icarium ctx curate <id> rule --artifact <name>`
      - refactor: file the task first —
        `icarium task add "<title>" --domain <d> --discipline <d> --body-stdin` (link the entry: `icarium link add <task> references <ctx>`), then
        `icarium ctx curate <id> refactor --artifact <task-id>`
      - keep: `icarium ctx curate <id> keep`
      - stale: `icarium ctx curate <id> stale [--artifact <superseding-ctx-id>]`
      Always pass `--note`: what you vetted and why this disposition — the note is the human's audit trail.
3. After the queue, present the **ledger**: one line per entry — disposition, reason, what vetting showed — plus artifacts created. The human reverses any call by naming it; apply reversals with a fresh `ctx curate` event.

## Constraints

- Group trivially-similar entries when escalating; one question covers at most 3-4 entries.
- Don't delete entries or rewrite their bodies; retirement is the mechanism.
