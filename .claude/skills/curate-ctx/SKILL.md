---
name: curate-ctx
description: Walk the ctx curation queue entry by entry — propose a disposition with reasoning, confirm with the human, record via `icarium ctx curate`. Use when the user says "curate ctx", "run the ctx sweep", "sweep the context entries", or invokes /curate-ctx. Requires a working dir with .icarium/ or icarium.toml.
argument-hint: "[--older-than DAYS]"
allowed-tools: [Read, Edit, Write, Grep, Glob, Bash, AskUserQuestion]
---

# /curate-ctx — sweep accumulated context entries

You are running the curation sweep (ADR 0001, vocabulary in CONTEXT.md). The tool records outcomes; judgment stays with the human. You propose, they decide.

## Dispositions

- **guidance** — content graduates into a doc or skill. Artifact: the doc/skill path.
- **rule** — claim becomes checkable: a lint rule, invariant, or test. Artifact: the rule/test name.
- **refactor** — the entry describes friction the code should absorb. Artifact: a task id.
- **keep** — still earns its place as retrieval context. No artifact.
- **stale** — no longer true or useful. Optional artifact: the superseding entry id.

guidance/rule/refactor/stale retire the entry (category injection stops; explicit references still deliver, except stale). keep leaves it current and resets its re-curation clock.

## Loop

1. `icarium ctx curate --json` — bare lists never-curated entries. Pass through `--older-than DAYS` if given; if the bare queue is empty, suggest `--older-than 90` before declaring the sweep done.
2. Per entry, oldest first:
   a. `Read $(icarium ctx path <id>)` and `icarium ctx show <id>`. Check who points at it (`linked_count`, `icarium ctx children <id>`) — a well-referenced entry leans keep.
   b. Form one proposed disposition with a one-line reason. Signals: repeated in dispatch prompts but never actionable → guidance; states a "never/always" the code could enforce → rule; complains about code shape → refactor; superseded or contradicted by current code → stale; still the best short answer to a live question → keep.
   c. AskUserQuestion: proposed disposition first, labeled "(Recommended)", the plausible alternatives after; reason in the description. Offer "skip" via an option when genuinely unsure.
   d. Do the paperwork the disposition implies, then record:
      - guidance: move the content into the doc/skill (write the edit now), then
        `icarium ctx curate <id> guidance --artifact <path>`
      - rule: encode it if small (lint config, test, invariant); else park as refactor instead.
        `icarium ctx curate <id> rule --artifact <name>`
      - refactor: file the task first —
        `icarium task add "<title>" --domain <d> --discipline <d> --body-stdin` (link the entry: `icarium link add <task> references <ctx>`), then
        `icarium ctx curate <id> refactor --artifact <task-id>`
      - keep: `icarium ctx curate <id> keep`
      - stale: `icarium ctx curate <id> stale [--artifact <superseding-ctx-id>]`
      Add `--note` when the reason won't be obvious later.
3. After the queue: one-paragraph summary — counts per disposition, artifacts created, anything skipped and why.

## Constraints

- Never batch-record without per-entry confirmation; one AskUserQuestion per entry (group at most 3-4 trivially-similar entries into one question).
- Don't delete entries or rewrite their bodies; retirement is the mechanism.
- A wrong call is cheap to reverse (a later `keep` revives) — bias toward deciding over skipping.
