# Issue tracker: icarium (dogfood)

Issues for this repo are icarium tasks — tracked by the tool this repo builds.
DB at `.icarium/icarium.db`; bodies are markdown at `.icarium/bodies/tasks/<ulid>.md`.
Run `icarium agents` for the full quickstart; the `icarium-author` skill covers
authoring durable tasks and ctx entries.

## When a skill says "publish to the issue tracker"

    icarium task add "Title" --domain <d> --discipline <d> --kind <k> --body-stdin <<'EOF'
    ...markdown body...
    EOF

All three must be registered (`icarium category list`; register with
`icarium category add --axis <axis> <name>`). `--domain`/`--discipline` are the
retrieval axes — they decide which ctx entries a dispatch prompt pulls in, so
fill them. `--kind` (`bug`, `enhancement`, `chore`, …) records the shape of the
work; it is task-only and never affects the pull. New tasks start in state
`planned`; pass `--state` to file one straight into triage (`idea`) or onto the
dispatch queue (`ready-headless`) — see [triage labels](triage-labels.md).
Specs/PRDs are ctx entries (`icarium ctx add`), linked via
`icarium link add <task> references <ctx>`.

## When a skill says "fetch the relevant ticket"

    icarium task show <id>             # metadata; add --json for machine output
    Read $(icarium task path <id>)     # body — Read before any Edit

Any unique ULID prefix works as an id. Find things with `icarium search "query"`.

## When a skill says "post a comment"

There are no comments — append an H2 section to the body instead
(ADR 0006). A "comment" is a *newly appended* section; never edit prior
sections (dispatch diffs on this — `Icarium.Dispatch.BodyDiff`). Repeat
entries share one section with dated bullets:

    ## Triage notes
    - 2026-07-19: needs repro on macOS; asked reporter.

Blessed names: `## Question`/`## Answer` (wayfinder), `## Proof`/`## Notes`
(dispatch workers), `## Triage notes` (triage). Timestamps come from dated
bullets and the tracker's stamps — `.icarium/` is gitignored, so bodies
have no git history.

## Wayfinding operations

Used by `/wayfinder`. Maps onto icarium primitives:

- **Map**: a ctx entry holding Notes / Decisions-so-far / Fog.
- **Child ticket**: one task per question, linked `references` the map ctx;
  a `Type:` line in the body records research/prototype/grilling/task.
- **Blocking**: `icarium link add <child> depends-on <blocker>`.
- **Frontier + claim**: `icarium task claim` — atomically takes the next
  unblocked `ready_interactive` task (safe with concurrent agents).
  `icarium task claim <id>` takes a named task in either ready state.
  Exit 1 means the queue is empty; exit 3 means the write lock stayed busy
  and the queue is still unknown — retry rather than concluding "no work".
- **Resolve**: append the answer under `## Answer` in the body,
  `icarium task done <id>`, then add the gist + task id to the map's
  Decisions-so-far.
