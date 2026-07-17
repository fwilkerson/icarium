# Issue tracker: icarium (dogfood)

Issues for this repo are icarium tasks — tracked by the tool this repo builds.
DB at `.icarium/icarium.db`; bodies are markdown at `.icarium/bodies/tasks/<ulid>.md`.
Run `icarium agents` for the full quickstart; the `icarium-author` skill covers
authoring durable tasks and ctx entries.

## When a skill says "publish to the issue tracker"

    icarium task add "Title" --domain <d> --discipline <d> --body-stdin <<'EOF'
    ...markdown body...
    EOF

`--domain`/`--discipline` must be registered (`icarium category list`). New tasks
start in state `idea`. Specs/PRDs are ctx entries (`icarium ctx add`), linked via
`icarium link add <task> references <ctx>`.

## When a skill says "fetch the relevant ticket"

    icarium task show <id>             # metadata; add --json for machine output
    Read $(icarium task path <id>)     # body — Read before any Edit

Any unique ULID prefix works as an id. Find things with `icarium search "query"`.

## Wayfinding operations

Used by `/wayfinder`. Maps onto icarium primitives:

- **Map**: a ctx entry holding Notes / Decisions-so-far / Fog.
- **Child ticket**: one task per question, linked `references` the map ctx;
  a `Type:` line in the body records research/prototype/grilling/task.
- **Blocking**: `icarium link add <child> depends-on <blocker>`.
- **Frontier + claim**: `icarium task claim` — atomically takes the next ready,
  unblocked task (safe with concurrent agents).
- **Resolve**: append the answer under `## Answer` in the body,
  `icarium task done <id>`, then add the gist + task id to the map's
  Decisions-so-far.
