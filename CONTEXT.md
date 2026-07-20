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

Categories split by job. `domain`/`discipline` are *retrieval* axes: carried
by tasks and ctx alike, and every one of them narrows the auto-pull above.
`kind` is a *workflow* axis — task-only, describing the shape of the work
(bug wants a repro, enhancement wants acceptance criteria) — and is excluded
from auto-pull, since the kind of work that produced a learning does not
predict when that learning is relevant. `Types.retrievalAxes` is where that
split is decided; adding an axis means choosing a side there.

## Language

### Task lifecycle

**State**:
A task's position in its lifecycle, stored on the task. One axis, one value
at a time. A state says what a task *is*, never who may select it.

**Queue**:
An ordered selection of actionable work: tasks in a ready state whose
dependencies are all satisfied, ordered by priority then age. Derived, never
stored — a queue is a view over states, so the two are not interchangeable.
_Avoid_: backlog (unordered, and not gated on dependencies)

**Headless queue / Interactive queue**:
The two queues, distinguished only by who does the work. Headless work a
dispatched agent may take unattended; interactive work needs a human at a
keyboard. They share one specification bar — both are fully specified, and
neither is the default reading of "ready".
_Avoid_: ready (unqualified — it named the headless queue on the state axis
and the interactive queue on the selector axis; ADR 0007)

**Actionable**:
A ready-state task whose dependencies are all satisfied. The dependency gate
is what separates a queue from a plain state filter.

**Claim**:
Atomically taking a task: marks it in-progress and stamps an owner, so
concurrent agents cannot take the same work. Claiming from a queue takes the
head; claiming a *named* task ignores the dependency gate, because naming it
is the selection.

### Dispatch lifecycle

**Landed / Merged**:
A dispatch branch that is in the integration branch. Distinct from task
state: `done` means the work was accepted (gates and reviewer passed);
only *merged* satisfies dependents.

**Parked**:
A successful dispatch whose branch could not land automatically — the
merge was blocked (conflict, dirty checkout, gate failure after rebase).
An exception state needing a human, not a resting state: every success
attempts to land immediately.
_Avoid_: park-by-default (the pre-auto-merge model, where parked was the
normal post-success state)

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
