# icarium derives the review verdict from finding severities

Status: accepted (2026-07-20)

The reviewer's structured return carries `findings[]` and nothing else. It does
not report a verdict; `icarium` computes `ReviewVerdict` as the worst severity
across the findings (empty ⇒ pass). Workers likewise report a *submission*, not
a success — the verdict over their diff is never theirs to give.

## Context

The reviewer prompt already defined its verdict as a function of its findings —
"status is the verdict over BOTH axes, the worst outcome on either wins" — while
also asking the model to emit that verdict as a separate `status:` field. Two
representations of one value, and `--json-schema` constrained decoding
guarantees *shape*, not *consistency*: a `fail` finding reported under a `pass`
verdict is a well-formed payload the schema cannot reject.

This surfaced during the return-payload design, alongside a naming collision
that turned out to be the same problem. `DispatchOutcome` (`success | failure |
interrupted`) is icarium's conclusion about a dispatch. The proposed worker
field was also called `outcome`, also with a `success` value — but a worker's
`success` is contested: gates, the dirty-tree guard, and the reviewer can all
turn it into `OFailure`.

## Decision

**A dispatch participant reports what it observed; icarium decides what it
means.** Concretely:

- The reviewer schema has no `verdict` property. `findings[]` is required, with
  `[]` as the pass case.
- Severity is per-finding (`warn | fail`), which is the reviewer's judgement and
  properly its own. Whether a severity lands the branch is policy, and policy is
  icarium's.
- The worker reports `status: submitted | blocked`. `submitted` names the act of
  handing over a diff, claiming nothing about its acceptance. `blocked` is kept
  as-is because it is *unilateral* — nothing downstream overrules a worker that
  says it is stuck — and it maps 1:1 onto `TaskState.Blocked` plus
  `task.block_reason`.

`ReviewVerdict`, `setReviewInfo`, and the `PCRetry` branch are unchanged. Only
the value's provenance moves, from asserted to derived.

## Considered options

- **Keep the model's verdict.** An explicit verdict field asks for a *gestalt* —
  "twenty small things add up to something disqualifying" — that `max(severity)`
  cannot express. Rejected: the reviewer retains that lever by marking one
  finding `fail` and saying why, which is more auditable than an unexplained
  override, and it costs the contradiction risk above.
- **A `fail_on_warn` policy knob.** Rejected on its merits, and made unnecessary
  by this decision. The reviewer prompt deliberately manufactures low-confidence
  findings ("report every issue… including ones you are uncertain about or
  consider minor — attach a severity rather than withholding"), so promoting
  `warn` to a merge blocker aims the retry loop at judgement calls. Worse, since
  users may override the reviewer prompt but not the schema, the cheapest escape
  from a blocking warn is to weaken the reviewer until it stops reporting —
  degrading the observation to escape the policy. Warn already has a
  non-blocking consequence: it mints a ctx entry that `/curate-ctx` can promote
  with `ctx curate <id> refactor --artifact <task id>`. With the verdict derived,
  such a threshold becomes config that the reviewer never sees, so the option
  stays open without shaping the contract.

## Consequences

- `parseReviewVerdictFromText` and `lastYamlBlock` are deletable; the schema
  replaces the fenced-YAML scan.
- Fail-closed posture on the *process* is retained separately: a reviewer that
  times out or exits non-zero is still `RVFail`, independent of findings.
- Verdict derivation lives in one place and is unit-testable without invoking a
  model.
