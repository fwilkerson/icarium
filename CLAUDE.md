## Testing philosophy

Tests should cover the **interface and larger functional units**. Get nuanced only for code that is critical or genuinely tricky. The codebase is small; an over-tested codebase is a maintenance tax that slows real work.

**Three tiers, in priority order:**

1. **CLI integration tests** (highest value). Invoke `icarium` as a subprocess against a temp DB, assert on stdout/stderr/exit code. These are the contract we ship — they catch real regressions (broken flags, missing output, schema mismatches) and they're cheap to write.
2. **Repo tests.** Small, real-sqlite tests for each `Icarium.Repo.*` module. Cover the SQL surface where bugs actually hide.
3. **Pure-function tests.** For `parseEdgeKind`, `postClaudeGuard`, JSONL event parsing, etc. Tiny, fast, worth it because the inputs are easy to enumerate.

**Avoid:**
- Testing typeclass instance lookup. The type system already proves it.
- Per-enum round-trip tests. One parameterized test for "all enums round-trip" is enough.
- Mocking. sqlite-on-disk is fast enough; real DB tests are simpler and more honest.
- Byte-for-byte rendering tests beyond a single golden file per top-level renderer.
- Treating `Commands/*` as separate units. They're a thin gluing layer; CLI integration tests cover them.

If a function is hard to test, that's usually a design signal — extract the pure core, then the test is easy. Don't reach for mocks first.

**Run `make install` before the CLI tests.** `test/CliSpec.hs` shells out to `bin/icarium`, which `cabal build` does not produce. Skipping it fails with `posix_spawnp: does not exist` — a stale-binary problem wearing a flake's clothes. Worse, after a source change `bin/icarium` is the *previous* build until you reinstall, so a green run can be testing code you already replaced.

## Code style

- Comments explain WHY (a hidden constraint, a workaround, a non-obvious invariant), never WHAT the code does.
- Pre-1.0: no backwards-compatibility shims for removed or changed surfaces. Design for correctness, not minimal change.

## Error handling

For multi-step `IO` where any step can fail: use `ExceptT Text IO` rather than nested `case`-of-`Either`. More than two levels of nesting is the signal. `throwE` to exit early; `runExceptT` at the boundary to handle the outcome once.

## Lint and format

Lint hints are design feedback, not mechanical fixes. When `hlint` flags something, the first response is to consider whether the code shape itself is wrong — refactor to make the smell go away naturally. Mechanical auto-fix and adding `ignore:` entries both bypass that step. An ignore is correct only when the rule fundamentally conflicts with a deliberate house style.

## Dispatch agreement mirror

`.icarium/agreement.md` = the `builtInAgreement` body (`src/Icarium/Dispatch/Agreement.hs`) + dogfood additions. When editing either, sync the other. The file cannot carry comments — its content lands verbatim in dispatch prompts.

## Fresh clone setup

Install [direnv](https://direnv.net/) and run `direnv allow` at the repo root. The committed `.envrc` adds `bin/` to `PATH`, so bare `icarium` resolves to the local build artifact after `make install`.

## Agent skills

### Issue tracker

Issues are icarium tasks in this repo's own `.icarium/` DB (dogfooding). See `docs/agents/issue-tracker.md`.

### Triage labels

Default five-role vocabulary, realized as icarium task states. See `docs/agents/triage-labels.md`.

### Domain docs

Single-context: `CONTEXT.md` + `docs/adr/` at the repo root. See `docs/agents/domain.md`.
