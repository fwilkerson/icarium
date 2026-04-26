# icarium

A task / knowledge / dispatch tool for headless-agent workflows. Haskell, sqlite-backed, CLI-first.

## Testing philosophy

Tests should cover the **interface and larger functional units**. Get nuanced only for code that is critical or genuinely tricky. The codebase is small; an over-tested codebase is a maintenance tax that slows real work.

**Three tiers, in priority order:**

1. **CLI integration tests** (highest value). Invoke `./bin/icarium` as a subprocess against a temp DB, assert on stdout/stderr/exit code. These are the contract we ship — they catch real regressions (broken flags, missing output, schema mismatches) and they're cheap to write.
2. **Repo tests.** Small, real-sqlite tests for each `Icarium.Repo.*` module. Cover the SQL surface where bugs actually hide.
3. **Pure-function tests.** For `parseEdgeKind`, `postClaudeGuard`, JSONL event parsing, etc. Tiny, fast, worth it because the inputs are easy to enumerate.

**Avoid:**
- Testing typeclass instance lookup. The type system already proves it.
- Per-enum round-trip tests. One parameterized test for "all enums round-trip" is enough.
- Mocking. sqlite-on-disk is fast enough; real DB tests are simpler and more honest.
- Byte-for-byte rendering tests beyond a single golden file per top-level renderer.
- Treating `Commands/*` as separate units. They're a thin gluing layer; CLI integration tests cover them.

If a function is hard to test, that's usually a design signal — extract the pure core, then the test is easy. Don't reach for mocks first.

## Lint and format

Lint hints are design feedback, not mechanical fixes. When `hlint` flags something, the first response is to consider whether the code shape itself is wrong — refactor to make the smell go away naturally. Mechanical auto-fix and adding `ignore:` entries both bypass that step. An ignore is correct only when the rule fundamentally conflicts with a deliberate house style.

`make format` and `make lint` are enforced by a pre-commit hook (run `make init` after fresh clone to install).
