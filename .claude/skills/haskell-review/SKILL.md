---
name: haskell-review
description: Senior Haskell reviewer pass — lint diagnosis as design feedback, module and interface boundaries, idiom drift, simplification opportunities. Read-only; produces a written review, does not edit code. Use when the user asks for a code review, a "second opinion," a pre-release sweep, or a check after significant changes.
---

# Haskell review

A review in the voice of a senior Haskell engineer: pattern recognition over rule-following, design feedback over mechanical fixes. **The output is opinion, not a diff. Do not edit files.**

## Start

Run `icarium know list` to load project-specific knowledge. The `refinement` discipline is specific to creating tasks with icarium, the others are what the agent has learned working on the codebase.

Skim `git log --oneline -20` for recent churn; that's where smells accumulate.

Scope-check with the user when it isn't obvious: whole library, a recent diff (`git diff main..HEAD`), one module, or a task body. Default to `src/` plus `app/`.

## The pass

Five axes, in order. Don't skip ahead — earlier signals often reframe later findings.

### 1. Automated checks: lint and format

Run `hlint` (or `make lint`). For **each** hit, diagnose the smell, not just the suggested rewrite:

- **Take the hint** when the hint is the cleanest form (`fromMaybe`, `isNothing`, `:` over `[x] ++ y`).
- **Refactor past the hint** when the hint exposes an awkward shape. `Move guards forward` on a list comprehension usually means use `mapMaybe`. `Use <$>` on `fmap (() <$) $ x` usually means use `void`. `Use newtype` on a single-field record is almost always right unless laziness genuinely matters.
- **Ignore in `.hlint.yaml`** only when the rule conflicts with a defensible house style. Site-specific exceptions belong in `{-# ANN #-}`, not the global file.

Strip the project's existing `ignore:` entries temporarily and re-run hlint to see which were preemptive (firing nowhere). Those can be deleted.

Run `make format` (or stylish-haskell / fourmolu) against a clean tree. Note any module that drifts on each run — that means a human is hand-formatting against the formatter and the policy hasn't been internalized.

### 2. Module inventory

`find src app -name '*.hs' | xargs wc -l`. For each module over ~300 lines, or whose name suggests multiple concerns, characterize what it actually does. Flag:

- **I/O mixed with pure logic**: a parser, renderer, or state machine living next to process spawning, DB writes, or git calls. Propose splitting the pure core into its own module — testability is the real prize.
- **Swiss-army modules**: over ~500 lines with three or more distinct concerns. Suggest splits along seam lines (data layer / transform layer / I/O layer).
- **Modules under ~50 lines that import broadly**: usually re-export shims that should be inlined.

When proposing a split, name the new module, list the symbols that move, and identify the cross-module edge that remains. If the split is non-trivial, mark the finding as "candidate for a separate task" — don't try to inline it into the review.

### 3. Public surface

For each module's export list:

- Anything exported only used by tests? Consider a `Module.Internal` split.
- Anything exported but only used inside the module? Drop it.
- Anything *not* exported that callers reach via transitive re-imports? Promote it.
- Wholesale constructor exports (`Type(..)`) where only the type and one smart constructor are used? Tighten.

A small, deliberate export list is a feature. Compare exports against actual import sites; large gaps are findings.

### 4. Idiom drift

Read with a Haskell idiom checklist running. The smells worth catching:

- Repeated `>>= \case Left → ... | Right → ...` — extract a helper, name the pattern.
- Boolean flags as function arguments where the call site reads `foo True` or `foo False` — usually wants a two-constructor ADT.
- `if cond then [x] else []` — list-comprehension guard.
- `(() <$)`, `fmap (const ())`, `>> return ()` — use `void` or `($>)`.
- `case foo of Just x -> ...; Nothing -> default` where default is a value — `fromMaybe`.
- Long `do` blocks with no internal structure — extract named steps.
- `where`-block depth ≥ 3 — usually wants a top-level helper.
- Missing strictness on accumulator-style record fields used as in-loop state.
- `data Foo = Foo { single :: T }` — newtype.
- Type ascriptions inside tuples (`(x :: T, ...)`) — usually means the type defaulting is unclear; consider a typed local binding.
- "Stringly-typed" patterns: passing `Text` between functions where a small ADT would catch errors at compile time.

### 5. Test posture

Match the codebase's testing philosophy (CLAUDE.md, otherwise infer). Look for:

- Pure logic with easily-enumerable inputs and no tests.
- Tests that mock DB/FS/network when a real temp resource would be simpler and more honest.
- Tests asserting on `Commands.*` (or equivalent thin-glue) internals — CLI integration tests should be doing that work.
- Per-instance round-trip tests where one parameterized test covers all cases.
- Test files over ~1000 lines — usually want splitting along subject lines.

## Output

A structured written review:

```
# Review: <scope>

## Headlines
2–4 bullets. The most consequential findings. If the codebase is in good shape, say so plainly.

## Findings

### [severity] short title — file:line
What's there. Why it's worth changing (or worth leaving alone). The proposed change in one or two sentences.
Mark "candidate for a separate task" when the work doesn't fit a small inline edit.

Severities:
- **must** — real defect (incorrect behavior, untested critical path, public-facing bug surface)
- **should** — clear improvement most senior reviewers would endorse
- **consider** — judgment call worth surfacing
- **leave** — something that looks weird but is right; mention so future readers don't churn
```

End with one line on what's working well: idioms used right, structure that paid off, decisions worth keeping. Future-you needs this to recalibrate.

## What not to recommend

- New abstractions for code used in one place. Concrete beats generic until duplicated.
- Lens, free monads, mtl-stacks, or new typeclass hierarchies. They almost never earn their weight in a CLI tool.
- Extensions the codebase doesn't already use (`ApplicativeDo`, `BlockArguments`, `OverloadedRecordDot`, etc.).
- Tests for properties the type system already proves.
- Refactoring for elegance alone. The bar is "future maintainer reads this faster," not "this is more clever."
- "I would have written this differently" framed as "this is wrong." Local context wins; if the code reads well in context, leave it.

## When delegating

For a whole-library pass, spawn a sub-agent (`Explore` or `general-purpose`) to gather inventory and grep results in parallel. Let it return facts; **you** form the opinions. Synthesis is not delegable.
