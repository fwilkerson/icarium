---
name: refinement
description: Refine icarium tasks before headless dispatch — resolve open decisions and lock them into task bodies so a sonnet agent can execute without making product calls. Use when the working dir has .icarium/ or icarium.toml.
---

# Icarium refinement

Icarium dispatches headless sonnet agents on `ready` tasks. Agents execute well when decisions are already made; they pick poorly when bodies say "should we…" or list options. Refinement is where you and the user resolve those choices.

## Start

Ask the user what to focus on. They'll point at specific `idea` or `planned` tasks, or name an outcome and let you scope the relevant work.

The story is in `./bin/icarium task`, `task show <id>`, `dispatch list`, and `know list`. Pull what you need.

## For each task

1. Read the body. Identify unresolved decisions.
2. Research what you can yourself — read code, follow links, check git, spawn a sub-agent for breadth.
3. Batch open product questions to the user with AskUserQuestion. Concrete options, your recommendation first. Don't ask one at a time.
4. Lock the answers into the body with `task update ID --body-file …`. The body should read like a brief: spec, decisions, scope, "done when". No options, no open questions.
5. New task surfaced in conversation? Add it. Now-moot? Abandon with a one-line note.
6. Mark `--state ready` when the body would let an agent execute without making a product call.

## Quality bar

A ready body answers: *what changes, where, why this shape, how do we know it's done.* If a sonnet agent following it would have to make a judgment call about user-facing behavior, it's not ready.

## Close

Summarize in chat what's now ready and any non-obvious calls made.
