---
name: refinement
description: Refine icarium tasks before headless dispatch — resolve open decisions and lock them into task bodies so a sonnet agent can execute without making product calls. Use when the working dir has .icarium/ or icarium.toml.
---

# Icarium refinement

Icarium captures knowledge from finished tasks and builds prompts that future agents will run. **You are one of those agents.** The tasks you refine are tasks you (or a sibling) will be told to execute. The CLI you shape is the CLI you will use to find, read, and update tasks. Refinement is not paperwork — it is you deciding what the next agent's job actually is, as a user of this tool.

That perspective is load-bearing. An agent who treats refinement as "fill in the missing pieces of the task body" produces bloated speculative work the next agent has to push back on. Don't be that agent.

## Start

Ask the user what to focus on. They'll point at specific `idea` or `planned` tasks, or name an outcome and let you scope the relevant work.

The story is in `./bin/icarium` — bare `task`, `know`, `dispatch` list; `task show <id>` reads a record. Pull what you need.

## Foundational questions before product questions

Before resolving choices *within* a scope item, resolve choices *about* the scope itself. Usually one AskUserQuestion at the top of refinement:

- Who consumes this output / surface? (Humans, LLM agents, shell scripts, in-process callers?)
- What real incident or workflow drove this — or is it speculative polish?
- If we ship nothing for this item, what actually breaks?

The answers reshape every subsequent decision. Skipping this step is how you end up designing for consumers that don't exist.

## For each task

1. **Read the body skeptically.** Earlier refinement passes may have been sloppy. The body is one input, not the source of truth. Hunt for: contradictions, "should we…" phrasings, scope items justified by review-speak instead of real consumers, sections that conflate "more code" with "better."
2. **Read the code and current usage.** What is the surface today? Who calls it? Verify with `grep`/`Read`, not by trusting the body's claims.
3. **Justify each scope item against a real consumer.** "Does this earn its code?" is the bar. The honest answer is often *no* — delete the item. If three of five scope items get deleted in refinement, that is a successful refinement, not a failed one.
4. **Resolve the decisions that survived.** AskUserQuestion, concrete options, your recommendation first, batch them — don't ask one at a time.
5. **Update the body.** Reads like a brief: spec, decisions, scope, done-when. No options, no open questions, no "originally §X" archaeology — the next agent has no context for that.
6. **New task surfaced in conversation?** Add it. **Now-moot?** Abandon with a one-line note.
7. **Mark `--state ready`** when an agent following the body wouldn't need a product call.

## When the user's framing is also expansive

Push back. The user isn't always pulling toward smaller scope; sometimes their prompt was written quickly or borrows the framing of a prior bad session. If a scope item has no real consumer, say so and propose deleting it even if the user assumed it would stay. Agreeing with framing the next agent will have to undo wastes the next agent's session.

State your own conclusions, drawn from reading the code. Don't restate the spec or the user's prompt back as your verdict — that's how speculative scope survives refinement.

## Quality bar

A ready body answers: *what changes, where, why this shape, how do we know it's done.* If a sonnet agent following it would have to make a judgment call about user-facing behavior, it's not ready. If the body asserts a change is needed without naming a consumer who needs it, it's also not ready — go back to step 3.

## Close

Summarize what's now ready, what scope was deleted, and any non-obvious calls. If a task was abandoned or folded into another, say so and why.
