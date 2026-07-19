module Icarium.Commands.Agents (Command, parser, run) where

import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Options.Applicative

data Command = Agents

parser :: Parser Command
parser = pure Agents

run :: Command -> IO ()
run Agents = TIO.putStr guide

guide :: Text
guide =
    "icarium - agent quickstart\n\
    \\n\
    \Storage model\n\
    \  The DB tracks metadata. Bodies are plain markdown files on disk at\n\
    \  .icarium/bodies/{tasks,contexts}/<ulid>.md. Edit them with your normal\n\
    \  Read/Edit/Write tools.\n\
    \\n\
    \Create (one shot, fully linked)\n\
    \  icarium task add \"Title\" --domain <d> --discipline <d> --kind <k> \\\n\
    \    --references <ctx-id> --depends-on <task-id> --priority 7\n\
    \  # stdout: <new-id>\\n<body-path>\n\
    \\n\
    \  Two equivalent one-turn flows:\n\
    \    1. Run the command, then Write your markdown to <body-path>. The\n\
    \       file is not pre-created, so Write succeeds without a prior Read.\n\
    \    2. Pipe a heredoc: `icarium task add \"Title\" --body-stdin <<'EOF'`\n\
    \       ...markdown... `EOF`. No temp file, no extra Write.\n\
    \  Do NOT author a separate temp file just to pass it back in.\n\
    \  An empty --body/--body-stdin is an error; to defer the body, omit\n\
    \  the flag (flow 1).\n\
    \\n\
    \  Later edits (body already populated): Read $(icarium <kind> path <id>)\n\
    \  then Edit. Claude Code's Edit tool requires a prior Read of the path.\n\
    \\n\
    \  Same shape for `icarium ctx add` (no --depends-on; --derived-from\n\
    \  available for supersession chains).\n\
    \\n\
    \Edit a body (no retype, surgical)\n\
    \  Read $(icarium task path <id>)         # then Edit the same path\n\
    \  Read $(icarium ctx  path <id>)\n\
    \  FTS picks up edits on the next icarium command (mtime sweep) -\n\
    \  no manual `reindex` needed.\n\
    \\n\
    \Inspecting\n\
    \  icarium task show <id>                 # metadata only; body NOT printed.\n\
    \                                         # To see the body, Read the path.\n\
    \  icarium task cat <id>                  # print body to stdout (read-only)\n\
    \  icarium task next                      # print next ready task id (loop primitive)\n\
    \  icarium task claim                     # same pick, but atomically takes it:\n\
    \                                         # -> in-progress + owner stamp, prints id.\n\
    \                                         # Use this, not next + start, when other\n\
    \                                         # agents share the queue - two claims can\n\
    \                                         # never return the same task.\n\
    \  icarium task list --state ready        # actionable queue\n\
    \  icarium task exists <id>               # exit 0 found / 1 missing / 2 ambiguous\n\
    \  icarium search \"query\"                 # FTS5; \"phrase\", UPPERCASE OR,\n\
    \                                         # --kind task|ctx, --limit N\n\
    \  Same cat/exists on `icarium ctx`.\n\
    \  Add --json to task list/show, ctx list/show and search for stable\n\
    \  machine-readable output (arrays for lists; search returns\n\
    \  {total, hits} so --limit truncation is detectable; show carries\n\
    \  body_path, not body content).\n\
    \\n\
    \IDs\n\
    \  ULIDs. Any unique prefix works on the command line.\n\
    \\n\
    \Linking after the fact\n\
    \  icarium link add <task> depends-on <task>\n\
    \  icarium link add <task> references <ctx>\n\
    \\n\
    \Categories (three axes: domain, discipline, kind)\n\
    \  domain/discipline are retrieval axes: tasks and ctx both carry them,\n\
    \  and a dispatched task auto-pulls ctx matching on both. Fill them in.\n\
    \  kind is a workflow axis (bug, enhancement, chore, ...): it says what\n\
    \  shape the work is. Tasks only — ctx takes no --kind — and it never\n\
    \  affects which ctx gets pulled.\n\
    \  All values must already be registered:\n\
    \  icarium category list\n\
    \  icarium category add --axis domain <name>  # idempotent; updates\n\
    \                                             # icarium.toml + DB\n\
    \\n\
    \Task lifecycle\n\
    \  idea -> planned -> ready -> in-progress -> done\n\
    \                                         \\-> blocked  (requires --block-reason)\n\
    \  (abandoned is the dead-end state.) Dispatch picks up ready tasks whose\n\
    \  dependencies are done.\n\
    \\n\
    \  icarium task start <id>                # -> in-progress\n\
    \  icarium task done <id>                 # -> done\n\
    \  Other transitions: icarium task update <id> --state <state>\n\
    \\n\
    \Reviewers (if [review] is enabled in icarium.toml)\n\
    \  A reviewer agent grades your diff. If it returns `fail`, its findings are\n\
    \  appended to your next attempt under `## Reviewer findings from previous\n\
    \  attempt` - fix them. A `warn` merges but is saved as a `reviewer warn:`\n\
    \  context entry worth reading.\n\
    \\n\
    \More\n\
    \  icarium <cmd> --help                   # every subcommand has --help\n\
    \  icarium doctor                         # health check\n"
