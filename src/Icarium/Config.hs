module Icarium.Config
    ( defaultConfigPath
    , defaultConfigText
    ) where

import Data.Text (Text)

defaultConfigPath :: FilePath
defaultConfigPath = "icarium.toml"

-- | Template written by @icarium init@. Full parser lands in the next
-- commit; for now the file exists for humans and as a doctor-check.
defaultConfigText :: Text
defaultConfigText =
    "[project]\n\
    \integration_branch = \"main\"\n\
    \\n\
    \[commands]\n\
    \build = \"cabal build all\"\n\
    \test  = \"cabal test all\"\n\
    \\n\
    \[dispatch]\n\
    \model  = \"claude-sonnet-4-6\"\n\
    \effort = \"medium\"\n\
    \allowed_tools = [\n\
    \  \"Read\", \"Edit\", \"Write\", \"Grep\", \"Glob\",\n\
    \  \"Bash(icarium:*)\", \"Bash(git:*)\", \"Bash(cabal:*)\",\n\
    \]\n\
    \max_minutes_per_dispatch = 30\n\
    \max_dispatches_per_run   = 20\n\
    \heartbeat_stale_seconds  = 300\n\
    \log_retention_runs       = 25\n\
    \\n\
    \[categories.domains]\n\
    \values = []\n\
    \\n\
    \[categories.disciplines]\n\
    \values = []\n"
