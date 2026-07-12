{-# LANGUAGE OverloadedStrings #-}

module Icarium.Config (
    defaultConfigPath,
    defaultConfigText,

    -- * Types
    Config (..),
    ProjectConfig (..),
    CommandsConfig (..),
    DispatchConfig (..),
    CategoriesConfig (..),
    ReviewConfig (..),

    -- * IO
    loadConfig,
    validateConfig,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Toml (TomlCodec, (.=))
import Toml qualified

import Icarium.Types (Effort, effortText, parseEffort)

-- =============================================================
-- Types
-- =============================================================

data Config = Config
    { cfgProject :: ProjectConfig
    , cfgCommands :: Maybe CommandsConfig
    , cfgDispatch :: DispatchConfig
    , cfgCategories :: CategoriesConfig
    , cfgReview :: Maybe ReviewConfig
    }
    deriving (Show)

newtype ProjectConfig = ProjectConfig
    { pcIntegrationBranch :: Text
    }
    deriving (Show)

data CommandsConfig = CommandsConfig
    { ccBuild :: Text
    , ccTest :: Text
    }
    deriving (Show)

data DispatchConfig = DispatchConfig
    { dcModel :: Text
    , dcEffort :: Effort
    , dcTools :: [Text]
    , dcAllowedTools :: [Text]
    , dcScratchDir :: Text
    , dcMaxMinutesPerDispatch :: Int
    , dcHeartbeatStaleSeconds :: Int
    , dcLogRetentionRuns :: Int
    , dcRetryStormThreshold :: Int
    , dcWorktreeSetup :: Maybe Text
    , dcWorktreeTeardown :: Maybe Text
    }
    deriving (Show)

data CategoriesConfig = CategoriesConfig
    { catDomains :: [Text]
    , catDisciplines :: [Text]
    }
    deriving (Show)

data ReviewConfig = ReviewConfig
    { rcEnabled :: Bool
    , rcModel :: Maybe Text
    , rcMaxAttempts :: Int
    , rcPromptPath :: Maybe Text
    }
    deriving (Show)

-- =============================================================
-- TOML codecs
-- =============================================================

projectCodec :: TomlCodec ProjectConfig
projectCodec =
    ProjectConfig
        <$> Toml.text "integration_branch" .= pcIntegrationBranch

commandsCodec :: TomlCodec CommandsConfig
commandsCodec =
    CommandsConfig
        <$> Toml.text "build" .= ccBuild
        <*> Toml.text "test" .= ccTest

effortField :: Toml.Key -> TomlCodec Effort
effortField = Toml.textBy effortText $ \t ->
    case parseEffort t of
        Just e -> Right e
        Nothing -> Left $ "invalid effort: " <> t

dispatchCodec :: TomlCodec DispatchConfig
dispatchCodec =
    DispatchConfig
        <$> Toml.text "model" .= dcModel
        <*> effortField "effort" .= dcEffort
        <*> Toml.arrayOf Toml._Text "tools" .= dcTools
        <*> Toml.arrayOf Toml._Text "allowed_tools" .= dcAllowedTools
        <*> Toml.text "scratch_dir" .= dcScratchDir
        <*> Toml.int "max_minutes_per_dispatch" .= dcMaxMinutesPerDispatch
        <*> Toml.int "heartbeat_stale_seconds" .= dcHeartbeatStaleSeconds
        <*> Toml.int "log_retention_runs" .= dcLogRetentionRuns
        <*> (fromMaybe 3 <$> Toml.dioptional (Toml.int "retry_storm_threshold")) .= (Just . dcRetryStormThreshold)
        <*> Toml.dioptional (Toml.text "worktree_setup") .= dcWorktreeSetup
        <*> Toml.dioptional (Toml.text "worktree_teardown") .= dcWorktreeTeardown

categoriesCodec :: TomlCodec CategoriesConfig
categoriesCodec =
    CategoriesConfig
        <$> Toml.arrayOf Toml._Text "domains" .= catDomains
        <*> Toml.arrayOf Toml._Text "disciplines" .= catDisciplines

reviewCodec :: TomlCodec ReviewConfig
reviewCodec =
    ReviewConfig
        <$> Toml.bool "enabled" .= rcEnabled
        <*> Toml.dioptional (Toml.text "model") .= rcModel
        <*> (fromMaybe 2 <$> Toml.dioptional (Toml.int "max_attempts")) .= (Just . rcMaxAttempts)
        <*> Toml.dioptional (Toml.text "prompt_path") .= rcPromptPath

configCodec :: TomlCodec Config
configCodec =
    Config
        <$> Toml.table projectCodec "project" .= cfgProject
        <*> Toml.dioptional (Toml.table commandsCodec "commands") .= cfgCommands
        <*> Toml.table dispatchCodec "dispatch" .= cfgDispatch
        <*> Toml.table categoriesCodec "categories" .= cfgCategories
        <*> Toml.dioptional (Toml.table reviewCodec "review") .= cfgReview

-- =============================================================
-- IO
-- =============================================================

defaultConfigPath :: FilePath
defaultConfigPath = "icarium.toml"

{- | Validate a decoded config. Returns Left with a human-readable error
when a field value is semantically invalid.
-}
validateConfig :: Config -> Either String Config
validateConfig c
    | dcMaxMinutesPerDispatch (cfgDispatch c) <= 0 =
        Left "dispatch.max_minutes_per_dispatch must be a positive integer"
    | otherwise = Right c

loadConfig :: FilePath -> IO (Either String Config)
loadConfig fp = do
    src <- TIO.readFile fp
    pure $ case Toml.decode configCodec src of
        Left errs -> Left (T.unpack (Toml.prettyTomlDecodeErrors errs))
        Right c -> validateConfig c

-- | Template written by @icarium init@. Values must match the codec above.
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
    \effort = \"high\"\n\
    \tools = [\"Read\", \"Edit\", \"Write\", \"Grep\", \"Glob\", \"Bash\"]\n\
    \allowed_tools = [\n\
    \  \"Read\", \"Edit\", \"Write\", \"Grep\", \"Glob\",\n\
    \  \"Bash(icarium:*)\", \"Bash(git:*)\", \"Bash(cabal:*)\",\n\
    \]\n\
    \scratch_dir = \".icarium/scratch\"\n\
    \# Wall-clock timeout per dispatch (minutes, must be a positive integer).\n\
    \max_minutes_per_dispatch = 30\n\
    \heartbeat_stale_seconds  = 300\n\
    \log_retention_runs       = 25\n\
    \# Retry-storm watchdog: kill dispatch after this many consecutive api_retry\n\
    \# events. Lower = faster kill on transient API spikes (risk: false kills);\n\
    \# higher = more tolerant but a stuck agent burns more wall-clock budget.\n\
    \retry_storm_threshold    = 3\n\
    \# Optional commands run inside each dispatch worktree, after creation and\n\
    \# before removal. Setup exit 75 means \"no capacity, try later\" (queue\n\
    \# drain stops cleanly); any other nonzero exit is an error.\n\
    \# worktree_setup    = \"scripts/worktree-init.sh\"\n\
    \# worktree_teardown = \"scripts/worktree-free.sh\"\n\
    \\n\
    \[categories]\n\
    \# Source of truth for the category vocabulary. Edit here, then run\n\
    \# `icarium category sync` to apply changes to the DB. Use --prune to\n\
    \# delete categories that have been removed from this list.\n\
    \domains     = [\"core\"]\n\
    \disciplines = [\"development\"]\n\
    \\n\
    \# [review]\n\
    \# enabled      = true\n\
    \# model        = \"claude-sonnet-4-6\"   # defaults to dispatch.model\n\
    \# max_attempts = 2\n\
    \# prompt_path  = \".icarium/reviewer.md\" # defaults to built-in prompt\n"
