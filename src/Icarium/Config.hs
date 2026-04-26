{-# LANGUAGE OverloadedStrings #-}
module Icarium.Config
    ( defaultConfigPath
    , defaultConfigText
      -- * Types
    , Config(..)
    , ProjectConfig(..)
    , CommandsConfig(..)
    , DispatchConfig(..)
    , CategoriesConfig(..)
      -- * IO
    , loadConfig
    ) where

import           Data.Text     (Text)
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import qualified Toml
import           Toml          (TomlCodec, (.=))

import           Icarium.Types (Effort, effortText, parseEffort)

-- =============================================================
-- Types
-- =============================================================

data Config = Config
    { cfgProject    :: ProjectConfig
    , cfgCommands   :: CommandsConfig
    , cfgDispatch   :: DispatchConfig
    , cfgCategories :: CategoriesConfig
    } deriving (Show)

data ProjectConfig = ProjectConfig
    { pcIntegrationBranch :: Text
    } deriving (Show)

data CommandsConfig = CommandsConfig
    { ccBuild :: Text
    , ccTest  :: Text
    } deriving (Show)

data DispatchConfig = DispatchConfig
    { dcModel                 :: Text
    , dcEffort                :: Effort
    , dcTools                 :: [Text]
    , dcAllowedTools          :: [Text]
    , dcScratchDir            :: Text
    , dcMaxMinutesPerDispatch :: Int
    , dcMaxDispatchesPerRun   :: Int
    , dcHeartbeatStaleSeconds :: Int
    , dcLogRetentionRuns      :: Int
    } deriving (Show)

data CategoriesConfig = CategoriesConfig
    { catDomains     :: [Text]
    , catDisciplines :: [Text]
    } deriving (Show)

-- =============================================================
-- TOML codecs
-- =============================================================

projectCodec :: TomlCodec ProjectConfig
projectCodec = ProjectConfig
    <$> Toml.text "integration_branch" .= pcIntegrationBranch

commandsCodec :: TomlCodec CommandsConfig
commandsCodec = CommandsConfig
    <$> Toml.text "build" .= ccBuild
    <*> Toml.text "test"  .= ccTest

effortField :: Toml.Key -> TomlCodec Effort
effortField = Toml.textBy effortText $ \t ->
    case parseEffort t of
        Just e  -> Right e
        Nothing -> Left $ "invalid effort: " <> t

dispatchCodec :: TomlCodec DispatchConfig
dispatchCodec = DispatchConfig
    <$> Toml.text                     "model"                    .= dcModel
    <*> effortField                   "effort"                   .= dcEffort
    <*> Toml.arrayOf Toml._Text       "tools"                    .= dcTools
    <*> Toml.arrayOf Toml._Text       "allowed_tools"            .= dcAllowedTools
    <*> Toml.text                     "scratch_dir"              .= dcScratchDir
    <*> Toml.int                      "max_minutes_per_dispatch" .= dcMaxMinutesPerDispatch
    <*> Toml.int                      "max_dispatches_per_run"   .= dcMaxDispatchesPerRun
    <*> Toml.int                      "heartbeat_stale_seconds"  .= dcHeartbeatStaleSeconds
    <*> Toml.int                      "log_retention_runs"       .= dcLogRetentionRuns

categoriesCodec :: TomlCodec CategoriesConfig
categoriesCodec = CategoriesConfig
    <$> Toml.arrayOf Toml._Text "domains"     .= catDomains
    <*> Toml.arrayOf Toml._Text "disciplines" .= catDisciplines

configCodec :: TomlCodec Config
configCodec = Config
    <$> Toml.table projectCodec    "project"    .= cfgProject
    <*> Toml.table commandsCodec   "commands"   .= cfgCommands
    <*> Toml.table dispatchCodec   "dispatch"   .= cfgDispatch
    <*> Toml.table categoriesCodec "categories" .= cfgCategories

-- =============================================================
-- IO
-- =============================================================

defaultConfigPath :: FilePath
defaultConfigPath = "icarium.toml"

loadConfig :: FilePath -> IO (Either String Config)
loadConfig fp = do
    src <- TIO.readFile fp
    pure $ case Toml.decode configCodec src of
        Right c   -> Right c
        Left errs -> Left (T.unpack (Toml.prettyTomlDecodeErrors errs))

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
    \effort = \"medium\"\n\
    \tools = [\"Read\", \"Edit\", \"Write\", \"Grep\", \"Glob\", \"Bash\"]\n\
    \allowed_tools = [\n\
    \  \"Read\", \"Edit\", \"Write\", \"Grep\", \"Glob\",\n\
    \  \"Bash(icarium:*)\", \"Bash(git:*)\", \"Bash(cabal:*)\",\n\
    \]\n\
    \scratch_dir = \".icarium/scratch\"\n\
    \max_minutes_per_dispatch = 30\n\
    \max_dispatches_per_run   = 20\n\
    \heartbeat_stale_seconds  = 300\n\
    \log_retention_runs       = 25\n\
    \\n\
    \[categories]\n\
    \domains     = []\n\
    \disciplines = []\n"
