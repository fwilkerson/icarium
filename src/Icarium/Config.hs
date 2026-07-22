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

    -- * Editing
    addCategoryToToml,
) where

import Data.List (dropWhileEnd, findIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Toml (TomlCodec, (.=))
import Toml qualified

import Icarium.Types (CategoryAxis (..), Effort, effortText, parseEffort)

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
    , dcMaxMinutesPerGate :: Int
    , dcHeartbeatStaleSeconds :: Int
    , dcLogRetentionRuns :: Int
    , dcRetryStormThreshold :: Int
    , dcWorktreeSetup :: Maybe Text
    , dcWorktreeTeardown :: Maybe Text
    , dcMcpConfig :: Maybe Text
    , dcAgreementPath :: Maybe Text
    }
    deriving (Show)

{- | The @[categories]@ table. @catKinds@ is the workflow axis and decodes
as optional, so configs written before the axis existed still load; empty
means the repo does not classify by kind.
-}
data CategoriesConfig = CategoriesConfig
    { catDomains :: [Text]
    , catDisciplines :: [Text]
    , catKinds :: [Text]
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
        <*> (fromMaybe 20 <$> Toml.dioptional (Toml.int "max_minutes_per_gate")) .= (Just . dcMaxMinutesPerGate)
        <*> Toml.int "heartbeat_stale_seconds" .= dcHeartbeatStaleSeconds
        <*> Toml.int "log_retention_runs" .= dcLogRetentionRuns
        <*> (fromMaybe 3 <$> Toml.dioptional (Toml.int "retry_storm_threshold")) .= (Just . dcRetryStormThreshold)
        <*> Toml.dioptional (Toml.text "worktree_setup") .= dcWorktreeSetup
        <*> Toml.dioptional (Toml.text "worktree_teardown") .= dcWorktreeTeardown
        <*> Toml.dioptional (Toml.text "mcp_config") .= dcMcpConfig
        <*> Toml.dioptional (Toml.text "agreement_path") .= dcAgreementPath

categoriesCodec :: TomlCodec CategoriesConfig
categoriesCodec =
    CategoriesConfig
        <$> Toml.arrayOf Toml._Text "domains" .= catDomains
        <*> Toml.arrayOf Toml._Text "disciplines" .= catDisciplines
        <*> (fromMaybe [] <$> Toml.dioptional (Toml.arrayOf Toml._Text "kinds")) .= (Just . catKinds)

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
    | dcMaxMinutesPerGate (cfgDispatch c) <= 0 =
        Left "dispatch.max_minutes_per_gate must be a positive integer"
    | otherwise = Right c

loadConfig :: FilePath -> IO (Either String Config)
loadConfig fp = do
    src <- TIO.readFile fp
    pure $ case Toml.decode configCodec src of
        Left errs -> Left (T.unpack (Toml.prettyTomlDecodeErrors errs))
        Right c -> validateConfig c

{- | Append a category name to the axis's array in icarium.toml source text,
preserving all other lines (comments included). When the axis has no array
line at all — @kinds@ in configs written before the axis existed — the array
is created at the end of the @[categories]@ table. Fails without modifying
anything when there is nowhere to put it or the result no longer decodes —
the caller falls back to telling the user to edit by hand.
-}
addCategoryToToml :: CategoryAxis -> Text -> Text -> Either String Text
addCategoryToToml axis name src = do
    let key = case axis of
            Domain -> "domains"
            Discipline -> "disciplines"
            Kind -> "kinds"
        ls = T.lines src
        manualHint =
            "add \""
                <> T.unpack name
                <> "\" to the "
                <> T.unpack key
                <> " array in icarium.toml manually and run `icarium category sync`"
    out <- case findIndex (isArrayLine key) ls of
        Just i -> do
            newLine <- insertIntoArray name (ls !! i)
            Right (T.unlines (take i ls <> [newLine] <> drop (i + 1) ls))
        Nothing -> case appendArrayLine key name ls of
            Just ls' -> Right (T.unlines ls')
            Nothing ->
                Left
                    ( "no single-line `"
                        <> T.unpack key
                        <> " = [...]` and no [categories] table in icarium.toml; "
                        <> manualHint
                    )
    case Toml.decode configCodec out of
        Left _ -> Left ("editing icarium.toml would corrupt it; " <> manualHint)
        Right c
            | name `elem` axisNames (cfgCategories c) -> Right out
            | otherwise -> Left ("edit did not take effect; " <> manualHint)
  where
    axisNames = case axis of
        Domain -> catDomains
        Discipline -> catDisciplines
        Kind -> catKinds

-- | Matches @key = [ ... ]@ on one line, ignoring surrounding whitespace.
isArrayLine :: Text -> Text -> Bool
isArrayLine key l =
    case T.stripPrefix key (T.stripStart l) of
        Nothing -> False
        Just rest -> case T.uncons (T.stripStart rest) of
            Just ('=', arr) ->
                let v = T.strip arr
                 in "[" `T.isPrefixOf` v && "]" `T.isSuffixOf` v
            _ -> False

{- | Create @key = ["name"]@ as the last entry of the @[categories]@ table,
after any trailing comments but before the blank run separating the next
table. @Nothing@ when there is no @[categories]@ table to extend.
-}
appendArrayLine :: Text -> Text -> [Text] -> Maybe [Text]
appendArrayLine key name ls = do
    hdr <- findIndex ((== "[categories]") . T.strip) ls
    let body = takeWhile (not . isTableHeader) (drop (hdr + 1) ls)
        at = hdr + 1 + length (dropWhileEnd (T.null . T.strip) body)
    pure (take at ls <> [key <> " = [\"" <> name <> "\"]"] <> drop at ls)

isTableHeader :: Text -> Bool
isTableHeader = T.isPrefixOf "[" . T.stripStart

-- | Insert @"name"@ before the closing bracket of a single-line array.
insertIntoArray :: Text -> Text -> Either String Text
insertIntoArray name l =
    let (beforeIncl, after) = T.breakOnEnd "]" l
        before = T.dropEnd 1 beforeIncl
        emptyArray = T.null (T.strip (T.drop 1 (snd (T.breakOn "[" before))))
        quoted = "\"" <> name <> "\""
        insertion = if emptyArray then quoted else ", " <> quoted
     in if T.null beforeIncl
            then Left ("no closing ] on line: " <> T.unpack l)
            else Right (before <> insertion <> "]" <> after)

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
    \model  = \"claude-opus-4-8\"\n\
    \effort = \"medium\"\n\
    \# The tools list is the gate: a tool absent here does not exist for the\n\
    \# worker. Adding \"Skill\" also enables slash commands and skills (no\n\
    \# allowed_tools entry needed -- skill invocation is permission-exempt).\n\
    \tools = [\"Read\", \"Edit\", \"Write\", \"Grep\", \"Glob\", \"Bash\"]\n\
    \allowed_tools = [\n\
    \  \"Read\", \"Edit\", \"Write\", \"Grep\", \"Glob\",\n\
    \  \"Bash(git:*)\", \"Bash(cabal:*)\",\n\
    \]\n\
    \scratch_dir = \".icarium/scratch\"\n\
    \# Wall-clock timeout per dispatch (minutes, must be a positive integer).\n\
    \max_minutes_per_dispatch = 30\n\
    \# Wall-clock timeout for each gate command (build, test) on its own. A\n\
    \# full build plus suite is not the same shape of work as one worker turn,\n\
    \# so it gets its own budget. Default 20.\n\
    \max_minutes_per_gate     = 20\n\
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
    \# Workers run with --strict-mcp-config, so no MCP servers load unless this\n\
    \# is set. Reviewers never get MCP servers regardless of this key.\n\
    \# mcp_config = \".mcp.json\"\n\
    \# Replace the built-in working agreement in dispatch prompts with this\n\
    \# file's content -- the whole of it; icarium appends nothing.\n\
    \# Unreadable file = fatal before the worker starts.\n\
    \# agreement_path = \".icarium/agreement.md\"\n\
    \\n\
    \[categories]\n\
    \# Source of truth for the category vocabulary. Register new names with\n\
    \# `icarium category add --axis domain <name>` (updates this file + DB),\n\
    \# or edit here and run `icarium category sync` (--prune deletes removed\n\
    \# names from the DB).\n\
    \#\n\
    \# domains/disciplines are retrieval axes: tasks and context both carry\n\
    \# them, and a task auto-pulls context matching on both. `kinds` is a\n\
    \# workflow axis — task-only, and deliberately ignored by auto-pull.\n\
    \domains     = [\"core\"]\n\
    \disciplines = [\"development\"]\n\
    \kinds       = [\"bug\", \"enhancement\", \"chore\"]\n\
    \\n\
    \[review]\n\
    \enabled      = true\n\
    \# A cross-model reviewer avoids the dispatch model grading itself.\n\
    \# Omit this key to inherit dispatch.model instead.\n\
    \model        = \"claude-sonnet-5\"\n\
    \max_attempts = 2\n\
    \# prompt_path  = \".icarium/reviewer.md\" # defaults to built-in prompt\n"
