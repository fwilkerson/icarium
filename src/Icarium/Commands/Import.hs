module Icarium.Commands.Import (Options, parser, run) where

import           Control.Monad                  (forM_, unless, when)
import           Data.Aeson                     (FromJSON (..), eitherDecode, withObject, (.:))
import qualified Data.ByteString.Lazy           as BL
import           Data.Text                      (Text)
import           Database.SQLite.Simple         (Connection, Only (..), Query (..), SQLData,
                                                 execute, query_, withTransaction)
import           Database.SQLite.Simple.ToField (toField)
import           Options.Applicative
import           System.IO                      (stdin)

import           Icarium.Db                     (defaultDbPath, withDb)
import           Icarium.Schema                 (schemaVersion)
import           Icarium.Types

data Options = Options
    { optMerge :: Bool
    , optIn    :: Maybe FilePath
    }

data ImportPayload = ImportPayload
    { ipSchemaVersion :: Int
    , ipTasks         :: [Task]
    , ipKnowledge     :: [Knowledge]
    , ipEdges         :: [Edge]
    , ipCategories    :: [Category]
    , ipDispatches    :: [Dispatch]
    }

instance FromJSON ImportPayload where
    parseJSON = withObject "ImportPayload" $ \o -> ImportPayload
        <$> o .:  "schema_version"
        <*> o .:  "tasks"
        <*> o .:  "knowledge"
        <*> o .:  "edges"
        <*> o .:  "categories"
        <*> o .:  "dispatches"

parser :: Parser Options
parser = Options
    <$> switch
          ( long "merge"
         <> help "Allow import into a non-empty DB; existing records are skipped" )
    <*> optional (strArgument (metavar "FILE"))

run :: Options -> IO ()
run Options{..} = do
    bytes   <- case optIn of
        Nothing  -> BL.hGetContents stdin
        Just "-" -> BL.hGetContents stdin
        Just path -> BL.readFile path
    payload <- either (ioError . userError . ("JSON parse error: " <>)) pure
                   (eitherDecode bytes)
    when (ipSchemaVersion payload /= schemaVersion) $
        ioError $ userError $
            "unsupported schema_version " <> show (ipSchemaVersion payload)
         <> " (this build expects " <> show schemaVersion <> ")"
    withDb defaultDbPath $ \conn -> do
        unless optMerge $ do
            [Only tc] <- query_ conn "SELECT COUNT(*) FROM tasks"     :: IO [Only Int]
            [Only kc] <- query_ conn "SELECT COUNT(*) FROM knowledge" :: IO [Only Int]
            when (tc + kc > 0) $
                ioError $ userError
                    "database is not empty; re-run with --merge to import anyway"
        withTransaction conn $ importAll conn optMerge payload

importAll :: Connection -> Bool -> ImportPayload -> IO ()
importAll conn merge ImportPayload{..} = do
    forM_ ipCategories $ \Category{..} ->
        execute conn
            (ins "categories" "id, axis, name" "?, ?, ?")
            (categoryId, categoryAxis, categoryName)
    forM_ ipTasks $ \Task{..} ->
        execute conn
            (ins "tasks"
                 "id, title, body, state, priority, block_reason, created_at, updated_at"
                 "?, ?, ?, ?, ?, ?, ?, ?")
            (taskId, taskTitle, taskBody, taskState, taskPriority,
             taskBlockReason, taskCreatedAt, taskUpdatedAt)
    forM_ ipKnowledge $ \Knowledge{..} ->
        execute conn
            (ins "knowledge"
                 "id, title, body, stale, created_at, updated_at"
                 "?, ?, ?, ?, ?, ?")
            (knowledgeId, knowledgeTitle, knowledgeBody,
             boolToInt knowledgeStale, knowledgeCreatedAt, knowledgeUpdatedAt)
    forM_ ipEdges $ \Edge{..} ->
        execute conn
            (ins "edges"
                 "id, kind, src_kind, src_id, dst_kind, dst_id, created_at"
                 "?, ?, ?, ?, ?, ?, ?")
            (edgeId, edgeKind, edgeSrcKind, edgeSrcId,
             edgeDstKind, edgeDstId, edgeCreatedAt)
    forM_ ipDispatches $ \Dispatch{..} ->
        execute conn
            (ins "dispatches"
                 "id, task_id, branch, base_branch, base_sha, pid, model, effort, \
                 \started_at, heartbeat_at, ended_at, outcome, merge_sha, \
                 \last_commit, notes, log_path"
                 "?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?")
            ([ toField dispatchId,         toField dispatchTaskId
             , toField dispatchBranch,     toField dispatchBaseBranch
             , toField dispatchBaseSha,    toField dispatchPid
             , toField dispatchModel,      toField dispatchEffort
             , toField dispatchStartedAt,  toField dispatchHeartbeat
             , toField dispatchEndedAt,    toField dispatchOutcome
             , toField dispatchMergeSha,   toField dispatchLastCommit
             , toField dispatchNotes,      toField dispatchLogPath
             ] :: [SQLData])
  where
    orIgnore :: Text
    orIgnore = if merge then " OR IGNORE" else ""

    ins :: Text -> Text -> Text -> Query
    ins tbl cols qs =
        Query $ "INSERT" <> orIgnore <> " INTO " <> tbl
             <> " (" <> cols <> ") VALUES (" <> qs <> ")"

    boolToInt :: Bool -> Int
    boolToInt True  = 1
    boolToInt False = 0
