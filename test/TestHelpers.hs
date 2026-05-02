module TestHelpers (
    withTestDb,
    withBaseTestDb,
    mkCat,
    mkKnowledge,
    attachKnowledgeCats,
    minTask,
) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, close, open)

import Icarium.Db (migrateDb)
import Icarium.Repo.Category qualified as RC
import Icarium.Repo.Knowledge qualified as RK
import Icarium.Schema (applySchema)
import Icarium.Types

-- | In-memory DB at the full current schema (base + all migrations).
withTestDb :: (Connection -> IO a) -> IO a
withTestDb act = bracket (open ":memory:") close $ \conn -> do
    applySchema conn
    migrateDb conn
    act conn

-- | In-memory DB at the base schema only (user_version = 1, no migrations).
withBaseTestDb :: (Connection -> IO a) -> IO a
withBaseTestDb act = bracket (open ":memory:") close $ \conn -> do
    applySchema conn
    act conn

mkCat :: Connection -> CategoryAxis -> Text -> IO Category
mkCat c axis name = do
    cid <- RC.insertCategory c axis name
    pure (Category cid axis name)

mkKnowledge :: Connection -> Text -> Text -> IO Text
mkKnowledge c title body =
    RK.insertKnowledge c RK.NewKnowledge{RK.nkTitle = title, RK.nkBody = body}

attachKnowledgeCats :: Connection -> Text -> [Category] -> IO ()
attachKnowledgeCats c kid cats =
    forM_ cats $ \cat -> RC.attachKnowledgeCategory c kid (categoryId cat)

minTask :: Task
minTask =
    Task
        { taskId = "01TEST00000000000000000000"
        , taskTitle = "Test task"
        , taskBody = "Body text"
        , taskState = Ready
        , taskPriority = Nothing
        , taskBlockReason = Nothing
        , taskCreatedAt = "2026-01-01T00:00:00Z"
        , taskUpdatedAt = "2026-01-01T00:00:00Z"
        }
