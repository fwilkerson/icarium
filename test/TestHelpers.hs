module TestHelpers
    ( withTestDb
    , mkCat
    , mkKnowledge
    , attachKnowledgeCats
    , minTask
    ) where

import           Control.Exception      (bracket)
import           Control.Monad          (forM_)
import           Data.Text              (Text)
import           Database.SQLite.Simple (Connection, close, open)

import qualified Icarium.Repo.Category  as RC
import qualified Icarium.Repo.Knowledge as RK
import           Icarium.Schema         (applySchema)
import           Icarium.Types

withTestDb :: (Connection -> IO a) -> IO a
withTestDb act = bracket (open ":memory:") close $ \conn -> do
    applySchema conn
    act conn

mkCat :: Connection -> CategoryAxis -> Text -> IO Category
mkCat c axis name = do
    cid <- RC.insertCategory c axis name
    pure (Category cid axis name)

mkKnowledge :: Connection -> Text -> Text -> IO Text
mkKnowledge c title body =
    RK.insertKnowledge c RK.NewKnowledge { RK.nkTitle = title, RK.nkBody = body }

attachKnowledgeCats :: Connection -> Text -> [Category] -> IO ()
attachKnowledgeCats c kid cats =
    forM_ cats $ \cat -> RC.attachKnowledgeCategory c kid (categoryId cat)

minTask :: Task
minTask = Task
    { taskId          = "01TEST00000000000000000000"
    , taskTitle       = "Test task"
    , taskBody        = "Body text"
    , taskState       = Ready
    , taskPriority    = Nothing
    , taskBlockReason = Nothing
    , taskCreatedAt   = "2026-01-01T00:00:00Z"
    , taskUpdatedAt   = "2026-01-01T00:00:00Z"
    }
