module Icarium.Repo.Edge
    ( insertEdge
    , listEdges
    , deleteEdge
    , referencedKnowledge
    , dependencyTasks
    , knowledgeDerivedFromTask
    ) where

import           Data.Text              (Text)
import           Database.SQLite.Simple (Connection, Only (..), Query (..), execute, query, query_)

import           Icarium.Id             (newId)
import           Icarium.Types          (Edge (..), EdgeKind (..), Knowledge, NodeKind (..), Task)

-- | Insert an edge. The DB enforces kind/endpoint typing and node
-- existence via CHECK constraints and triggers (see schema.sql).
insertEdge
    :: Connection
    -> EdgeKind
    -> NodeKind -> Text   -- ^ source
    -> NodeKind -> Text   -- ^ destination
    -> IO Text
insertEdge conn kind srcKind srcId dstKind dstId = do
    eid <- newId
    execute conn
        (Query "INSERT INTO edges (id, kind, src_kind, src_id, \
               \dst_kind, dst_id) VALUES (?, ?, ?, ?, ?, ?)")
        (eid, kind, srcKind, srcId, dstKind, dstId)
    pure eid

edgeCols :: Text
edgeCols = "id, kind, src_kind, src_id, dst_kind, dst_id, created_at"

listEdges
    :: Connection
    -> Maybe Text       -- ^ filter by src id
    -> Maybe Text       -- ^ filter by dst id
    -> Maybe EdgeKind   -- ^ filter by kind
    -> IO [Edge]
listEdges conn mSrc mDst mKind = do
    rows <- query_ conn
        (Query $ "SELECT " <> edgeCols <> " FROM edges ORDER BY created_at ASC")
    pure
        $ filterBy mKind (\k -> (== k) . edgeKind)
        . filterBy mDst  (\d -> (== d) . edgeDstId)
        . filterBy mSrc  (\s -> (== s) . edgeSrcId)
        $ rows
  where
    filterBy Nothing  _ xs = xs
    filterBy (Just v) p xs = filter (p v) xs

deleteEdge :: Connection -> Text -> IO Bool
deleteEdge conn eid = do
    rows <- query conn
        (Query "SELECT id FROM edges WHERE id = ?") (Only eid) :: IO [Only Text]
    case rows of
        [] -> pure False
        _  -> do
            execute conn (Query "DELETE FROM edges WHERE id = ?") (Only eid)
            pure True

-- | Knowledge linked by @references@ edges from the given task.
referencedKnowledge :: Connection -> Text -> IO [Knowledge]
referencedKnowledge conn tid = query conn
    (Query "SELECT k.id, k.title, k.body, k.stale, k.created_at, k.updated_at \
           \FROM edges e \
           \JOIN knowledge k ON k.id = e.dst_id \
           \WHERE e.kind = 'references' \
           \  AND e.src_kind = 'task' AND e.src_id = ? \
           \  AND e.dst_kind = 'knowledge' \
           \ORDER BY e.created_at ASC")
    (Only tid)

-- | Tasks that the given task depends on (depends_on edges, dst side).
dependencyTasks :: Connection -> Text -> IO [Task]
dependencyTasks conn tid = query conn
    (Query "SELECT t.id, t.title, t.body, t.state, t.priority, \
           \       t.block_reason, t.created_at, t.updated_at \
           \FROM edges e \
           \JOIN tasks t ON t.id = e.dst_id \
           \WHERE e.kind = 'depends_on' \
           \  AND e.src_kind = 'task' AND e.src_id = ? \
           \  AND e.dst_kind = 'task' \
           \ORDER BY e.created_at ASC")
    (Only tid)

-- | Knowledge entries that have a derived_from edge pointing at the given task.
knowledgeDerivedFromTask :: Connection -> Text -> IO [Knowledge]
knowledgeDerivedFromTask conn tid = query conn
    (Query "SELECT k.id, k.title, k.body, k.stale, k.created_at, k.updated_at \
           \FROM edges e \
           \JOIN knowledge k ON k.id = e.src_id \
           \WHERE e.kind = 'derived_from' \
           \  AND e.src_kind = 'knowledge' \
           \  AND e.dst_kind = 'task' AND e.dst_id = ? \
           \ORDER BY e.created_at ASC")
    (Only tid)
