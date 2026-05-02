module Icarium.Repo.Edge (
    insertEdge,
    listEdges,
    deleteEdge,
    getEdgesByPrefix,
    resolveEdgeId,
    referencedKnowledge,
    dependencyTasks,
    taskEdgeCounts,
    knowledgeInboundCounts,
    knowledgeDerivedFromDispatch,
) where

import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (
    Connection,
    Only (..),
    Query (..),
    SQLData (..),
    execute,
    query,
 )

import Icarium.Id (newId)
import Icarium.Repo.Internal (prefixLookup, resolveByPrefix)
import Icarium.Repo.Knowledge (knowColsQualified)
import Icarium.Repo.Task (taskColsQualified)
import Icarium.Types (
    Edge (..),
    EdgeKind (..),
    Knowledge,
    NodeKind (..),
    Task,
    edgeKindDbText,
 )

{- | Insert an edge. The DB enforces kind/endpoint typing and node
existence via CHECK constraints and triggers (see schema.sql).
-}
insertEdge ::
    Connection ->
    EdgeKind ->
    NodeKind ->
    -- | source
    Text ->
    NodeKind ->
    -- | destination
    Text ->
    IO Text
insertEdge conn kind srcKind srcId dstKind dstId = do
    eid <- newId
    execute
        conn
        ( Query
            "INSERT INTO edges (id, kind, src_kind, src_id, \
            \dst_kind, dst_id) VALUES (?, ?, ?, ?, ?, ?)"
        )
        (eid, kind, srcKind, srcId, dstKind, dstId)
    pure eid

edgeCols :: Text
edgeCols = "id, kind, src_kind, src_id, dst_kind, dst_id, created_at"

listEdges ::
    Connection ->
    -- | filter by src id
    Maybe Text ->
    -- | filter by dst id
    Maybe Text ->
    -- | filter by kind
    Maybe EdgeKind ->
    IO [Edge]
listEdges conn mSrc mDst mKind =
    query
        conn
        (Query $ "SELECT " <> edgeCols <> " FROM edges" <> whereClause <> " ORDER BY created_at ASC")
        params
  where
    filters =
        catMaybes
            [ fmap (\s -> ("src_id = ?", SQLText s)) mSrc
            , fmap (\d -> ("dst_id = ?", SQLText d)) mDst
            , fmap (\k -> ("kind = ?", SQLText (edgeKindDbText k))) mKind
            ]
    (whereClause, params) = case filters of
        [] -> ("", [])
        fs -> (" WHERE " <> T.intercalate " AND " (map fst fs), map snd fs)

deleteEdge :: Connection -> Text -> IO Bool
deleteEdge conn eid = do
    rows <-
        query
            conn
            (Query "SELECT id FROM edges WHERE id = ?")
            (Only eid) ::
            IO [Only Text]
    case rows of
        [] -> pure False
        _ -> do
            execute conn (Query "DELETE FROM edges WHERE id = ?") (Only eid)
            pure True

-- | Edges whose ULID starts with @prefix@.
getEdgesByPrefix :: Connection -> Text -> IO [Edge]
getEdgesByPrefix conn = prefixLookup conn "edges" edgeCols

-- | Resolve a user-supplied string to a canonical edge ULID via prefix match.
resolveEdgeId :: Connection -> Text -> IO (Either String Text)
resolveEdgeId conn = resolveByPrefix (getEdgesByPrefix conn) edgeId "edge"

-- | Knowledge linked by @references@ edges from the given task.
referencedKnowledge :: Connection -> Text -> IO [Knowledge]
referencedKnowledge conn tid =
    query
        conn
        ( Query $
            "SELECT "
                <> knowColsQualified "k"
                <> " FROM edges e \
                   \JOIN knowledge k ON k.id = e.dst_id \
                   \WHERE e.kind = 'references' \
                   \  AND e.src_kind = 'task' AND e.src_id = ? \
                   \  AND e.dst_kind = 'knowledge' \
                   \ORDER BY e.created_at ASC"
        )
        (Only tid)

-- | Tasks that the given task depends on (depends_on edges, dst side).
dependencyTasks :: Connection -> Text -> IO [Task]
dependencyTasks conn tid =
    query
        conn
        ( Query $
            "SELECT "
                <> taskColsQualified "t"
                <> " FROM edges e \
                   \JOIN tasks t ON t.id = e.dst_id \
                   \WHERE e.kind = 'depends_on' \
                   \  AND e.src_kind = 'task' AND e.src_id = ? \
                   \  AND e.dst_kind = 'task' \
                   \ORDER BY e.created_at ASC"
        )
        (Only tid)

{- | Count outgoing depends_on and references edges for multiple task ids.
Returns @(deps_count, refs_count)@ per task id.
Tasks with no edges are included with counts of 0.
-}
taskEdgeCounts :: Connection -> [Text] -> IO [(Text, (Int, Int))]
taskEdgeCounts _ [] = pure []
taskEdgeCounts conn ids = do
    deps <- query conn (Query (countQ "depends_on")) params :: IO [(Text, Int)]
    refs <- query conn (Query (countQ "references")) params :: IO [(Text, Int)]
    pure
        [ (tid, (fromMaybe 0 (lookup tid deps), fromMaybe 0 (lookup tid refs)))
        | tid <- ids
        ]
  where
    ph = "(" <> T.intercalate "," (replicate (length ids) "?") <> ")"
    countQ k =
        "SELECT src_id, COUNT(*) FROM edges \
        \WHERE kind = '"
            <> k
            <> "' AND src_kind = 'task' AND src_id IN "
            <> ph
            <> " \
               \GROUP BY src_id"
    params = map SQLText ids

{- | Count inbound edges for multiple knowledge ids (all kinds, all source node kinds).
Returns only ids with at least one inbound edge; caller treats missing as 0.
-}
knowledgeInboundCounts :: Connection -> [Text] -> IO [(Text, Int)]
knowledgeInboundCounts _ [] = pure []
knowledgeInboundCounts conn ids =
    query conn (Query q) params :: IO [(Text, Int)]
  where
    ph = "(" <> T.intercalate "," (replicate (length ids) "?") <> ")"
    q =
        "SELECT dst_id, COUNT(*) FROM edges \
        \WHERE dst_kind = 'knowledge' AND dst_id IN "
            <> ph
            <> " \
               \GROUP BY dst_id"
    params = map SQLText ids

{- | Knowledge derived from a task during a specific dispatch window.
Filters by knowledge.created_at >= started_at (and <= ended_at when present)
so that only entries from this dispatch are returned when the same task has
had multiple dispatches.
-}
knowledgeDerivedFromDispatch :: Connection -> Text -> Text -> Maybe Text -> IO [Knowledge]
knowledgeDerivedFromDispatch conn tid startedAt mEndedAt =
    query conn q params
  where
    endClause = case mEndedAt of
        Nothing -> ""
        Just _ -> " AND k.created_at <= ?"
    q =
        Query $
            "SELECT "
                <> knowColsQualified "k"
                <> " FROM edges e \
                   \JOIN knowledge k ON k.id = e.src_id \
                   \WHERE e.kind = 'derived_from' \
                   \  AND e.src_kind = 'knowledge' \
                   \  AND e.dst_kind = 'task' AND e.dst_id = ? \
                   \  AND k.created_at >= ?"
                <> endClause
                <> " ORDER BY k.created_at ASC"
    params = case mEndedAt of
        Nothing -> [SQLText tid, SQLText startedAt]
        Just ea -> [SQLText tid, SQLText startedAt, SQLText ea]
