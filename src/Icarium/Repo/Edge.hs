module Icarium.Repo.Edge (
    insertEdge,
    listEdges,
    deleteEdge,
    getEdgesByPrefix,
    resolveEdgeId,
    referencedContexts,
    dependencyTasks,
    derivedFromTasks,
    taskEdgeCounts,
    contextInboundCounts,
    ctxChildContexts,
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
    (:.) (..),
 )

import Icarium.Id (newId)
import Icarium.Repo.Internal (prefixLookup, resolveByPrefix, taskCols)
import Icarium.Types (
    Context,
    Edge (..),
    EdgeKind (..),
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

{- | Context entries linked by @references@ edges from the given task.
An explicit reference delivers regardless of retirement — except
disposition 'stale', which is known-wrong content and never surfaces
(ADR 0001).
-}
referencedContexts :: Connection -> Text -> IO [Context]
referencedContexts conn tid =
    query
        conn
        "SELECT cx.id, cx.title, cx.body, cx.created_at, cx.updated_at \
        \FROM edges e \
        \JOIN context cx ON cx.id = e.dst_id \
        \WHERE e.kind = 'references' \
        \  AND e.src_kind = 'task' AND e.src_id = ? \
        \  AND e.dst_kind = 'context' \
        \  AND cx.id NOT IN (SELECT context_id FROM retired_context WHERE disposition = 'stale') \
        \ORDER BY e.created_at ASC"
        (Only tid)

-- | Tasks that the given task depends on (depends_on edges, dst side).
dependencyTasks :: Connection -> Text -> IO [Task]
dependencyTasks conn tid =
    query
        conn
        ( Query $
            "SELECT "
                <> taskCols "t."
                <> " FROM edges e \
                   \JOIN tasks t ON t.id = e.dst_id \
                   \WHERE e.kind = 'depends_on' \
                   \  AND e.src_kind = 'task' AND e.src_id = ? \
                   \  AND e.dst_kind = 'task' \
                   \ORDER BY e.created_at ASC"
        )
        (Only tid)

{- | Tasks this task was derived from — a follow-up pointing back at the work
that turned it up. Distinct from depends_on: the parent does not block it.
-}
derivedFromTasks :: Connection -> Text -> IO [Task]
derivedFromTasks conn tid =
    query
        conn
        ( Query $
            "SELECT "
                <> taskCols "t."
                <> " FROM edges e \
                   \JOIN tasks t ON t.id = e.dst_id \
                   \WHERE e.kind = 'derived_from' \
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

{- | Count inbound edges for multiple context ids (all kinds, all source node kinds).
Returns only ids with at least one inbound edge; caller treats missing as 0.
-}
contextInboundCounts :: Connection -> [Text] -> IO [(Text, Int)]
contextInboundCounts _ [] = pure []
contextInboundCounts conn ids =
    query conn (Query q) params :: IO [(Text, Int)]
  where
    ph = "(" <> T.intercalate "," (replicate (length ids) "?") <> ")"
    q =
        "SELECT dst_id, COUNT(*) FROM edges \
        \WHERE dst_kind = 'context' AND dst_id IN "
            <> ph
            <> " \
               \GROUP BY dst_id"
    params = map SQLText ids

{- | The "children" of a context: entries on the source side of a
context→context edge pointing at it — they derive from, reference, or
supersede it. Paired with the edge kind that got them there.
-}
ctxChildContexts :: Connection -> Text -> Maybe EdgeKind -> IO [(EdgeKind, Context)]
ctxChildContexts conn dstId mKind = do
    rows <- query conn q params
    pure [(k, cx) | Only k :. cx <- rows]
  where
    kindClause = case mKind of
        Nothing -> ""
        Just _ -> " AND e.kind = ?"
    q =
        Query $
            "SELECT e.kind, cx.id, cx.title, cx.body, cx.created_at, cx.updated_at \
            \FROM edges e \
            \JOIN context cx ON cx.id = e.src_id \
            \WHERE e.dst_id = ? AND e.dst_kind = 'context' AND e.src_kind = 'context'"
                <> kindClause
                <> " ORDER BY e.created_at ASC"
    params = case mKind of
        Nothing -> [SQLText dstId]
        Just k -> [SQLText dstId, SQLText (edgeKindDbText k)]
