module Icarium.Repo.Context (
    NewContext (..),
    ContextUpdate (..),
    emptyUpdate,
    contextCols,
    insertContext,
    getContext,
    getContextsByPrefix,
    resolveContextId,
    listContexts,
    updateContext,
    deleteContext,
    categoryMatchedContexts,
    listContextIdTimes,
    getContextBody,
    getContextTitle,
    setContextBody,
    contextExists,
) where

import Control.Monad (when)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (
    Connection,
    Only (..),
    Query (..),
    SQLData (..),
    execute,
    query,
    query_,
 )

import Icarium.Id (newId)
import Icarium.Repo.Fts qualified as Fts
import Icarium.Repo.Internal (prefixLookup, resolveByPrefix)
import Icarium.Types (Category (..), CategoryAxis (..), Context (..), NodeKind (..))

data NewContext = NewContext
    { ncTitle :: Text
    , ncBody :: Text
    }

newtype ContextUpdate = ContextUpdate
    { cuTitle :: Maybe Text
    }

emptyUpdate :: ContextUpdate
emptyUpdate = ContextUpdate Nothing

insertContext :: Connection -> NewContext -> IO Text
insertContext conn NewContext{..} = do
    cid <- newId
    execute
        conn
        (Query "INSERT INTO context (id, title, body) VALUES (?, ?, ?)")
        (cid, ncTitle, ncBody)
    Fts.indexEntry conn cid ContextNode ncTitle ncBody
    pure cid

contextCols :: Text
contextCols = "id, title, body, created_at, updated_at"

getContext :: Connection -> Text -> IO (Maybe Context)
getContext conn cid = do
    rows <-
        query
            conn
            (Query $ "SELECT " <> contextCols <> " FROM context WHERE id = ?")
            (Only cid)
    pure $ case rows of
        (k : _) -> Just k
        [] -> Nothing

-- | Context entries whose ULID starts with @prefix@.
getContextsByPrefix :: Connection -> Text -> IO [Context]
getContextsByPrefix conn = prefixLookup conn "context" contextCols

-- | Resolve a user-supplied string to a canonical context ULID via prefix match.
resolveContextId :: Connection -> Text -> IO (Either String Text)
resolveContextId conn = resolveByPrefix (getContextsByPrefix conn) contextId "context"

{- | List context entries. @retiredFilter@: @Nothing@ = all entries,
@Just True@ = retired only, @Just False@ = current only (see the
@retired_context@ view for the derived predicate).
@includeSuperseded@: @False@ excludes entries that are the @dst@ of a
@supersedes@ edge (i.e. older versions); @True@ keeps all entries.
-}
listContexts :: Connection -> Maybe Bool -> Bool -> Maybe Text -> Maybe Text -> IO [Context]
listContexts conn retiredFilter includeSuperseded mDomain mDisc =
    query conn q params
  where
    (whereClause, params) = ctxCatWhere retiredFilter includeSuperseded mDomain mDisc
    q = Query $ "SELECT " <> contextCols <> " FROM context" <> whereClause <> " ORDER BY created_at ASC"

ctxCatWhere :: Maybe Bool -> Bool -> Maybe Text -> Maybe Text -> (Text, [SQLData])
ctxCatWhere retiredFilter includeSuperseded mDomain mDisc =
    let catSubq axis =
            "id IN (SELECT context_id FROM context_categories cc"
                <> " JOIN categories c ON c.id = cc.category_id"
                <> " WHERE c.axis = '"
                <> axis
                <> "' AND c.name = ?)"
        retiredClauses = case retiredFilter of
            Nothing -> []
            Just True -> ["id IN (SELECT context_id FROM retired_context)"]
            Just False -> ["id NOT IN (SELECT context_id FROM retired_context)"]
        catFilters =
            catMaybes
                [ fmap (\n -> (catSubq "domain", SQLText n)) mDomain
                , fmap (\n -> (catSubq "discipline", SQLText n)) mDisc
                ]
        supersededClause =
            [ "id NOT IN (SELECT dst_id FROM edges WHERE kind = 'supersedes' AND dst_kind = 'context')"
            | not includeSuperseded
            ]
        clauses = retiredClauses <> supersededClause <> map fst catFilters
        catParams = map snd catFilters
     in case clauses of
            [] -> ("", [])
            cs -> (" WHERE " <> T.intercalate " AND " cs, catParams)

updateContext :: Connection -> Text -> ContextUpdate -> IO Bool
updateContext conn cid ContextUpdate{..} = do
    mk <- getContext conn cid
    case mk of
        Nothing -> pure False
        Just k -> do
            let newTitle = fromMaybe (contextTitle k) cuTitle
            execute
                conn
                (Query "UPDATE context SET title=? WHERE id=?")
                (newTitle, cid)
            when (isJust cuTitle) $
                Fts.indexEntry conn cid ContextNode newTitle (contextBody k)
            pure True

deleteContext :: Connection -> Text -> IO Bool
deleteContext conn cid = do
    mk <- getContext conn cid
    case mk of
        Nothing -> pure False
        Just _ -> do
            execute conn (Query "DELETE FROM context WHERE id = ?") (Only cid)
            Fts.removeEntry conn cid
            pure True

listContextIdTimes :: Connection -> IO [(Text, Text)]
listContextIdTimes conn = query_ conn "SELECT id, updated_at FROM context"

getContextBody :: Connection -> Text -> IO Text
getContextBody conn cid = do
    rows <- query conn "SELECT body FROM context WHERE id = ?" (Only cid) :: IO [Only Text]
    pure $ case rows of
        (Only b : _) -> b
        [] -> ""

getContextTitle :: Connection -> Text -> IO (Maybe Text)
getContextTitle conn cid = do
    rows <- query conn "SELECT title FROM context WHERE id = ?" (Only cid) :: IO [Only Text]
    pure $ case rows of
        (Only t : _) -> Just t
        [] -> Nothing

setContextBody :: Connection -> Text -> Text -> IO ()
setContextBody conn cid body =
    execute conn "UPDATE context SET body = ? WHERE id = ?" (body, cid)

contextExists :: Connection -> Text -> IO Bool
contextExists conn cid = do
    rows <- query conn "SELECT 1 FROM context WHERE id = ?" (Only cid) :: IO [Only Int]
    pure (not (null rows))

{- | Context entries whose categories AND-intersect with the given
category list (one condition per axis present in the input). Current
entries only — retired entries never auto-pull. Returns [] immediately
when the input list is empty. Cap limits results; order is
most-recently-created first.
-}
categoryMatchedContexts :: Connection -> [Category] -> Int -> IO [Context]
categoryMatchedContexts conn cats cap
    | null clauses = pure []
    | otherwise = query conn q params
  where
    domains = [categoryName c | c <- cats, categoryAxis c == Domain]
    discs = [categoryName c | c <- cats, categoryAxis c == Discipline]
    axisClause axis names =
        let ph = T.intercalate "," (replicate (length names) "?")
         in "id IN (SELECT context_id FROM context_categories cc \
            \JOIN categories c ON c.id = cc.category_id \
            \WHERE c.axis = '"
                <> axis
                <> "' AND c.name IN ("
                <> ph
                <> "))"
    clauses =
        [axisClause "domain" domains | not (null domains)]
            <> [axisClause "discipline" discs | not (null discs)]
    q =
        Query $
            "SELECT "
                <> contextCols
                <> " FROM context"
                <> " WHERE id NOT IN (SELECT context_id FROM retired_context) AND "
                <> T.intercalate " AND " clauses
                <> " ORDER BY created_at DESC LIMIT ?"
    params = map SQLText domains <> map SQLText discs <> [SQLInteger (fromIntegral cap)]
