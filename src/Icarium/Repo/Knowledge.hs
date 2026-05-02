module Icarium.Repo.Knowledge (
    NewKnowledge (..),
    KnowledgeUpdate (..),
    emptyUpdate,
    insertKnowledge,
    getKnowledge,
    getKnowledgesByPrefix,
    resolveKnowledgeId,
    listKnowledge,
    updateKnowledge,
    deleteKnowledge,
    categoryMatchedKnowledge,
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
import Icarium.Types (Category (..), CategoryAxis (..), Knowledge (..))

data NewKnowledge = NewKnowledge
    { nkTitle :: Text
    , nkBody :: Text
    }

data KnowledgeUpdate = KnowledgeUpdate
    { kuTitle :: Maybe Text
    , kuBody :: Maybe Text
    , kuStale :: Maybe Bool
    }

emptyUpdate :: KnowledgeUpdate
emptyUpdate = KnowledgeUpdate Nothing Nothing Nothing

knowCols :: Text
knowCols = "id, title, body, stale, created_at, updated_at"

insertKnowledge :: Connection -> NewKnowledge -> IO Text
insertKnowledge conn NewKnowledge{..} = do
    kid <- newId
    execute
        conn
        (Query "INSERT INTO knowledge (id, title, body) VALUES (?, ?, ?)")
        (kid, nkTitle, nkBody)
    pure kid

getKnowledge :: Connection -> Text -> IO (Maybe Knowledge)
getKnowledge conn kid = do
    rows <-
        query
            conn
            (Query $ "SELECT " <> knowCols <> " FROM knowledge WHERE id = ?")
            (Only kid)
    pure $ case rows of
        (k : _) -> Just k
        [] -> Nothing

-- | Escape LIKE special characters so they match literally.
escapeLike :: Text -> Text
escapeLike = T.concatMap esc
  where
    esc c
        | c `elem` ['%', '_', '\\'] = T.pack ['\\', c]
        | otherwise = T.singleton c

-- | Knowledge entries whose ULID starts with @prefix@.
getKnowledgesByPrefix :: Connection -> Text -> IO [Knowledge]
getKnowledgesByPrefix conn prefix =
    query
        conn
        (Query $ "SELECT " <> knowCols <> " FROM knowledge WHERE id LIKE ? ESCAPE '\\'")
        (Only (escapeLike prefix <> "%"))

-- | Resolve a user-supplied string to a canonical knowledge ULID via prefix match.
resolveKnowledgeId :: Connection -> Text -> IO (Either String Text)
resolveKnowledgeId conn input = do
    ks <- getKnowledgesByPrefix conn input
    case ks of
        [k] -> pure (Right (knowledgeId k))
        [] -> pure (Left $ "knowledge not found: " <> T.unpack input)
        _ ->
            pure
                ( Left $
                    "ambiguous id: "
                        <> T.unpack input
                        <> " (matches "
                        <> show (length ks)
                        <> " entries)"
                )

{- | List knowledge entries. @staleFilter@: @Nothing@ = all entries,
@Just True@ = stale only, @Just False@ = exclude stale.
-}
listKnowledge :: Connection -> Maybe Bool -> Maybe Text -> Maybe Text -> IO [Knowledge]
listKnowledge conn staleFilter mDomain mDisc =
    query conn q params
  where
    (whereClause, params) = knowCatWhere staleFilter mDomain mDisc
    q = Query $ "SELECT " <> knowCols <> " FROM knowledge" <> whereClause <> " ORDER BY created_at ASC"

knowCatWhere :: Maybe Bool -> Maybe Text -> Maybe Text -> (Text, [SQLData])
knowCatWhere staleFilter mDomain mDisc =
    let catSubq axis =
            "id IN (SELECT knowledge_id FROM knowledge_categories kc"
                <> " JOIN categories c ON c.id = kc.category_id"
                <> " WHERE c.axis = '"
                <> axis
                <> "' AND c.name = ?)"
        staleClauses = case staleFilter of
            Nothing -> []
            Just True -> ["stale = 1"]
            Just False -> ["stale = 0"]
        catFilters =
            catMaybes
                [ fmap (\n -> (catSubq "domain", SQLText n)) mDomain
                , fmap (\n -> (catSubq "discipline", SQLText n)) mDisc
                ]
        clauses = staleClauses <> map fst catFilters
        catParams = map snd catFilters
     in case clauses of
            [] -> ("", [])
            cs -> (" WHERE " <> T.intercalate " AND " cs, catParams)

updateKnowledge :: Connection -> Text -> KnowledgeUpdate -> IO Bool
updateKnowledge conn kid KnowledgeUpdate{..} = do
    mk <- getKnowledge conn kid
    case mk of
        Nothing -> pure False
        Just k -> do
            let newTitle = fromMaybe (knowledgeTitle k) kuTitle
                newBody = fromMaybe (knowledgeBody k) kuBody
                newStale = fromMaybe (knowledgeStale k) kuStale
                staleInt = if newStale then 1 else 0 :: Int
            execute
                conn
                (Query "UPDATE knowledge SET title=?, body=?, stale=? WHERE id=?")
                (newTitle, newBody, staleInt, kid)
            pure True

deleteKnowledge :: Connection -> Text -> IO Bool
deleteKnowledge conn kid = do
    mk <- getKnowledge conn kid
    case mk of
        Nothing -> pure False
        Just _ -> do
            execute conn (Query "DELETE FROM knowledge WHERE id = ?") (Only kid)
            pure True

{- | Knowledge entries whose categories AND-intersect with the given
category list (one condition per axis present in the input). Excludes
stale entries. Returns [] immediately when the input list is empty.
Cap limits results; order is most-recently-created first.
-}
categoryMatchedKnowledge :: Connection -> [Category] -> Int -> IO [Knowledge]
categoryMatchedKnowledge conn cats cap
    | null clauses = pure []
    | otherwise = query conn q params
  where
    domains = [categoryName c | c <- cats, categoryAxis c == Domain]
    discs = [categoryName c | c <- cats, categoryAxis c == Discipline]
    axisClause axis names =
        let ph = T.intercalate "," (replicate (length names) "?")
         in "id IN (SELECT knowledge_id FROM knowledge_categories kc \
            \JOIN categories c ON c.id = kc.category_id \
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
                <> knowCols
                <> " FROM knowledge"
                <> " WHERE stale = 0 AND "
                <> T.intercalate " AND " clauses
                <> " ORDER BY created_at DESC LIMIT ?"
    params = map SQLText domains <> map SQLText discs <> [SQLInteger (fromIntegral cap)]
