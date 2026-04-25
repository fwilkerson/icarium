module Icarium.Repo.Knowledge
    ( NewKnowledge(..)
    , KnowledgeUpdate(..)
    , emptyUpdate
    , insertKnowledge
    , getKnowledge
    , listKnowledge
    , updateKnowledge
    , deleteKnowledge
    ) where

import           Data.Maybe             (catMaybes, fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Database.SQLite.Simple (Connection, Only (..), Query (..), SQLData (..), execute,
                                         query)

import           Icarium.Id             (newId)
import           Icarium.Types          (Knowledge (..))

data NewKnowledge = NewKnowledge
    { nkTitle :: Text
    , nkBody  :: Text
    }

data KnowledgeUpdate = KnowledgeUpdate
    { kuTitle :: Maybe Text
    , kuBody  :: Maybe Text
    , kuStale :: Maybe Bool
    }

emptyUpdate :: KnowledgeUpdate
emptyUpdate = KnowledgeUpdate Nothing Nothing Nothing

knowCols :: Text
knowCols = "id, title, body, stale, created_at, updated_at"

insertKnowledge :: Connection -> NewKnowledge -> IO Text
insertKnowledge conn NewKnowledge{..} = do
    kid <- newId
    execute conn
        (Query "INSERT INTO knowledge (id, title, body) VALUES (?, ?, ?)")
        (kid, nkTitle, nkBody)
    pure kid

getKnowledge :: Connection -> Text -> IO (Maybe Knowledge)
getKnowledge conn kid = do
    rows <- query conn
        (Query $ "SELECT " <> knowCols <> " FROM knowledge WHERE id = ?")
        (Only kid)
    pure $ case rows of
        (k:_) -> Just k
        []    -> Nothing

listKnowledge :: Connection -> Bool -> Maybe Text -> Maybe Text -> IO [Knowledge]
listKnowledge conn staleOnly mDomain mDisc =
    query conn q params
  where
    (whereClause, params) = knowCatWhere staleOnly mDomain mDisc
    q = Query $ "SELECT " <> knowCols <> " FROM knowledge" <> whereClause <> " ORDER BY created_at ASC"

knowCatWhere :: Bool -> Maybe Text -> Maybe Text -> (Text, [SQLData])
knowCatWhere staleOnly mDomain mDisc =
    let catSubq axis = "id IN (SELECT knowledge_id FROM knowledge_categories kc"
                    <> " JOIN categories c ON c.id = kc.category_id"
                    <> " WHERE c.axis = '" <> axis <> "' AND c.name = ?)"
        staleClauses = ["stale = 1" | staleOnly]
        catFilters   = catMaybes
            [ fmap (\n -> (catSubq "domain",      SQLText n)) mDomain
            , fmap (\n -> (catSubq "discipline",   SQLText n)) mDisc
            ]
        clauses = staleClauses <> map fst catFilters
        catParams   = map snd catFilters
    in case clauses of
        [] -> ("", [])
        cs -> (" WHERE " <> T.intercalate " AND " cs, catParams)

updateKnowledge :: Connection -> Text -> KnowledgeUpdate -> IO Bool
updateKnowledge conn kid KnowledgeUpdate{..} = do
    mk <- getKnowledge conn kid
    case mk of
        Nothing -> pure False
        Just k  -> do
            let newTitle = fromMaybe (knowledgeTitle k) kuTitle
                newBody  = fromMaybe (knowledgeBody k)  kuBody
                newStale = fromMaybe (knowledgeStale k) kuStale
                staleInt = if newStale then 1 else 0 :: Int
            execute conn
                (Query "UPDATE knowledge SET title=?, body=?, stale=? WHERE id=?")
                (newTitle, newBody, staleInt, kid)
            pure True

deleteKnowledge :: Connection -> Text -> IO Bool
deleteKnowledge conn kid = do
    mk <- getKnowledge conn kid
    case mk of
        Nothing -> pure False
        Just _  -> do
            execute conn (Query "DELETE FROM knowledge WHERE id = ?") (Only kid)
            pure True
