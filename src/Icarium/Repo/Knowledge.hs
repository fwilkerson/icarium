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

import Data.Text (Text)
import Database.SQLite.Simple
    ( Connection, Only(..), Query(..), execute, query, query_
    )

import Icarium.Id (newId)
import Icarium.Types (Knowledge(..))

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

listKnowledge :: Connection -> Bool {- staleOnly -} -> IO [Knowledge]
listKnowledge conn staleOnly = do
    rows <- query_ conn
        (Query $ "SELECT " <> knowCols <> " FROM knowledge ORDER BY created_at ASC")
    pure $ if staleOnly then filter knowledgeStale rows else rows

updateKnowledge :: Connection -> Text -> KnowledgeUpdate -> IO Bool
updateKnowledge conn kid KnowledgeUpdate{..} = do
    mk <- getKnowledge conn kid
    case mk of
        Nothing -> pure False
        Just k  -> do
            let newTitle = maybe (knowledgeTitle k) id kuTitle
                newBody  = maybe (knowledgeBody k)  id kuBody
                newStale = maybe (knowledgeStale k) id kuStale
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
