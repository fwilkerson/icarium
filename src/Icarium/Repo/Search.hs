module Icarium.Repo.Search (
    SearchHit (..),
    Term (..),
    ParsedQuery (..),
    parseQuery,
    searchEntries,
) where

import Data.List (sortBy)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, Query (..), query)

import Icarium.Repo.Context (ctxCols)
import Icarium.Repo.Internal (escapeLike)
import Icarium.Repo.Task (taskCols)
import Icarium.Types

data SearchHit = SearchHit
    { hitId :: Text
    , hitKind :: NodeKind
    , hitTitle :: Text
    , hitBody :: Text
    , hitUpdatedAt :: Text
    , hitTitleMatch :: Bool
    , hitState :: Maybe TaskState
    , hitStale :: Bool
    }

-- | A single token in a query, either an exact phrase (quoted) or a bare word.
data Term
    = Phrase Text
    | Word Text
    deriving (Eq, Show)

-- | Parsed representation of a search query.
data ParsedQuery
    = -- | All terms must appear in the entry (default for bare multi-word input).
      AndQuery [Term]
    | -- | Any term suffices; explicit @OR@ between tokens in the input.
      OrQuery [Term]
    deriving (Eq, Show)

termText :: Term -> Text
termText (Phrase t) = t
termText (Word t) = t

{- | Parse a user query string into structured tokens.

Whitespace-delimited tokens are joined by AND by default.  A bare
uppercase @OR@ token flips the expression to OR-of-terms.  Text
inside double quotes is treated as a phrase (exact-substring match).
-}
parseQuery :: Text -> ParsedQuery
parseQuery q =
    let rawTokens = tokenizeRaw (T.strip q)
        hasOr = "OR" `elem` rawTokens
        terms = [toTerm t | t <- rawTokens, t /= "OR"]
     in if hasOr then OrQuery terms else AndQuery terms
  where
    toTerm t
        | "\"" `T.isPrefixOf` t && "\"" `T.isSuffixOf` t =
            Phrase (T.drop 1 (T.dropEnd 1 t))
        | otherwise = Word t

tokenizeRaw :: Text -> [Text]
tokenizeRaw "" = []
tokenizeRaw t
    | "\"" `T.isPrefixOf` t =
        let (phrase, rest) = T.breakOn "\"" (T.drop 1 t)
         in if T.null rest
                then [phrase]
                else ("\"" <> phrase <> "\"") : tokenizeRaw (T.stripStart (T.drop 1 rest))
    | otherwise =
        let (tok, rest) = T.break (== ' ') t
         in tok : tokenizeRaw (T.stripStart rest)

{- | Build a SQL WHERE clause and LIKE-pattern parameters for @pq@.

For an @AndQuery@, every term must match (title OR body); if all terms
are bare @Word@s, an additional OR branch matches the underscore-joined
form so that @client credentials@ finds @client_credentials@.
For an @OrQuery@, any one term matching is sufficient.
-}
buildWhere :: Text -> Text -> ParsedQuery -> (Text, [Text])
buildWhere tc bc pq = case pq of
    AndQuery terms ->
        let andClauses = map mkTermClause terms
            andExpr = parens (T.intercalate " AND " (map fst andClauses))
            andParams = concatMap snd andClauses
         in case snakeClause terms of
                Nothing -> (andExpr, andParams)
                Just (sc, sp) -> (parens (andExpr <> " OR " <> sc), andParams ++ sp)
    OrQuery terms ->
        let orClauses = map mkTermClause terms
            orExpr = parens (T.intercalate " OR " (map fst orClauses))
            orParams = concatMap snd orClauses
         in (orExpr, orParams)
  where
    likePat txt = "%" <> escapeLike txt <> "%"
    mkTermClause term =
        let p = likePat (termText term)
         in (parens (tc <> " LIKE ? ESCAPE '\\' OR " <> bc <> " LIKE ? ESCAPE '\\'"), [p, p])
    snakeClause terms =
        let words' = [w | Word w <- terms]
         in if length words' >= 2
                then
                    let p = likePat (T.intercalate "_" words')
                     in Just (parens (tc <> " LIKE ? ESCAPE '\\' OR " <> bc <> " LIKE ? ESCAPE '\\'"), [p, p])
                else Nothing
    parens s = "(" <> s <> ")"

-- | Check whether @title@ satisfies the title-match criterion for ranking.
matchesTitle :: ParsedQuery -> Text -> Bool
matchesTitle pq title = case pq of
    AndQuery terms ->
        all (termIn titleLower) terms
            || let words' = [w | Word w <- terms]
                in length words' >= 2
                    && T.toLower (T.intercalate "_" words') `T.isInfixOf` titleLower
    OrQuery terms -> any (termIn titleLower) terms
  where
    titleLower = T.toLower title
    termIn haystack term = T.toLower (termText term) `T.isInfixOf` haystack

{- | Search tasks and context for @q@. Results ranked: title hits first,
then updated_at DESC. @mKind@ narrows to one table; @Nothing@ = both.
-}
searchEntries :: Connection -> Text -> Maybe NodeKind -> Int -> IO [SearchHit]
searchEntries conn q mKind limit = do
    taskHits <- case mKind of
        Just ContextNode -> pure []
        _ -> searchTasks conn pq
    ctxHits <- case mKind of
        Just TaskNode -> pure []
        _ -> searchContexts conn pq
    let ranked = sortBy rankHit (taskHits ++ ctxHits)
    pure (take limit ranked)
  where
    pq = parseQuery q
    rankHit a b = case compare (hitTitleMatch b) (hitTitleMatch a) of
        EQ -> compare (hitUpdatedAt b) (hitUpdatedAt a)
        o -> o

searchTasks :: Connection -> ParsedQuery -> IO [SearchHit]
searchTasks conn pq = do
    rows <- query conn sql params :: IO [Task]
    pure (map toHit rows)
  where
    (whereClause, params) = buildWhere "title" "body" pq
    sql = Query $ "SELECT " <> taskCols <> " FROM tasks WHERE " <> whereClause
    toHit t =
        SearchHit
            { hitId = taskId t
            , hitKind = TaskNode
            , hitTitle = taskTitle t
            , hitBody = taskBody t
            , hitUpdatedAt = taskUpdatedAt t
            , hitTitleMatch = matchesTitle pq (taskTitle t)
            , hitState = Just (taskState t)
            , hitStale = False
            }

searchContexts :: Connection -> ParsedQuery -> IO [SearchHit]
searchContexts conn pq = do
    rows <- query conn sql params :: IO [Context]
    pure (map toHit rows)
  where
    (whereClause, params) = buildWhere "title" "body" pq
    sql = Query $ "SELECT " <> ctxCols <> " FROM context WHERE " <> whereClause
    toHit cx =
        SearchHit
            { hitId = contextId cx
            , hitKind = ContextNode
            , hitTitle = contextTitle cx
            , hitBody = contextBody cx
            , hitUpdatedAt = contextUpdatedAt cx
            , hitTitleMatch = matchesTitle pq (contextTitle cx)
            , hitState = Nothing
            , hitStale = contextStale cx
            }
