module Icarium.Repo.Search (
    SearchHit (..),
    SearchScope (..),
    SearchFilters (..),
    noFilters,
    Term (..),
    ParsedQuery (..),
    parseQuery,
    searchEntries,
) where

import Data.List (sortBy)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, Only (..), Query (..), SQLData (..), query, (:.) (..))

import Icarium.Repo.Internal (inClause, qualified)
import Icarium.Types

data SearchHit = SearchHit
    { hitId :: Text
    , hitKind :: NodeKind
    , hitTitle :: Text
    , hitBody :: Text
    , hitUpdatedAt :: Text
    , hitTitleMatch :: Bool
    , hitBodyMatch :: Bool
    , hitState :: Maybe TaskState
    , hitRetired :: Bool
    }

data SearchScope = ScopeAll | ScopeTitle | ScopeBody

data SearchFilters = SearchFilters
    { sfKind :: Maybe NodeKind
    , sfDomains :: [Text]
    , sfDisciplines :: [Text]
    , sfExcludeDomains :: [Text]
    , sfExcludeDisciplines :: [Text]
    , sfScope :: SearchScope
    }

noFilters :: SearchFilters
noFilters = SearchFilters Nothing [] [] [] [] ScopeAll

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

queryTerms :: ParsedQuery -> [Term]
queryTerms (AndQuery ts) = ts
queryTerms (OrQuery ts) = ts

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

{- | Convert a parsed query to an FTS5 MATCH string.
Words and phrases are wrapped in double quotes to escape FTS5 special chars.
AndQuery terms are joined by space (FTS5 implicit AND);
OrQuery terms are joined by OR.
When scope is ScopeTitle or ScopeBody, each term is prefixed with a
column filter so FTS5 matches only that column.
-}
buildFts5Query :: SearchScope -> ParsedQuery -> Text
buildFts5Query scope pq = case pq of
    AndQuery terms -> T.intercalate " " (map (applyScope . quoteTerm) terms)
    OrQuery terms -> T.intercalate " OR " (map (applyScope . quoteTerm) terms)
  where
    applyScope t = case scope of
        ScopeAll -> t
        ScopeTitle -> "{title}: " <> t
        ScopeBody -> "{body}: " <> t
    quoteTerm term = "\"" <> T.replace "\"" "\"\"" (termText term) <> "\""

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

-- | Check whether @body@ contains any of the query terms (plain substring).
matchesBody :: ParsedQuery -> Text -> Bool
matchesBody pq body = case pq of
    AndQuery terms -> all (termIn bodyLower) terms
    OrQuery terms -> any (termIn bodyLower) terms
  where
    bodyLower = T.toLower body
    termIn haystack term = T.toLower (termText term) `T.isInfixOf` haystack

{- | Search tasks and context for @q@. Returns @(total, results)@ where
@total@ is the count before the limit is applied. Results are ranked:
title hits first, current before retired within the same tier, then
updated_at DESC.
-}
searchEntries :: Connection -> Text -> SearchFilters -> Int -> IO (Int, [SearchHit])
searchEntries conn q filters limit
    | null (queryTerms pq) = pure (0, [])
    | otherwise = do
        taskHits <- case sfKind filters of
            Just ContextNode -> pure []
            _ -> searchTasks conn pq filters
        ctxHits <- case sfKind filters of
            Just TaskNode -> pure []
            _ -> searchContexts conn pq filters
        let ranked = sortBy rankHit (taskHits ++ ctxHits)
            total = length ranked
        pure (total, take limit ranked)
  where
    pq = parseQuery q
    rankHit a b = case compare (hitTitleMatch b) (hitTitleMatch a) of
        EQ -> case compare (hitRetired a) (hitRetired b) of
            EQ -> compare (hitUpdatedAt b) (hitUpdatedAt a)
            o -> o
        o -> o

{- | Build extra WHERE clause fragments and parameters for category
include/exclude filters. The node table must have an @id@ column that
is the primary key; the subquery selects from @joinTable@ whose FK
column is @fkCol@.
-}
buildCatClauses ::
    -- | join table (task_categories | context_categories)
    Text ->
    -- | FK column in join table (task_id | context_id)
    Text ->
    -- | include domain names (OR within axis)
    [Text] ->
    -- | include discipline names (OR within axis)
    [Text] ->
    -- | exclude domain names
    [Text] ->
    -- | exclude discipline names
    [Text] ->
    ([Text], [SQLData])
buildCatClauses jt fkCol inclDomains inclDiscs exclDomains exclDiscs =
    let pairs =
            one "domain" inclDomains True
                ++ one "discipline" inclDiscs True
                ++ one "domain" exclDomains False
                ++ one "discipline" exclDiscs False
     in (map fst pairs, concatMap snd pairs)
  where
    one _ [] _ = []
    one axis names include =
        let ph = inClause names
            subq =
                "id IN (SELECT "
                    <> fkCol
                    <> " FROM "
                    <> jt
                    <> " JOIN categories c ON c.id = "
                    <> jt
                    <> ".category_id"
                    <> " WHERE c.axis = '"
                    <> axis
                    <> "' AND c.name IN "
                    <> ph
                    <> ")"
            clause = if include then subq else "NOT " <> subq
         in [(clause, map SQLText names)]

searchTasks :: Connection -> ParsedQuery -> SearchFilters -> IO [SearchHit]
searchTasks conn pq filters = do
    rows <- query conn sql allParams :: IO [Task]
    pure (map toHit rows)
  where
    ftsQ = buildFts5Query (sfScope filters) pq
    (catCl, catPs) =
        buildCatClauses
            "task_categories"
            "task_id"
            (sfDomains filters)
            (sfDisciplines filters)
            (sfExcludeDomains filters)
            (sfExcludeDisciplines filters)
    extraWhere
        | null catCl = ""
        | otherwise = " AND " <> T.intercalate " AND " catCl
    sql =
        Query $
            "SELECT "
                <> qualified "" taskCols
                <> " FROM tasks"
                <> " WHERE id IN (SELECT id FROM body_fts WHERE body_fts MATCH ? AND kind = 'task')"
                <> extraWhere
    allParams = SQLText ftsQ : catPs
    toHit t =
        SearchHit
            { hitId = taskId t
            , hitKind = TaskNode
            , hitTitle = taskTitle t
            , hitBody = taskBody t
            , hitUpdatedAt = taskUpdatedAt t
            , hitTitleMatch = matchesTitle pq (taskTitle t)
            , hitBodyMatch = matchesBody pq (taskBody t)
            , hitState = Just (taskState t)
            , hitRetired = False
            }

searchContexts :: Connection -> ParsedQuery -> SearchFilters -> IO [SearchHit]
searchContexts conn pq filters = do
    rows <- query conn sql allParams :: IO [Context :. Only Bool]
    pure (map toHit rows)
  where
    ftsQ = buildFts5Query (sfScope filters) pq
    (catCl, catPs) =
        buildCatClauses
            "context_categories"
            "context_id"
            (sfDomains filters)
            (sfDisciplines filters)
            (sfExcludeDomains filters)
            (sfExcludeDisciplines filters)
    extraWhere
        | null catCl = ""
        | otherwise = " AND " <> T.intercalate " AND " catCl
    sql =
        Query $
            "SELECT "
                <> qualified "" contextCols
                <> ", id IN (SELECT context_id FROM retired_context) FROM context"
                <> " WHERE id IN (SELECT id FROM body_fts WHERE body_fts MATCH ? AND kind = 'context')"
                <> extraWhere
    allParams = SQLText ftsQ : catPs
    toHit (cx :. Only retired) =
        SearchHit
            { hitId = contextId cx
            , hitKind = ContextNode
            , hitTitle = contextTitle cx
            , hitBody = contextBody cx
            , hitUpdatedAt = contextUpdatedAt cx
            , hitTitleMatch = matchesTitle pq (contextTitle cx)
            , hitBodyMatch = matchesBody pq (contextBody cx)
            , hitState = Nothing
            , hitRetired = retired
            }
