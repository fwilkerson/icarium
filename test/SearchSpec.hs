-- | Query parsing and the FTS-backed @searchEntries@ surface.
module SearchSpec (tests) where

import Control.Monad (forM_, void)
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Repo.Search (ParsedQuery (..), Term (..), parseQuery)
import Icarium.Repo.Search qualified as RS
import Icarium.Repo.Task qualified as RT
import Icarium.Types

import TestHelpers

tests :: TestTree
tests =
    testGroup
        "search"
        [ testGroup
            "searchEntries"
            [ testCase "whitespace-only query returns (0, [])" testSearchWhitespaceOnly
            , testCase "title hit ranks before body hit" testSearchTitleBeforeBody
            , testCase "title hit from context outranks body hit from task" testSearchCrossKindRank
            , testCase "escapeLike: query containing % matches literally" testSearchEscapePercent
            , testCase "escapeLike: query containing _ matches literally" testSearchEscapeUnderscore
            , testCase "--kind task excludes context hits" testSearchKindTask
            , testCase "--kind ctx excludes task hits" testSearchKindCtx
            , testCase "limit caps result count" testSearchLimit
            , testCase "no match returns empty list" testSearchNoMatch
            , testCase "AND: multi-word matches tokens in any order" testSearchAndTokens
            , testCase "AND: entry missing one token excluded" testSearchAndExcludes
            , testCase "phrase: exact substring required" testSearchPhrase
            , testCase "OR: union of token matches" testSearchOrTokens
            , testCase "snake_case: space-separated tokens match underscore-joined form" testSearchSnakeCase
            , testCase "--domain filter includes only domain-tagged entries" testSearchDomainFilter
            , testCase "--discipline filter includes only discipline-tagged entries" testSearchDisciplineFilter
            , testCase "multiple --domain values are OR'd" testSearchMultiDomainOr
            , testCase "--exclude-domain removes tagged entries" testSearchExcludeDomain
            , testCase "--title-only scopes FTS to title column" testSearchTitleOnly
            , testCase "--body-only scopes FTS to body column" testSearchBodyOnly
            , testCase "hitBodyMatch set for body matches" testSearchHitBodyMatch
            ]
        , testGroup
            "parseQuery"
            [ testCase "single word → AndQuery [Word]" testParseQueryWord
            , testCase "two words → AndQuery [Word, Word]" testParseQueryAnd
            , testCase "quoted phrase → AndQuery [Phrase]" testParseQueryPhrase
            , testCase "explicit OR → OrQuery" testParseQueryOr
            , testCase "OR is case-sensitive (lowercase not treated as OR)" testParseQueryOrCase
            , testCase "whitespace-only → AndQuery []" testParseQueryWhitespace
            ]
        ]

-- =============================================================
-- searchEntries
-- =============================================================

testSearchWhitespaceOnly :: IO ()
testSearchWhitespaceOnly = withTestDb $ \c -> do
    _ <- mkContext c "some title" "some body"
    (total, results) <- RS.searchEntries c "   " RS.noFilters 10
    total @?= 0
    null results @?= True

testSearchTitleBeforeBody :: IO ()
testSearchTitleBeforeBody = withTestDb $ \c -> do
    tid <- mkTaskRow c "fts needle title"
    kid <- mkContext c "unrelated title" "body contains needle here"
    (_, results) <- RS.searchEntries c "needle" RS.noFilters 10
    assertBool "two results returned" (length results == 2)
    RS.hitId (head results) @?= tid
    RS.hitTitleMatch (head results) @?= True
    RS.hitId (results !! 1) @?= kid
    RS.hitTitleMatch (results !! 1) @?= False

testSearchCrossKindRank :: IO ()
testSearchCrossKindRank = withTestDb $ \c -> do
    _ <-
        RT.insertTask
            c
            RT.NewTask
                { RT.ntTitle = "no match title"
                , RT.ntBody = "body has xyzzy"
                , RT.ntState = ReadyHeadless
                , RT.ntPriority = Nothing
                , RT.ntNoCommit = False
                , RT.ntRouting = mempty
                }
    kid <- mkContext c "title has xyzzy" "body"
    (_, results) <- RS.searchEntries c "xyzzy" RS.noFilters 10
    assertBool "context title hit before task body hit" (RS.hitId (head results) == kid)
    RS.hitTitleMatch (head results) @?= True

testSearchEscapePercent :: IO ()
testSearchEscapePercent = withTestDb $ \c -> do
    kid <- mkContext c "100% correct" "body"
    _ <- mkContext c "unrelated" "body"
    (_, results) <- RS.searchEntries c "100%" RS.noFilters 10
    length results @?= 1
    RS.hitId (head results) @?= kid

testSearchEscapeUnderscore :: IO ()
testSearchEscapeUnderscore = withTestDb $ \c -> do
    kid <- mkContext c "snake_case naming" "body"
    _ <- mkContext c "unrelated" "body"
    (_, results) <- RS.searchEntries c "snake_case" RS.noFilters 10
    length results @?= 1
    RS.hitId (head results) @?= kid

testSearchKindTask :: IO ()
testSearchKindTask = withTestDb $ \c -> do
    tid <- mkTaskRow c "needle task"
    _ <- mkContext c "needle context" "body"
    (_, results) <- RS.searchEntries c "needle" RS.noFilters{RS.sfKind = Just TaskNode} 10
    length results @?= 1
    RS.hitId (head results) @?= tid

testSearchKindCtx :: IO ()
testSearchKindCtx = withTestDb $ \c -> do
    _ <- mkTaskRow c "needle task"
    kid <- mkContext c "needle context" "body"
    (_, results) <- RS.searchEntries c "needle" RS.noFilters{RS.sfKind = Just ContextNode} 10
    length results @?= 1
    RS.hitId (head results) @?= kid

testSearchLimit :: IO ()
testSearchLimit = withTestDb $ \c -> do
    forM_ [(1 :: Int) .. 5] $ \i ->
        void $ mkContext c ("needle entry " <> T.pack (show i)) "body"
    (_, results) <- RS.searchEntries c "needle" RS.noFilters 3
    length results @?= 3

testSearchNoMatch :: IO ()
testSearchNoMatch = withTestDb $ \c -> do
    _ <- mkContext c "some title" "some body"
    (_, results) <- RS.searchEntries c "xyzzy_no_match" RS.noFilters 10
    null results @?= True

testSearchAndTokens :: IO ()
testSearchAndTokens = withTestDb $ \c -> do
    kid <- mkContext c "credentials owned by client" "body"
    _ <- mkContext c "client only" "body"
    (_, results) <- RS.searchEntries c "client credentials" RS.noFilters 10
    length results @?= 1
    RS.hitId (head results) @?= kid

testSearchAndExcludes :: IO ()
testSearchAndExcludes = withTestDb $ \c -> do
    _ <- mkContext c "only alpha here" "body"
    (_, results) <- RS.searchEntries c "alpha beta" RS.noFilters 10
    null results @?= True

testSearchPhrase :: IO ()
testSearchPhrase = withTestDb $ \c -> do
    kid <- mkContext c "client credentials flow" "body"
    _ <- mkContext c "credentials for client" "body"
    (_, results) <- RS.searchEntries c "\"client credentials\"" RS.noFilters 10
    length results @?= 1
    RS.hitId (head results) @?= kid

testSearchOrTokens :: IO ()
testSearchOrTokens = withTestDb $ \c -> do
    _ <- mkContext c "foo topic" "body"
    _ <- mkContext c "bar topic" "body"
    _ <- mkContext c "unrelated" "body"
    (_, results) <- RS.searchEntries c "foo OR bar" RS.noFilters 10
    length results @?= 2

testSearchSnakeCase :: IO ()
testSearchSnakeCase = withTestDb $ \c -> do
    kid <- mkContext c "client_credentials" "body"
    _ <- mkContext c "unrelated" "body"
    (_, results) <- RS.searchEntries c "client credentials" RS.noFilters 10
    length results @?= 1
    RS.hitId (head results) @?= kid

testSearchDomainFilter :: IO ()
testSearchDomainFilter = withTestDb $ \c -> do
    domCat <- mkCat c Domain "mydom"
    kid <- mkContext c "needle in domain entry" "body"
    attachContextCats c kid [domCat]
    _ <- mkContext c "needle no domain" "body"
    (_, results) <- RS.searchEntries c "needle" RS.noFilters{RS.sfDomains = ["mydom"]} 10
    length results @?= 1
    RS.hitId (head results) @?= kid

testSearchDisciplineFilter :: IO ()
testSearchDisciplineFilter = withTestDb $ \c -> do
    discCat <- mkCat c Discipline "mydisc"
    kid <- mkContext c "needle in discipline entry" "body"
    attachContextCats c kid [discCat]
    _ <- mkContext c "needle no discipline" "body"
    (_, results) <- RS.searchEntries c "needle" RS.noFilters{RS.sfDisciplines = ["mydisc"]} 10
    length results @?= 1
    RS.hitId (head results) @?= kid

testSearchMultiDomainOr :: IO ()
testSearchMultiDomainOr = withTestDb $ \c -> do
    domA <- mkCat c Domain "domA"
    domB <- mkCat c Domain "domB"
    kidA <- mkContext c "needle entry A" "body"
    attachContextCats c kidA [domA]
    kidB <- mkContext c "needle entry B" "body"
    attachContextCats c kidB [domB]
    _ <- mkContext c "needle entry C untagged" "body"
    (_, results) <- RS.searchEntries c "needle" RS.noFilters{RS.sfDomains = ["domA", "domB"]} 10
    length results @?= 2
    assertBool "domA entry present" (any (\h -> RS.hitId h == kidA) results)
    assertBool "domB entry present" (any (\h -> RS.hitId h == kidB) results)

testSearchExcludeDomain :: IO ()
testSearchExcludeDomain = withTestDb $ \c -> do
    domCat <- mkCat c Domain "noisydom"
    kidExcluded <- mkContext c "needle noise entry" "body"
    attachContextCats c kidExcluded [domCat]
    kidKept <- mkContext c "needle good entry" "body"
    (_, results) <-
        RS.searchEntries c "needle" RS.noFilters{RS.sfExcludeDomains = ["noisydom"]} 10
    length results @?= 1
    RS.hitId (head results) @?= kidKept

testSearchTitleOnly :: IO ()
testSearchTitleOnly = withTestDb $ \c -> do
    kidTitle <- mkContext c "scopetoken in title" "body content only"
    _ <- mkContext c "unrelated title" "scopetoken in body only"
    (_, results) <- RS.searchEntries c "scopetoken" RS.noFilters{RS.sfScope = RS.ScopeTitle} 10
    length results @?= 1
    RS.hitId (head results) @?= kidTitle

testSearchBodyOnly :: IO ()
testSearchBodyOnly = withTestDb $ \c -> do
    _ <- mkContext c "bodytoken in title" "body content only"
    kidBody <- mkContext c "unrelated title" "bodytoken in body only"
    (_, results) <- RS.searchEntries c "bodytoken" RS.noFilters{RS.sfScope = RS.ScopeBody} 10
    length results @?= 1
    RS.hitId (head results) @?= kidBody

testSearchHitBodyMatch :: IO ()
testSearchHitBodyMatch = withTestDb $ \c -> do
    kidBodyOnly <- mkContext c "unrelated title" "bmatch_token lives here"
    kidBoth <- mkContext c "bmatch_token in title too" "bmatch_token in body"
    (_, results) <- RS.searchEntries c "bmatch_token" RS.noFilters 10
    let findHit i = head [h | h <- results, RS.hitId h == i]
        hBody = findHit kidBodyOnly
        hBoth = findHit kidBoth
    RS.hitTitleMatch hBody @?= False
    RS.hitBodyMatch hBody @?= True
    RS.hitTitleMatch hBoth @?= True
    RS.hitBodyMatch hBoth @?= True

-- =============================================================
-- parseQuery
-- =============================================================

testParseQueryWord :: IO ()
testParseQueryWord =
    parseQuery "needle" @?= AndQuery [Word "needle"]

testParseQueryAnd :: IO ()
testParseQueryAnd =
    parseQuery "client credentials" @?= AndQuery [Word "client", Word "credentials"]

testParseQueryPhrase :: IO ()
testParseQueryPhrase =
    parseQuery "\"client credentials\"" @?= AndQuery [Phrase "client credentials"]

testParseQueryOr :: IO ()
testParseQueryOr =
    parseQuery "foo OR bar" @?= OrQuery [Word "foo", Word "bar"]

testParseQueryOrCase :: IO ()
testParseQueryOrCase =
    parseQuery "foo or bar" @?= AndQuery [Word "foo", Word "or", Word "bar"]

testParseQueryWhitespace :: IO ()
testParseQueryWhitespace =
    parseQuery "   " @?= AndQuery []
