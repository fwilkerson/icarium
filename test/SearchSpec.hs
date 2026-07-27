{- | Query parsing and the FTS-backed @searchEntries@ surface.

Match and filter semantics are settled here, once, against the repo
function. The CLI spec proves the flags reach these filters and that hits
render; it does not restate the semantics.
-}
module SearchSpec (tests) where

import Control.Monad (forM_, void)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Icarium.Repo.Search (ParsedQuery (..), Term (..), parseQuery)
import Icarium.Repo.Search qualified as RS
import Icarium.Types

import TestHelpers

tests :: TestTree
tests =
    testGroup
        "search"
        [ testGroup "searchEntries" $
            map matchCase matchCases
                <> [ testCase "title hits outrank body hits in both directions, with match flags" testRanking
                   , testCase "limit caps the result list, total counts past it" testLimit
                   ]
        , testGroup "parseQuery" (map parseCase parseCases)
        ]

-- =============================================================
-- searchEntries: which entries match
-- =============================================================

{- | One matching scenario: seed a labelled corpus, run a query under some
filters, and name the entries that must come back. Compared as a set —
'testRanking' owns the order.
-}
data MatchCase = MatchCase
    { mcName :: String
    , mcSeed :: Connection -> IO [(String, Text)]
    , mcQuery :: Text
    , mcFilters :: RS.SearchFilters
    , mcExpect :: [String]
    }

matchCase :: MatchCase -> TestTree
matchCase MatchCase{..} = testCase mcName $ withTestDb $ \c -> do
    corpus <- mcSeed c
    (_, hits) <- RS.searchEntries c mcQuery mcFilters 10
    let labelOf i = lookup i [(v, k) | (k, v) <- corpus]
    sort (map (labelOf . RS.hitId) hits) @?= sort (map Just mcExpect)

-- | Seed context entries as @(label, title, body)@.
ctxs :: [(String, Text, Text)] -> Connection -> IO [(String, Text)]
ctxs entries c = mapM (\(l, t, b) -> (,) l <$> mkContext c t b) entries

-- | A task and a context sharing a search token, for the kind filters.
taskAndCtx :: Connection -> IO [(String, Text)]
taskAndCtx c = do
    tid <- mkTaskRow c "needle task"
    kid <- mkContext c "needle context" "body"
    pure [("task", tid), ("ctx", kid)]

-- | Seed a category on @axis@ and tag the named context entries with it.
tagged :: CategoryAxis -> Text -> [String] -> (Connection -> IO [(String, Text)]) -> Connection -> IO [(String, Text)]
tagged axis name labels seed c = do
    cat <- mkCat c axis name
    corpus <- seed c
    forM_ [i | (l, i) <- corpus, l `elem` labels] $ \i ->
        attachContextCats c i [cat]
    pure corpus

matchCases :: [MatchCase]
matchCases =
    [ MatchCase
        "whitespace-only query matches nothing"
        (ctxs [("a", "some title", "some body")])
        "   "
        RS.noFilters
        []
    , MatchCase
        "a query no entry carries matches nothing"
        (ctxs [("a", "some title", "some body")])
        "xyzzy_no_match"
        RS.noFilters
        []
    , MatchCase
        "escapeLike: a query containing % matches literally"
        (ctxs [("hit", "100% correct", "body"), ("miss", "unrelated", "body")])
        "100%"
        RS.noFilters
        ["hit"]
    , MatchCase
        "escapeLike: a query containing _ matches literally"
        (ctxs [("hit", "snake_case naming", "body"), ("miss", "unrelated", "body")])
        "snake_case"
        RS.noFilters
        ["hit"]
    , MatchCase
        "AND: multi-word matches tokens in any order, and excludes an entry missing one"
        (ctxs [("hit", "credentials owned by client", "body"), ("miss", "client only", "body")])
        "client credentials"
        RS.noFilters
        ["hit"]
    , MatchCase
        "phrase: exact substring required"
        (ctxs [("hit", "client credentials flow", "body"), ("miss", "credentials for client", "body")])
        "\"client credentials\""
        RS.noFilters
        ["hit"]
    , MatchCase
        "OR: union of token matches"
        (ctxs [("foo", "foo topic", "body"), ("bar", "bar topic", "body"), ("miss", "unrelated", "body")])
        "foo OR bar"
        RS.noFilters
        ["foo", "bar"]
    , MatchCase
        "snake_case: space-separated tokens match the underscore-joined form"
        (ctxs [("hit", "client_credentials", "body"), ("miss", "unrelated", "body")])
        "client credentials"
        RS.noFilters
        ["hit"]
    , MatchCase
        "--kind task excludes context hits"
        taskAndCtx
        "needle"
        RS.noFilters{RS.sfKind = Just TaskNode}
        ["task"]
    , MatchCase
        "--kind ctx excludes task hits"
        taskAndCtx
        "needle"
        RS.noFilters{RS.sfKind = Just ContextNode}
        ["ctx"]
    , MatchCase
        "--domain keeps only entries on that domain"
        (tagged Domain "mydom" ["hit"] (ctxs [("hit", "needle tagged", "body"), ("miss", "needle untagged", "body")]))
        "needle"
        RS.noFilters{RS.sfDomains = ["mydom"]}
        ["hit"]
    , MatchCase
        "--discipline keeps only entries on that discipline"
        (tagged Discipline "mydisc" ["hit"] (ctxs [("hit", "needle tagged", "body"), ("miss", "needle untagged", "body")]))
        "needle"
        RS.noFilters{RS.sfDisciplines = ["mydisc"]}
        ["hit"]
    , MatchCase
        "multiple --domain values are OR'd"
        ( tagged Domain "domB" ["b"] $
            tagged Domain "domA" ["a"] $
                ctxs [("a", "needle entry A", "body"), ("b", "needle entry B", "body"), ("miss", "needle untagged", "body")]
        )
        "needle"
        RS.noFilters{RS.sfDomains = ["domA", "domB"]}
        ["a", "b"]
    , MatchCase
        "--exclude-domain drops entries on that domain"
        (tagged Domain "noisydom" ["miss"] (ctxs [("hit", "needle good", "body"), ("miss", "needle noise", "body")]))
        "needle"
        RS.noFilters{RS.sfExcludeDomains = ["noisydom"]}
        ["hit"]
    , MatchCase
        "--title-only scopes FTS to the title column"
        (ctxs [("hit", "scopetoken in title", "body content"), ("miss", "unrelated title", "scopetoken in body")])
        "scopetoken"
        RS.noFilters{RS.sfScope = RS.ScopeTitle}
        ["hit"]
    , MatchCase
        "--body-only scopes FTS to the body column"
        (ctxs [("miss", "scopetoken in title", "body content"), ("hit", "unrelated title", "scopetoken in body")])
        "scopetoken"
        RS.noFilters{RS.sfScope = RS.ScopeBody}
        ["hit"]
    ]

-- =============================================================
-- searchEntries: rank and match flags
-- =============================================================

{- | Rank is decided by /where/ the hit landed, never by node kind: a title
hit outranks a body hit whichever side of the task\/context divide it is
on. The per-hit match flags are the same signal the renderer badges with.
-}
testRanking :: IO ()
testRanking = withTestDb $ \c -> do
    -- Title hit on a task, body hit on a context.
    tTitle <- mkTaskRow c "needle in a task title"
    kBody <- mkContext c "unrelated" "body carries needle"
    (_, up) <- RS.searchEntries c "needle" RS.noFilters 10
    map RS.hitId up @?= [tTitle, kBody]
    map RS.hitTitleMatch up @?= [True, False]
    map RS.hitBodyMatch up @?= [False, True]

    -- The mirror image: title hit on a context, body hit on a task.
    tBody <- mkTaskBody c "unrelated" "body carries xyzzy" ReadyHeadless
    kTitle <- mkContext c "xyzzy in a context title" "body"
    (_, down) <- RS.searchEntries c "xyzzy" RS.noFilters 10
    map RS.hitId down @?= [kTitle, tBody]

    -- A hit in both columns sets both flags.
    _ <- mkContext c "bmatch_token in title" "bmatch_token in body"
    (_, both) <- RS.searchEntries c "bmatch_token" RS.noFilters 10
    map RS.hitTitleMatch both @?= [True]
    map RS.hitBodyMatch both @?= [True]

testLimit :: IO ()
testLimit = withTestDb $ \c -> do
    forM_ [(1 :: Int) .. 5] $ \i ->
        void $ mkContext c ("needle entry " <> T.pack (show i)) "body"
    (total, results) <- RS.searchEntries c "needle" RS.noFilters 3
    total @?= 5
    length results @?= 3

-- =============================================================
-- parseQuery
-- =============================================================

parseCases :: [(String, Text, ParsedQuery)]
parseCases =
    [ ("single word", "needle", AndQuery [Word "needle"])
    , ("two words are AND'd", "client credentials", AndQuery [Word "client", Word "credentials"])
    , ("quoted phrase", "\"client credentials\"", AndQuery [Phrase "client credentials"])
    , ("explicit OR", "foo OR bar", OrQuery [Word "foo", Word "bar"])
    , ("OR is case-sensitive", "foo or bar", AndQuery [Word "foo", Word "or", Word "bar"])
    , ("whitespace-only", "   ", AndQuery [])
    ]

parseCase :: (String, Text, ParsedQuery) -> TestTree
parseCase (name, input, expected) = testCase name $ parseQuery input @?= expected
