{- | CLI contract for @icarium search@ and the body-file/FTS sync that feeds
it (mtime sweep, orphan scan, @reindex@).

Match and rank semantics belong to 'SearchSpec'. What is proved here is
the part only the CLI owns: that argv reaches the query parser and the
filters, and that hits render.
-}
module CliSearchSpec (tests) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.List (isInfixOf)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Database.SQLite.Simple (Query (..), close, execute, execute_, open)
import System.Directory (doesFileExist, setModificationTime)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import CliHelpers (decodeOut, expectField, expectObject, runIcarium, withTempDb)

tests :: TestTree
tests =
    testGroup
        "search"
        [ testCase "search: task and context hits surface, each badged by match source" testMatchSources
        , testCase "search: bare and quoted multi-word queries reach the parser" testMultiWordQuery
        , testCase "search: --kind task excludes context results" testSearchKindTask
        , testCase "search: --title-only and --body-only scope the query" testScopeFlags
        , testCase "search: --domain keeps and --exclude-discipline drops tagged entries" testCategoryFlags
        , testCase "search: --no-snippet suppresses indented line" testSearchNoSnippet
        , testCase "search: snippet collapses embedded newlines to single line" testSearchSnippetSingleLine
        , testCase "search: empty result prints (no matches)" testSearchNoMatches
        , testCase "search: --limit truncation shows footer with total count" testSearchTruncationFooter
        , testCase "search: retired context ranks below current context" testSearchRetiredLast
        , testCase "search --json: {total, hits} object; total counts past --limit" testSearchJson
        , testCase "mtime sweep: external body edit is re-indexed" testMtimeSweepReindex
        , testCase "orphan sweep: stray .md file moved to trash on sync command" testOrphanRemoval
        , testCase "orphan sweep: read-only command leaves orphan body file intact" testReadOnlyCommandPreservesOrphan
        , testCase "reindex: rebuilds FTS from DB after body_fts wipe" testReindexRestoresFts
        ]

-- | The 10-char id prefix @ctx add@/@task add@ prints on its first line.
addedId :: String -> String
addedId out = take 10 (head (words out))

{- | One corpus, one query: a task hit and a context hit both surface, and
each row carries the indicator for the column its match landed in.
-}
testMatchSources :: IO ()
testMatchSources = withTempDb $ \db -> do
    (_, taskAdd, _) <- runIcarium db ["task", "add", "srctoken in a task title", "--state", "ready-headless"]
    (_, bodyAdd, _) <- runIcarium db ["ctx", "add", "neutral title", "--body", "srctoken appears here"]
    (_, bothAdd, _) <- runIcarium db ["ctx", "add", "srctoken in title", "--body", "and srctoken in body"]
    (code, out, _) <- runIcarium db ["search", "srctoken"]
    code @?= ExitSuccess
    let rowFor addOut = head [l | l <- lines out, addedId addOut `isInfixOf` l]
    assertBool "title-only row badged [t]" ("[t]" `isInfixOf` rowFor taskAdd)
    assertBool "title-only row not badged [b]" (not ("[b]" `isInfixOf` rowFor taskAdd))
    assertBool "body-only row badged [b]" ("[b]" `isInfixOf` rowFor bodyAdd)
    assertBool "both-columns row badged [t+b]" ("[t+b]" `isInfixOf` rowFor bothAdd)

-- | Both query forms survive argv; the parser's semantics are SearchSpec's.
testMultiWordQuery :: IO ()
testMultiWordQuery = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["ctx", "add", "client credentials flow"]
    (_, _, _) <- runIcarium db ["ctx", "add", "credentials for client"]
    (_, _, _) <- runIcarium db ["ctx", "add", "client only"]
    (bareCode, bare, _) <- runIcarium db ["search", "client credentials"]
    bareCode @?= ExitSuccess
    assertBool "both-token entry present" ("client credentials flow" `isInfixOf` bare)
    assertBool "single-token entry absent" (not ("client only" `isInfixOf` bare))
    (phraseCode, phrase, _) <- runIcarium db ["search", "\"client credentials\""]
    phraseCode @?= ExitSuccess
    assertBool "exact phrase present" ("client credentials flow" `isInfixOf` phrase)
    assertBool "out-of-order absent" (not ("credentials for client" `isInfixOf` phrase))

testSearchKindTask :: IO ()
testSearchKindTask = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["task", "add", "needle task", "--state", "ready-headless"]
    (_, _, _) <- runIcarium db ["ctx", "add", "needle context"]
    (code, out, _) <- runIcarium db ["search", "needle", "--kind", "task"]
    code @?= ExitSuccess
    assertBool "task result present" ("needle task" `isInfixOf` out)
    assertBool "context result absent" (not ("needle context" `isInfixOf` out))

testScopeFlags :: IO ()
testScopeFlags = withTempDb $ \db -> do
    (_, titleAdd, _) <- runIcarium db ["ctx", "add", "scopetoken in title", "--body", "unrelated"]
    (_, bodyAdd, _) <- runIcarium db ["ctx", "add", "unrelated heading", "--body", "scopetoken in body"]
    let titleId = addedId titleAdd
        bodyId = addedId bodyAdd
    (tCode, tOut, _) <- runIcarium db ["search", "scopetoken", "--title-only"]
    tCode @?= ExitSuccess
    assertBool "title-only hit retained" (titleId `isInfixOf` tOut)
    assertBool "body-only hit excluded" (not (bodyId `isInfixOf` tOut))
    (bCode, bOut, _) <- runIcarium db ["search", "scopetoken", "--body-only"]
    bCode @?= ExitSuccess
    assertBool "body-only hit retained" (bodyId `isInfixOf` bOut)
    assertBool "title-only hit excluded" (not (titleId `isInfixOf` bOut))

testCategoryFlags :: IO ()
testCategoryFlags = withTempDb $ \db -> do
    -- Seed an untagged ctx first so the DB and schema exist, then insert the
    -- categories via raw SQL (CLI tests don't use TestHelpers).
    (_, untaggedAdd, _) <- runIcarium db ["ctx", "add", "cattoken untagged"]
    let untaggedId = addedId untaggedAdd
    seedCategory db "domain" "cli"
    seedCategory db "discipline" "haskell"
    (_, domAdd, _) <- runIcarium db ["ctx", "add", "cattoken on a domain", "--domain", "cli"]
    (_, discAdd, _) <- runIcarium db ["ctx", "add", "cattoken on a discipline", "--discipline", "haskell"]
    let domId = addedId domAdd
        discId = addedId discAdd
    (iCode, included, _) <- runIcarium db ["search", "cattoken", "--domain", "cli"]
    iCode @?= ExitSuccess
    assertBool "domain-tagged entry retained" (domId `isInfixOf` included)
    assertBool "untagged entry excluded" (not (untaggedId `isInfixOf` included))
    (eCode, excluded, _) <- runIcarium db ["search", "cattoken", "--exclude-discipline", "haskell"]
    eCode @?= ExitSuccess
    assertBool "untagged entry retained" (untaggedId `isInfixOf` excluded)
    assertBool "discipline-tagged entry excluded" (not (discId `isInfixOf` excluded))

testSearchNoSnippet :: IO ()
testSearchNoSnippet = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["ctx", "add", "some title", "--body", "the body has needle123 inside it"]
    (code, out, _) <- runIcarium db ["search", "needle123", "--no-snippet"]
    code @?= ExitSuccess
    assertBool "title line present" ("some title" `isInfixOf` out)
    assertBool "snippet line absent" (not ("needle123" `isInfixOf` unlines (tail (lines out))))

testSearchSnippetSingleLine :: IO ()
testSearchSnippetSingleLine = withTempDb $ \db -> do
    let multilineBody = "line one\nline two with needleXYZ here\nline three"
    (_, _, _) <- runIcarium db ["ctx", "add", "multiline body entry", "--body", multilineBody]
    (code, out, _) <- runIcarium db ["search", "needleXYZ"]
    code @?= ExitSuccess
    let snippetLines = filter ("needleXYZ" `isInfixOf`) (lines out)
    assertBool "snippet line found" (not (null snippetLines))
    assertBool "snippet is a single line (no embedded newlines)" (length snippetLines == 1)

testSearchNoMatches :: IO ()
testSearchNoMatches = withTempDb $ \db -> do
    (code, out, _) <- runIcarium db ["search", "xyzzy_nothing_matches_this_99"]
    code @?= ExitSuccess
    assertBool "(no matches) printed" ("(no matches)" `isInfixOf` out)

testSearchTruncationFooter :: IO ()
testSearchTruncationFooter = withTempDb $ \db -> do
    mapM_
        (\i -> runIcarium db ["ctx", "add", "needle entry " ++ show (i :: Int)])
        [1 .. 5]
    -- limit 3 with 5 total matches should produce a footer
    (code, out, _) <- runIcarium db ["search", "needle", "--limit", "3"]
    code @?= ExitSuccess
    assertBool "footer mentions total count" ("of 5 matches" `isInfixOf` out)
    assertBool "footer mentions shown count" ("showing 3" `isInfixOf` out)
    -- when all results fit, no footer
    (code2, out2, _) <- runIcarium db ["search", "needle", "--limit", "10"]
    code2 @?= ExitSuccess
    assertBool "no footer when all results fit" (not ("showing" `isInfixOf` out2))

testSearchRetiredLast :: IO ()
testSearchRetiredLast = withTempDb $ \db -> do
    (_, liveOut, _) <- runIcarium db ["ctx", "add", "zqtoken live entry"]
    (_, retiredOut, _) <- runIcarium db ["ctx", "add", "zqtoken retired entry"]
    let liveId = addedId liveOut
        retiredId = addedId retiredOut
    (_, _, _) <- runIcarium db ["ctx", "curate", retiredId, "stale"]
    (code, out, _) <- runIcarium db ["search", "zqtoken"]
    code @?= ExitSuccess
    let outLines = lines out
        idx i = head [n | (n, l) <- zip [0 :: Int ..] outLines, i `isInfixOf` l]
    assertBool "live entry ranks above retired entry" (idx liveId < idx retiredId)

seedCategory :: FilePath -> String -> String -> IO ()
seedCategory db axis name = do
    conn <- open db
    execute
        conn
        (Query "INSERT INTO categories (id, axis, name) VALUES (?, ?, ?)")
        ("01TESTCAT" ++ replicate 14 '0' ++ take 3 (name ++ "XXX"), axis, name)
    close conn

testSearchJson :: IO ()
testSearchJson = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "jsontoken searchable entry"]
    let cxid = head (words addOut)
        searchJson q extra = do
            (code, out, _) <- runIcarium db (["search", q, "--json"] <> extra)
            code @?= ExitSuccess
            let o = expectObject (decodeOut out)
                hits = case KM.lookup "hits" o of
                    Just (Array vs) -> map (expectField "id" . expectObject) (toList vs)
                    other -> error ("expected hits array, got: " <> show other)
                total = case KM.lookup "total" o of
                    Just (Number n) -> n
                    other -> error ("expected numeric total, got: " <> show other)
            pure (total, hits)

    (missTotal, missHits) <- searchJson "xyzzy_nothing_matches_this_99" []
    missTotal @?= 0
    missHits @?= []

    (total, hits) <- searchJson "jsontoken" []
    total @?= 1
    hits @?= [cxid]

    -- total counts all matches even when --limit truncates the hit list
    (_, _, _) <- runIcarium db ["ctx", "add", "jsontoken second entry"]
    (cappedTotal, cappedHits) <- searchJson "jsontoken" ["--limit", "1"]
    cappedTotal @?= 2
    length cappedHits @?= 1

{- | Write new content to a body file and set its mtime to a far-future
time so the sweep condition (file_mtime > updated_at) is guaranteed to
fire on the next sync command.  The tasks_touch trigger resets updated_at on
every UPDATE, so we cannot rewind it via SQL; bumping the file mtime is
the reliable alternative.
-}
testMtimeSweepReindex :: IO ()
testMtimeSweepReindex = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    (_, addOut, _) <- runIcarium db ["task", "add", "sweep-test task", "--body", "original xsweep1"]
    let outLines = lines addOut
        bodyPath = outLines !! 1
    -- external edit with future mtime so sweep always triggers
    writeFile bodyPath "edited xsweep2"
    setModificationTime bodyPath (UTCTime (fromGregorian 2099 1 1) 0)
    -- search triggers withDbSync → mtimeSweep runs and reindexes the edited file
    _ <- runIcarium db ["search", "xsweep1"]
    -- new content must be findable via FTS
    (code, out, _) <- runIcarium db ["search", "xsweep2"]
    code @?= ExitSuccess
    assertBool "edited body content surfaces in search" ("sweep-test task" `isInfixOf` out)

{- | A .md file placed in bodies/tasks/ with no matching DB row is an
orphan.  When a sync command runs, the orphan is moved to .trash/ and a
warn: line is emitted on stderr.  The original path is vacated.
-}
testOrphanRemoval :: IO ()
testOrphanRemoval = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
        orphanFile = dir </> "bodies" </> "tasks" </> "01ORPHAN0000000000000000XX.md"
    -- seed the DB so that bodies/tasks/ gets created
    _ <- runIcarium db ["task", "add", "seed task"]
    -- plant a stray file with no DB row
    writeFile orphanFile "orphan content"
    -- search is a sync command; triggers orphanScan
    (_, _, err) <- runIcarium db ["search", "seed"]
    assertBool "warn: emitted for orphan" ("warn:" `isInfixOf` err)
    gone <- not <$> doesFileExist orphanFile
    assertBool "orphan file moved from original location" gone

{- | A read-only command must NOT trigger mtimeSweep, so an orphan body file
placed before the command must still be present afterwards.
-}
testReadOnlyCommandPreservesOrphan :: IO ()
testReadOnlyCommandPreservesOrphan = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
        orphanFile = dir </> "bodies" </> "tasks" </> "01ORPHANREAD00000000000000.md"
    -- seed the DB so that bodies/tasks/ gets created
    _ <- runIcarium db ["task", "add", "read-only seed task"]
    -- plant a stray file with no DB row
    writeFile orphanFile "orphan content"
    -- task list is a read-only command; must not run orphanScan
    _ <- runIcarium db ["task", "list"]
    stillThere <- doesFileExist orphanFile
    assertBool "orphan file untouched by read-only command" stillThere

{- | After wiping body_fts, search should miss (we pin updated_at to the
future so mtimeSweep does not auto-repair it).  icarium reindex rebuilds
the index from the body column; search must find the entry afterwards.
-}
testReindexRestoresFts :: IO ()
testReindexRestoresFts = withSystemTempDirectory "icarium-test" $ \dir -> do
    let db = dir <> "/icarium.db"
    _ <- runIcarium db ["ctx", "add", "reindex test entry", "--body", "xreindex999 unique token"]
    -- wipe FTS and pin updated_at to the future so mtimeSweep won't repair it
    conn <- open db
    execute_ conn "DELETE FROM body_fts"
    execute_ conn "UPDATE context SET updated_at = '2099-01-01 00:00:00'"
    close conn
    -- search must miss before reindex
    (_, outBefore, _) <- runIcarium db ["search", "xreindex999"]
    assertBool "search misses before reindex" (not ("reindex test entry" `isInfixOf` outBefore))
    -- reindex rebuilds FTS from DB body column
    (rCode, _, _) <- runIcarium db ["reindex"]
    rCode @?= ExitSuccess
    -- now search must find the entry
    (code, out, _) <- runIcarium db ["search", "xreindex999"]
    code @?= ExitSuccess
    assertBool "entry found after reindex" ("reindex test entry" `isInfixOf` out)
