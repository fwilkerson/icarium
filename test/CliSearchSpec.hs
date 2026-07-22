{- | CLI contract for @icarium search@ and the body-file/FTS sync that feeds
it (mtime sweep, orphan scan, @reindex@).
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
        [ testCase "search: title-match and body-match both surface" testSearchBothKinds
        , testCase "search: title hit on context outranks body hit on task" testSearchCrossKindRank
        , testCase "search: --kind task excludes context results" testSearchKindTask
        , testCase "search: --no-snippet suppresses indented line" testSearchNoSnippet
        , testCase "search: empty result prints (no matches)" testSearchNoMatches
        , testCase "search: multi-word AND matches both tokens in any order" testSearchAndTokens
        , testCase "search: quoted phrase requires contiguous match" testSearchPhraseSemantics
        , testCase "search: OR returns union of token matches" testSearchOrSemantics
        , testCase "search: space-separated tokens find underscore-joined corpus entry" testSearchSnakeCaseFallback
        , testCase "search: --limit truncation shows footer with total count" testSearchTruncationFooter
        , testCase "search: retired context ranks below current context at same relevance" testSearchRetiredLast
        , testCase "search: snippet collapses embedded newlines to single line" testSearchSnippetSingleLine
        , testCase "search: match-source indicator [t] shown for title-only match" testSearchMatchSourceTitle
        , testCase "search: match-source indicator [b] shown for body-only match" testSearchMatchSourceBody
        , testCase "search: match-source indicator [t+b] shown when match in both" testSearchMatchSourceBoth
        , testCase "search: --title-only excludes body-only matches" testSearchTitleOnlyFlag
        , testCase "search: --body-only excludes title-only matches" testSearchBodyOnlyFlag
        , testCase "search: --domain filters to tagged entries" testSearchDomainFlag
        , testCase "search: --exclude-discipline removes tagged entries" testSearchExcludeDisciplineFlag
        , testCase "search --json: {total, hits} object; total counts past --limit" testSearchJson
        , testCase "mtime sweep: external body edit is re-indexed" testMtimeSweepReindex
        , testCase "orphan sweep: stray .md file moved to trash on sync command" testOrphanRemoval
        , testCase "orphan sweep: read-only command leaves orphan body file intact" testReadOnlyCommandPreservesOrphan
        , testCase "reindex: rebuilds FTS from DB after body_fts wipe" testReindexRestoresFts
        ]

testSearchBothKinds :: IO ()
testSearchBothKinds = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["task", "add", "mytoken in title task", "--state", "ready-headless"]
    (_, _, _) <- runIcarium db ["ctx", "add", "unrelated context", "--body", "body contains mytoken here"]
    (code, out, _) <- runIcarium db ["search", "mytoken"]
    code @?= ExitSuccess
    assertBool "title match surfaces" ("mytoken in title task" `isInfixOf` out)
    assertBool "body match surfaces" ("unrelated context" `isInfixOf` out)

testSearchCrossKindRank :: IO ()
testSearchCrossKindRank = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["task", "add", "no match here", "--body", "body has xyzzy123", "--state", "ready-headless"]
    (_, kOut, _) <- runIcarium db ["ctx", "add", "xyzzy123 in title context"]
    let kid = take 10 (head (words kOut))
    (code, out, _) <- runIcarium db ["search", "xyzzy123"]
    code @?= ExitSuccess
    let outLines = lines out
    assertBool "context title hit appears first" (kid `isInfixOf` head outLines)

testSearchKindTask :: IO ()
testSearchKindTask = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["task", "add", "needle task", "--state", "ready-headless"]
    (_, _, _) <- runIcarium db ["ctx", "add", "needle context"]
    (code, out, _) <- runIcarium db ["search", "needle", "--kind", "task"]
    code @?= ExitSuccess
    assertBool "task result present" ("needle task" `isInfixOf` out)
    assertBool "context result absent" (not ("needle context" `isInfixOf` out))

testSearchNoSnippet :: IO ()
testSearchNoSnippet = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["ctx", "add", "some title", "--body", "the body has needle123 inside it"]
    (code, out, _) <- runIcarium db ["search", "needle123", "--no-snippet"]
    code @?= ExitSuccess
    assertBool "title line present" ("some title" `isInfixOf` out)
    assertBool "snippet line absent" (not ("needle123" `isInfixOf` unlines (tail (lines out))))

testSearchNoMatches :: IO ()
testSearchNoMatches = withTempDb $ \db -> do
    (code, out, _) <- runIcarium db ["search", "xyzzy_nothing_matches_this_99"]
    code @?= ExitSuccess
    assertBool "(no matches) printed" ("(no matches)" `isInfixOf` out)

testSearchAndTokens :: IO ()
testSearchAndTokens = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["ctx", "add", "credentials owned by client"]
    (_, _, _) <- runIcarium db ["ctx", "add", "client only"]
    (code, out, _) <- runIcarium db ["search", "client credentials"]
    code @?= ExitSuccess
    assertBool "both-token entry present" ("credentials owned by client" `isInfixOf` out)
    assertBool "single-token entry absent" (not ("client only" `isInfixOf` out))

testSearchPhraseSemantics :: IO ()
testSearchPhraseSemantics = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["ctx", "add", "client credentials flow"]
    (_, _, _) <- runIcarium db ["ctx", "add", "credentials for client"]
    (code, out, _) <- runIcarium db ["search", "\"client credentials\""]
    code @?= ExitSuccess
    assertBool "exact phrase present" ("client credentials flow" `isInfixOf` out)
    assertBool "out-of-order absent" (not ("credentials for client" `isInfixOf` out))

testSearchOrSemantics :: IO ()
testSearchOrSemantics = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["ctx", "add", "foo topic"]
    (_, _, _) <- runIcarium db ["ctx", "add", "bar topic"]
    (_, _, _) <- runIcarium db ["ctx", "add", "unrelated"]
    (code, out, _) <- runIcarium db ["search", "foo OR bar"]
    code @?= ExitSuccess
    assertBool "foo entry present" ("foo topic" `isInfixOf` out)
    assertBool "bar entry present" ("bar topic" `isInfixOf` out)
    assertBool "unrelated absent" (not ("unrelated" `isInfixOf` out))

testSearchSnakeCaseFallback :: IO ()
testSearchSnakeCaseFallback = withTempDb $ \db -> do
    (_, _, _) <- runIcarium db ["ctx", "add", "client_credentials"]
    (_, _, _) <- runIcarium db ["ctx", "add", "unrelated"]
    (code, out, _) <- runIcarium db ["search", "client credentials"]
    code @?= ExitSuccess
    assertBool "snake_case entry found" ("client_credentials" `isInfixOf` out)
    assertBool "unrelated absent" (not ("unrelated" `isInfixOf` out))

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
    let liveId = take 10 (head (words liveOut))
    (_, retiredOut, _) <- runIcarium db ["ctx", "add", "zqtoken retired entry"]
    let retiredId = take 10 (head (words retiredOut))
    (_, _, _) <- runIcarium db ["ctx", "curate", retiredId, "stale"]
    (code, out, _) <- runIcarium db ["search", "zqtoken"]
    code @?= ExitSuccess
    let outLines = lines out
        liveIdx = head [i | (i, l) <- zip [0 :: Int ..] outLines, liveId `isInfixOf` l]
        retiredIdx = head [i | (i, l) <- zip [0 :: Int ..] outLines, retiredId `isInfixOf` l]
    assertBool "live entry ranks above retired entry" (liveIdx < retiredIdx)

testSearchSnippetSingleLine :: IO ()
testSearchSnippetSingleLine = withTempDb $ \db -> do
    let multilineBody = "line one\nline two with needleXYZ here\nline three"
    (_, _, _) <- runIcarium db ["ctx", "add", "multiline body entry", "--body", multilineBody]
    (code, out, _) <- runIcarium db ["search", "needleXYZ"]
    code @?= ExitSuccess
    let snippetLines = filter ("needleXYZ" `isInfixOf`) (lines out)
    assertBool "snippet line found" (not (null snippetLines))
    assertBool "snippet is a single line (no embedded newlines)" (length snippetLines == 1)

testSearchMatchSourceTitle :: IO ()
testSearchMatchSourceTitle = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "titletoken in title", "--body", "unrelated body"]
    let cid = take 10 (head (words addOut))
    (code, out, _) <- runIcarium db ["search", "titletoken"]
    code @?= ExitSuccess
    let hitLine = head [l | l <- lines out, cid `isInfixOf` l]
    assertBool "row carries [t] indicator" ("[t]" `isInfixOf` hitLine)
    assertBool "row does not carry [b] indicator" (not ("[b]" `isInfixOf` hitLine))

testSearchMatchSourceBody :: IO ()
testSearchMatchSourceBody = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "neutral title", "--body", "bodytoken appears here"]
    let cid = take 10 (head (words addOut))
    (code, out, _) <- runIcarium db ["search", "bodytoken"]
    code @?= ExitSuccess
    let hitLine = head [l | l <- lines out, cid `isInfixOf` l]
    assertBool "row carries [b] indicator" ("[b]" `isInfixOf` hitLine)
    assertBool "row does not carry [t+b] indicator" (not ("[t+b]" `isInfixOf` hitLine))

testSearchMatchSourceBoth :: IO ()
testSearchMatchSourceBoth = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "bothtoken in title", "--body", "and bothtoken in body"]
    let cid = take 10 (head (words addOut))
    (code, out, _) <- runIcarium db ["search", "bothtoken"]
    code @?= ExitSuccess
    let hitLine = head [l | l <- lines out, cid `isInfixOf` l]
    assertBool "row carries [t+b] indicator" ("[t+b]" `isInfixOf` hitLine)

testSearchTitleOnlyFlag :: IO ()
testSearchTitleOnlyFlag = withTempDb $ \db -> do
    (_, titleAdd, _) <- runIcarium db ["ctx", "add", "scopetoken in title", "--body", "unrelated"]
    let titleId = take 10 (head (words titleAdd))
    (_, bodyAdd, _) <- runIcarium db ["ctx", "add", "unrelated heading", "--body", "scopetoken in body"]
    let bodyId = take 10 (head (words bodyAdd))
    (code, out, _) <- runIcarium db ["search", "scopetoken", "--title-only"]
    code @?= ExitSuccess
    assertBool "title-only hit retained" (titleId `isInfixOf` out)
    assertBool "body-only hit excluded" (not (bodyId `isInfixOf` out))

testSearchBodyOnlyFlag :: IO ()
testSearchBodyOnlyFlag = withTempDb $ \db -> do
    (_, titleAdd, _) <- runIcarium db ["ctx", "add", "btoken in title", "--body", "unrelated"]
    let titleId = take 10 (head (words titleAdd))
    (_, bodyAdd, _) <- runIcarium db ["ctx", "add", "unrelated heading", "--body", "btoken in body"]
    let bodyId = take 10 (head (words bodyAdd))
    (code, out, _) <- runIcarium db ["search", "btoken", "--body-only"]
    code @?= ExitSuccess
    assertBool "body-only hit retained" (bodyId `isInfixOf` out)
    assertBool "title-only hit excluded" (not (titleId `isInfixOf` out))

testSearchDomainFlag :: IO ()
testSearchDomainFlag = withTempDb $ \db -> do
    -- Seed an untagged ctx first so the DB and schema exist, then insert
    -- a `domain=cli` category via raw SQL (CLI tests don't use TestHelpers).
    (_, untaggedAdd, _) <- runIcarium db ["ctx", "add", "domtoken untagged"]
    let untaggedId = take 10 (head (words untaggedAdd))
    seedCategory db "domain" "cli"
    (_, taggedAdd, _) <- runIcarium db ["ctx", "add", "domtoken tagged", "--domain", "cli"]
    let taggedId = take 10 (head (words taggedAdd))
    (code, out, _) <- runIcarium db ["search", "domtoken", "--domain", "cli"]
    code @?= ExitSuccess
    assertBool "tagged entry retained" (taggedId `isInfixOf` out)
    assertBool "untagged entry excluded" (not (untaggedId `isInfixOf` out))

testSearchExcludeDisciplineFlag :: IO ()
testSearchExcludeDisciplineFlag = withTempDb $ \db -> do
    (_, keptAdd, _) <- runIcarium db ["ctx", "add", "exctoken kept"]
    let keptId = take 10 (head (words keptAdd))
    seedCategory db "discipline" "haskell"
    (_, noisyAdd, _) <- runIcarium db ["ctx", "add", "exctoken noisy", "--discipline", "haskell"]
    let noisyId = take 10 (head (words noisyAdd))
    (code, out, _) <- runIcarium db ["search", "exctoken", "--exclude-discipline", "haskell"]
    code @?= ExitSuccess
    assertBool "non-tagged entry retained" (keptId `isInfixOf` out)
    assertBool "haskell-tagged entry excluded" (not (noisyId `isInfixOf` out))

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
