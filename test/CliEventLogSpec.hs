-- | The append-only event log, observed through the commands that write it.
module CliEventLogSpec (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Control.Monad (forM)
import Data.Aeson (Object)
import Data.List (isInfixOf, nub)
import Database.SQLite.Simple (close, execute_, open)
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import CliDispatchHelpers (addReadyTask, runDispatch, withDispatchRepo)
import CliHelpers (expectField, runIcarium, withTempDb)
import TestHelpers (eventField, readEventLog, withHeldWriteLock)

tests :: TestTree
tests =
    testGroup
        "event log"
        [ testCase "event log: task lifecycle appends one line per command" testEventLogTask
        , testCase "event log: start and done name the shorthand that was typed" testEventLogShorthandActor
        , testCase "event log: ctx lifecycle, curation included" testEventLogCtx
        , testCase "event log: a live dispatch logs claim, start, outcome, transition" testEventLogDispatch
        , testCase "event log: a claim that lost the write lock logs nothing" testEventLogClaimLockBusy
        , testCase "event log: a blocked worker logs an escalation with its reason" testEventLogEscalation
        , testCase "event log: concurrent writers never interleave a line" testEventLogConcurrent
        ]

-- | @(event, from, to)@ of one logged line.
eventShape :: Object -> (String, Maybe String, Maybe String)
eventShape o = (expectField "event" o, eventField "from" o, eventField "to" o)

testEventLogTask :: IO ()
testEventLogTask = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "logged task", "--state", "planned"]
    let tid = head (words addOut)
    (upCode, _, _) <- runIcarium db ["task", "update", tid, "--state", "ready-interactive"]
    upCode @?= ExitSuccess
    -- An edit that touches no state is not a transition, so it logs nothing:
    -- a from == to line would be a transition that never happened.
    (titleCode, _, _) <- runIcarium db ["task", "update", tid, "--title", "renamed task"]
    titleCode @?= ExitSuccess
    (clCode, _, _) <- runIcarium db ["task", "claim", tid, "--owner", "tester"]
    clCode @?= ExitSuccess
    (rmCode, _, _) <- runIcarium db ["task", "rm", tid]
    rmCode @?= ExitSuccess

    evs <- readEventLog db
    map eventShape evs
        @?= [ ("task.created", Nothing, Just "planned")
            , ("task.updated", Just "planned", Just "ready_interactive")
            , ("task.claimed", Just "ready_interactive", Just "in_progress")
            , ("task.deleted", Nothing, Nothing)
            ]
    map (expectField "id") evs @?= replicate 4 tid
    map (expectField "kind") evs @?= replicate 4 "task"
    map (expectField "actor") evs @?= ["task add", "task update", "task claim", "task rm"]
    assertBool "claim names its owner" (expectField "owner" (evs !! 2) == "tester")

    -- A command that changed nothing logs nothing.
    (missCode, _, _) <- runIcarium db ["task", "update", tid, "--state", "done"]
    missCode @?= ExitFailure 1
    after <- readEventLog db
    length after @?= 4

-- | The actor is what the user typed, not the code path it routed through.
testEventLogShorthandActor :: IO ()
testEventLogShorthandActor = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["task", "add", "shorthand task"]
    let tid = head (words addOut)
    (startCode, _, _) <- runIcarium db ["task", "start", tid]
    startCode @?= ExitSuccess
    (doneCode, _, _) <- runIcarium db ["task", "done", tid]
    doneCode @?= ExitSuccess

    evs <- readEventLog db
    map (expectField "actor") evs @?= ["task add", "task start", "task done"]

testEventLogCtx :: IO ()
testEventLogCtx = withTempDb $ \db -> do
    (_, addOut, _) <- runIcarium db ["ctx", "add", "logged entry", "--body", "text"]
    let cxid = head (words addOut)
    (upCode, _, _) <- runIcarium db ["ctx", "update", cxid, "--title", "renamed entry"]
    upCode @?= ExitSuccess
    (curCode, _, _) <-
        runIcarium db ["ctx", "curate", cxid, "rule", "--artifact", "some-test"]
    curCode @?= ExitSuccess
    (rmCode, _, _) <- runIcarium db ["ctx", "rm", cxid]
    rmCode @?= ExitSuccess

    evs <- readEventLog db
    map (expectField "event") evs
        @?= ["ctx.created", "ctx.updated", "ctx.curated", "ctx.deleted"]
    map (expectField "kind") evs @?= replicate 4 "ctx"
    map (expectField "id") evs @?= replicate 4 cxid
    map (expectField "actor") evs @?= ["ctx add", "ctx update", "ctx curate", "ctx rm"]
    let curated = evs !! 2
    expectField "to" curated @?= "rule"
    expectField "artifact" curated @?= "some-test"

    -- An update that leaves the row as it found it logs nothing: neither a
    -- bare update nor one whose flags restate what is already there.
    (_, addOut2, _) <- runIcarium db ["ctx", "add", "untouched entry", "--body", "text"]
    let cxid2 = head (words addOut2)
    noops <-
        forM
            [ ["ctx", "update", cxid2]
            , ["ctx", "update", cxid2, "--title", "untouched entry"]
            , ["ctx", "update", cxid2, "--domain", ""]
            ]
            (runIcarium db)
    map (\(c, _, _) -> c) noops @?= replicate 3 ExitSuccess
    after <- readEventLog db
    map (expectField "event") after
        @?= ["ctx.created", "ctx.updated", "ctx.curated", "ctx.deleted", "ctx.created"]

testEventLogDispatch :: IO ()
testEventLogDispatch = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "dispatched task"
    (code, _, _) <- runDispatch dir db (Just "commit") ["dispatch", "run", tid]
    code @?= ExitSuccess

    evs <- readEventLog db
    map (eventField "event") evs
        @?= map
            Just
            ["task.created", "task.claimed", "dispatch.started", "dispatch.finished", "task.updated"]
    let started = evs !! 2
        finished = evs !! 3
        transition = evs !! 4
    eventField "kind" started @?= Just "dispatch"
    eventField "task" started @?= Just tid
    eventField "task" finished @?= Just tid
    eventField "id" finished @?= eventField "id" started
    eventField "to" finished @?= Just "success"
    (eventField "from" transition, eventField "to" transition)
        @?= (Just "in_progress", Just "done")
    eventField "actor" transition @?= Just "dispatch"

{- | The log is append-only, so a claim event written for a claim that never
happened can never be retracted. A held write lock is the cheapest way to
make the claim fail after the command has already read the task row.
-}
testEventLogClaimLockBusy :: IO ()
testEventLogClaimLockBusy = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "contended task"
    (code, _, err) <-
        withHeldWriteLock db $
            runDispatch dir db (Just "commit") ["dispatch", "run", tid]
    code @?= ExitFailure 3
    assertBool "the run reports the busy lock" ("nothing was claimed" `isInfixOf` err)

    evs <- readEventLog db
    map (eventField "event") evs @?= [Just "task.created"]

testEventLogEscalation :: IO ()
testEventLogEscalation = withDispatchRepo $ \dir db -> do
    tid <- addReadyTask dir db "escalating task"
    (_, _, _) <- runDispatch dir db (Just "blocked") ["dispatch", "run", tid]

    evs <- readEventLog db
    map (eventField "event") evs
        @?= map
            Just
            [ "task.created"
            , "task.claimed"
            , "dispatch.started"
            , "dispatch.escalated"
            , "dispatch.finished"
            , "task.updated"
            ]
    let escalated = evs !! 3
    eventField "task" escalated @?= Just tid
    assertBool
        "escalation carries the worker's own reason"
        (maybe False ("needs a decision from a human" `isInfixOf`) (eventField "reason" escalated))
    eventField "to" (evs !! 4) @?= Just "failure"
    (eventField "from" (evs !! 5), eventField "to" (evs !! 5))
        @?= (Just "in_progress", Just "blocked")

{- | The log is the substrate for watchers, so a torn or interleaved line
would corrupt every consumer. 'readEventLog' fails loudly on a line that is
not a whole JSON object, which is the assertion that matters here.
-}
testEventLogConcurrent :: IO ()
testEventLogConcurrent = withTempDb $ \db -> do
    let n = 20 :: Int
    -- Create the store first: the race under test is the append, not migration.
    (seedCode, _, _) <- runIcarium db ["task", "add", "seed"]
    seedCode @?= ExitSuccess

    waits <- forM [1 .. n] $ \i -> do
        done <- newEmptyMVar
        _ <- forkIO (runIcarium db ["task", "add", "concurrent " <> show i] >>= putMVar done)
        pure done
    codes <- mapM (fmap (\(c, _, _) -> c) . takeMVar) waits
    codes @?= replicate n ExitSuccess

    evs <- readEventLog db
    length evs @?= n + 1
    nub (map (eventField "event") evs) @?= [Just "task.created"]
    length (nub (map (eventField "id") evs)) @?= n + 1
