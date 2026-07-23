module GateSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.Aeson (Value (..), decode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Query (..), close, execute, query_)
import Database.SQLite.Simple.Types (Only (..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Icarium.Config (CommandsConfig (..))
import Icarium.Db (migrateDb, openDb)
import Icarium.Dispatch.Gate (GateEnv (..), GateHeartbeat (..), runGate, runGates)
import Icarium.Heartbeat (pidAlive)
import Icarium.Repo.Task qualified as RT
import Icarium.Schema (applySchema)
import Icarium.Types (TaskState (..))
import TestHelpers (insertTestDispatch)

tests :: TestTree
tests =
    testGroup
        "gate"
        [ testCase "empty command is a no-op" testEmptyGate
        , testCase "failing command reports its exit code" testExitCode
        , testCase "gates stop at the first failure" testStopsAtFirstFailure
        , testCase "runGates without [commands] says so" testNoCommands
        , testCase "stdin is closed, not inherited" testStdinClosed
        , testCase "wedged gate is reaped and recorded as a timeout" testWedgedGate
        , testCase "gate output keeps the heartbeat warm" testHeartbeat
        ]

-- | A gate with no dispatch attached: merge-rebase shape.
plainEnv :: FilePath -> Int -> GateEnv
plainEnv dir budget =
    GateEnv{geDir = dir, geBudgetUsecs = budget, geLogPath = Nothing, geHeartbeat = Nothing}

testEmptyGate :: IO ()
testEmptyGate = do
    r <- runGate (plainEnv "." oneMinute) "   "
    r @?= Right ()

testExitCode :: IO ()
testExitCode = do
    r <- runGate (plainEnv "." oneMinute) "exit 3"
    r @?= Left "exit 3 -> exit 3"

testStopsAtFirstFailure :: IO ()
testStopsAtFirstFailure = withSystemTempDirectory "icarium-gate" $ \dir -> do
    let cc = CommandsConfig{ccBuild = "exit 1", ccTest = "touch ran-test"}
    r <- runGates (plainEnv dir oneMinute) (Just cc)
    r @?= Left "exit 1 -> exit 1"
    ran <- doesFileExist (dir </> "ran-test")
    assertBool "test gate must not run after a failed build gate" (not ran)

testNoCommands :: IO ()
testNoCommands = do
    r <- runGates (plainEnv "." oneMinute) Nothing
    r @?= Left "no [commands] section configured"

{- | @cat@ blocks forever on an inherited terminal; on /dev/null it sees EOF
and exits 0. The budget is the failure mode, so keep it short.
-}
testStdinClosed :: IO ()
testStdinClosed = do
    r <- runGate (plainEnv "." 3_000_000) "cat > /dev/null"
    r @?= Right ()

testWedgedGate :: IO ()
testWedgedGate = withGateDispatch $ \env _ -> do
    r <- runGate env "sleep 600"
    case r of
        Right () -> assertFailure "expected the wedged gate to fail"
        Left note -> do
            assertBool ("note names the gate: " <> T.unpack note) ("sleep 600" `T.isInfixOf` note)
            assertBool ("note names the budget: " <> T.unpack note) ("timed out after" `T.isInfixOf` note)
    forensics <- maybe (pure Nothing) readForensics (geLogPath env)
    case forensics of
        Nothing -> assertFailure "no gate_timeout line in the dispatch log"
        Just (gate, tree) -> do
            gate @?= "sleep 600"
            assertBool "forensics captured the process tree" (length tree >= 2)
            dead <- waitAllDead (treePids tree)
            assertBool ("processes survived the kill: " <> show tree) dead

testHeartbeat :: IO ()
testHeartbeat = withGateDispatch $ \env dbPath ->
    bracket (openDb dbPath) close $ \c -> do
        execute
            c
            (Query "UPDATE dispatches SET heartbeat_at = ? WHERE id = ?")
            ("2020-01-01 00:00:00" :: Text, gateDid)
        r <- runGate env "echo icarium-gate-heartbeat"
        r @?= Right ()
        beats <- query_ c (Query "SELECT heartbeat_at FROM dispatches") :: IO [Only Text]
        case beats of
            [Only hb] -> assertBool "heartbeat advanced during the gate" (hb /= "2020-01-01 00:00:00")
            other -> assertFailure ("expected one dispatch row, got " <> show (length other))

-- =============================================================
-- Helpers
-- =============================================================

oneMinute :: Int
oneMinute = 60_000_000

{- | A gate env attached to a real dispatch row in an on-disk DB — the
heartbeat thread and the forensics dump both address it by path.
-}
withGateDispatch :: (GateEnv -> FilePath -> IO a) -> IO a
withGateDispatch k = withSystemTempDirectory "icarium-gate" $ \dir -> do
    let dbPath = dir </> "icarium.db"
    bracket (openDb dbPath) close $ \c -> do
        applySchema c
        migrateDb c
        tid <- RT.insertTask c (newTask "gate test task")
        insertTestDispatch c gateDid tid
    k
        GateEnv
            { geDir = dir
            , geBudgetUsecs = 700_000
            , geLogPath = Just (dir </> "dispatch.jsonl")
            , geHeartbeat = Just GateHeartbeat{ghDbPath = dbPath, ghDid = gateDid}
            }
        dbPath

gateDid :: Text
gateDid = "01GATE00000000000000000000"

newTask :: Text -> RT.NewTask
newTask title =
    RT.NewTask
        { RT.ntTitle = title
        , RT.ntBody = ""
        , RT.ntState = ReadyHeadless
        , RT.ntPriority = Nothing
        , RT.ntNoCommit = False
        , RT.ntRouting = mempty
        }

-- | The gate command and process-tree rows from the log's forensics line.
readForensics :: FilePath -> IO (Maybe (Text, [Text]))
readForensics path = do
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            raw <- BLC.readFile path
            pure $ case [o | Just (Object o) <- map decode (BLC.lines raw)] of
                (o : _) | KM.lookup "type" o == Just (String "icarium.gate_timeout") -> do
                    String gate <- KM.lookup "gate" o
                    Array rows <- KM.lookup "tree" o
                    pure (gate, [t | String t <- foldr (:) [] rows])
                _ -> Nothing

-- | Pids from @ps@ rows, skipping the header.
treePids :: [Text] -> [Int]
treePids rows = [p | r <- drop 1 rows, Just p <- [readPid r]]
  where
    readPid r = case T.words r of
        (w : _) -> case reads (T.unpack w) of
            [(n, "")] -> Just n
            _ -> Nothing
        _ -> Nothing

waitAllDead :: [Int] -> IO Bool
waitAllDead pids = go (20 :: Int)
  where
    go 0 = allDead
    go n =
        allDead >>= \case
            True -> pure True
            False -> threadDelay 100_000 >> go (n - 1)
    allDead = and <$> mapM (fmap not . pidAlive) pids
