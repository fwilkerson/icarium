module Icarium.Commands.Doctor (Options, parser, run) where

import Control.Monad (filterM)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, getCurrentTime)
import Options.Applicative
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode (..), exitWith)

import Icarium.Bodies (bodiesDir, ctxBodyPath, taskBodyPath)
import Icarium.Config (
    DispatchConfig (..),
    cfgDispatch,
    defaultConfigPath,
    loadConfig,
 )
import Icarium.Db (dbSchemaVersion, withDb)
import Icarium.Heartbeat (heartbeatStale, pidAlive)
import Icarium.Repo.Context qualified as RCx
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Repo.Task qualified as RT
import Icarium.Schema (schemaVersion)
import Icarium.Types (Context (..), Dispatch (..), Task (..), TaskState (..))

data Options = Options

parser :: Parser Options
parser = pure Options

data CheckResult = OK String | WARN String | FAIL String

data Check = Check
    { checkName :: String
    , checkResult :: CheckResult
    }

run :: FilePath -> Options -> IO ()
run dbPath _ = do
    basic <-
        sequence
            [ checkConfig
            , checkFile "database" dbPath
            , checkSchema dbPath
            , checkBinary "claude"
            , checkBinary "git"
            ]
    orphans <- checkOrphanedDispatches dbPath
    bodies <- checkBodies dbPath
    let checks = basic ++ orphans ++ bodies
    mapM_ printCheck checks
    if any isFail checks
        then exitWith (ExitFailure 2)
        else putStrLn "all checks passed."

isFail :: Check -> Bool
isFail (Check _ (FAIL _)) = True
isFail _ = False

checkFile :: String -> FilePath -> IO Check
checkFile name path = do
    e <- doesFileExist path
    pure $
        Check name $
            if e
                then OK path
                else FAIL ("missing: " <> path)

checkConfig :: IO Check
checkConfig = do
    e <- doesFileExist defaultConfigPath
    if not e
        then pure $ Check "config" (FAIL ("missing: " <> defaultConfigPath))
        else do
            r <- loadConfig defaultConfigPath
            pure $ Check "config" $ case r of
                Right _ -> OK defaultConfigPath
                Left msg -> FAIL ("parse error\n" <> msg)

checkBinary :: String -> IO Check
checkBinary name = do
    r <- findExecutable name
    pure $
        Check ("bin:" <> name) $
            maybe (FAIL "not on PATH") OK r

checkSchema :: FilePath -> IO Check
checkSchema dbPath = do
    e <- doesFileExist dbPath
    if not e
        then pure $ Check "schema" (FAIL "no database")
        else do
            v <- withDb dbPath dbSchemaVersion
            let expected = fromIntegral schemaVersion :: Integer
                actual = fromIntegral v :: Integer
            pure $
                Check "schema" $
                    if actual == expected
                        then OK ("v" <> show actual)
                        else
                            FAIL
                                ( "expected v"
                                    <> show expected
                                    <> ", got v"
                                    <> show actual
                                )

checkOrphanedDispatches :: FilePath -> IO [Check]
checkOrphanedDispatches dbPath = do
    dbOk <- doesFileExist dbPath
    cfgOk <- doesFileExist defaultConfigPath
    if not dbOk || not cfgOk
        then pure []
        else do
            mcfg <- either (const Nothing) Just <$> loadConfig defaultConfigPath
            case mcfg of
                Nothing -> pure []
                Just cfg -> do
                    let staleSec = dcHeartbeatStaleSeconds (cfgDispatch cfg)
                    now <- getCurrentTime
                    withDb dbPath $ \conn -> do
                        open <- RD.listOpenDispatches conn
                        orphans <- filterM (isOrphanedDispatch now staleSec) open
                        pure $
                            if null orphans
                                then [Check "dispatches" (OK "no orphaned dispatches")]
                                else map toWarn orphans
  where
    toWarn d =
        Check
            "dispatch"
            ( WARN $
                T.unpack (T.take 8 (dispatchId d))
                    <> ": pid dead or heartbeat stale."
                    <> " Run `icarium dispatch recover` to reconcile."
            )

{- | Every task/context row must have a non-empty body file on disk. This
catches abandoned add-then-Write intermediates and out-of-band deletions
(issue #13). Abandoned tasks are exempt — they're explicit dead ends.
-}
checkBodies :: FilePath -> IO [Check]
checkBodies dbPath = do
    dbOk <- doesFileExist dbPath
    if not dbOk
        then pure []
        else do
            (tasks, ctxs) <- withDb dbPath $ \conn -> do
                ts <- RT.listTasks conn [] False []
                cxs <- RCx.listContexts conn Nothing True []
                pure (ts, cxs)
            let live = filter ((/= Abandoned) . taskState) tasks
            taskFails <- mapM (bodyFailure "task" taskBodyPath . taskId) live
            ctxFails <- mapM (bodyFailure "ctx" ctxBodyPath . contextId) ctxs
            let fails = concat (taskFails ++ ctxFails)
            pure $
                if null fails
                    then [Check "bodies" (OK "every task/ctx body file present and non-empty")]
                    else fails
  where
    bodyFailure :: String -> (FilePath -> Text -> FilePath) -> Text -> IO [Check]
    bodyFailure kind pathFn nid = do
        let fp = pathFn (bodiesDir dbPath) nid
        exists <- doesFileExist fp
        problem <-
            if not exists
                then pure (Just "body file missing")
                else do
                    content <- TIO.readFile fp
                    pure $
                        if T.null (T.strip content)
                            then Just "body file empty"
                            else Nothing
        pure
            [ Check "body" (FAIL (kind <> " " <> T.unpack (T.take 10 nid) <> ": " <> msg <> " — Write markdown to $(icarium " <> kind <> " path " <> T.unpack (T.take 10 nid) <> ")"))
            | Just msg <- [problem]
            ]

isOrphanedDispatch :: UTCTime -> Int -> Dispatch -> IO Bool
isOrphanedDispatch now staleSec d = do
    alive <- maybe (pure False) pidAlive (dispatchPid d)
    let stale = heartbeatStale now staleSec (dispatchHeartbeat d)
    pure (not alive || stale)

printCheck :: Check -> IO ()
printCheck c = case checkResult c of
    OK msg -> putStrLn $ "  ok    " <> pad 10 (checkName c) <> "  " <> msg
    WARN msg -> putStrLn $ "  WARN  " <> pad 10 (checkName c) <> "  " <> msg
    FAIL msg -> putStrLn $ "  FAIL  " <> pad 10 (checkName c) <> "  " <> msg
  where
    pad n s = s <> replicate (max 0 (n - length s)) ' '
