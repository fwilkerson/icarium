module Icarium.Commands.Doctor (Options, parser, run) where

import Control.Monad (filterM)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Options.Applicative
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode (..), exitWith)

import Icarium.Config (
    DispatchConfig (..),
    cfgDispatch,
    defaultConfigPath,
    loadConfig,
 )
import Icarium.Db (dbSchemaVersion, withDbReadOnly)
import Icarium.Heartbeat (heartbeatStale, pidAlive)
import Icarium.Repo.Dispatch qualified as RD
import Icarium.Schema (schemaVersion)
import Icarium.Types (Dispatch (..))

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
    let checks = basic ++ orphans
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
            v <- withDbReadOnly dbPath dbSchemaVersion
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
                    withDbReadOnly dbPath $ \conn -> do
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
