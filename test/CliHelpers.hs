{- | Shared plumbing for the CLI integration specs: ways to invoke
@./bin/icarium@ as a subprocess, a throwaway DB, and the JSON decoding the
@--json@ assertions need.
-}
module CliHelpers (
    absBin,
    runIcarium,
    runIcariumIn,
    runIcariumEnvDb,
    runIcariumStdin,
    runIcariumBare,
    withTempDb,
    minimalIcariumToml,
    commonPrefix,
    jsonIds,
    decodeOut,
    expectObject,
    expectField,
) where

import Data.Aeson (Key, Object, Value (..), decode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import System.Directory (makeAbsolute)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed (byteStringInput, proc, readProcess, setEnv, setStdin, setWorkingDir)

-- Resolved once at load time so concurrent cwd changes in PostClaudeSpec
-- tests (which use withCurrentDirectory) don't corrupt makeAbsolute calls.
{-# NOINLINE absBin #-}
absBin :: FilePath
absBin = unsafePerformIO (makeAbsolute "./bin/icarium")

runIcarium :: FilePath -> [String] -> IO (ExitCode, String, String)
runIcarium db args = do
    (code, outBs, errBs) <- readProcess (proc absBin (["--db", db] ++ args))
    pure (code, BLC.unpack outBs, BLC.unpack errBs)

{- | Run icarium with a custom working directory so config loading picks
up a test-supplied icarium.toml. Both the binary and db paths are
resolved against the test's cwd before chdir, so they stay valid.
-}
runIcariumIn :: FilePath -> FilePath -> [String] -> IO (ExitCode, String, String)
runIcariumIn workdir db args = do
    absDb <- makeAbsolute db
    (code, outBs, errBs) <-
        readProcess $
            setWorkingDir workdir $
                proc absBin (["--db", absDb] ++ args)
    pure (code, BLC.unpack outBs, BLC.unpack errBs)

withTempDb :: (FilePath -> IO a) -> IO a
withTempDb k = withSystemTempDirectory "icarium-test" $ \dir ->
    k (dir <> "/icarium.db")

{- | Run icarium with no --db flag; ICARIUM_DB in the environment resolves
the db path instead. Replaces any inherited ICARIUM_DB so the value passed
here always wins.
-}
runIcariumEnvDb :: FilePath -> [String] -> IO (ExitCode, String, String)
runIcariumEnvDb db args = do
    parentEnv <- getEnvironment
    let env = ("ICARIUM_DB", db) : filter ((/= "ICARIUM_DB") . fst) parentEnv
    (code, outBs, errBs) <- readProcess (setEnv env (proc absBin args))
    pure (code, BLC.unpack outBs, BLC.unpack errBs)

-- | Run icarium with the given string as its entire stdin.
runIcariumStdin :: FilePath -> String -> [String] -> IO (ExitCode, String, String)
runIcariumStdin db input args = do
    (code, outBs, errBs) <-
        readProcess $
            setStdin (byteStringInput (BLC.pack input)) $
                proc absBin (["--db", db] ++ args)
    pure (code, BLC.unpack outBs, BLC.unpack errBs)

runIcariumBare :: [String] -> IO (ExitCode, String, String)
runIcariumBare args = do
    (code, outBs, errBs) <- readProcess (proc absBin args)
    pure (code, BLC.unpack outBs, BLC.unpack errBs)

minimalIcariumToml :: String
minimalIcariumToml =
    unlines
        [ "[project]"
        , "integration_branch = \"main\""
        , ""
        , "[commands]"
        , "build = \"true\""
        , "test  = \"true\""
        , ""
        , "[dispatch]"
        , "model  = \"claude-sonnet-4-6\""
        , "effort = \"high\""
        , "tools = []"
        , "allowed_tools = []"
        , "scratch_dir = \".icarium/scratch\""
        , "max_minutes_per_dispatch = 30"
        , "heartbeat_stale_seconds  = 300"
        , "log_retention_runs       = 25"
        , ""
        , "[categories]"
        , "domains     = [\"core\"]"
        , "disciplines = [\"development\"]"
        ]

{- | The longest prefix two ids share — a deterministic way to hand a
resolver something ambiguous, where a fixed @take n@ only sometimes
collides.
-}
commonPrefix :: String -> String -> String
commonPrefix a b = map fst (takeWhile (uncurry (==)) (zip a b))

-- | Decode stdout as a JSON array of objects and pull each element's id.
jsonIds :: String -> [String]
jsonIds out = case decodeOut out of
    Array vs -> map (expectField "id" . expectObject) (toList vs)
    _ -> error ("expected a JSON array, got: " <> out)

decodeOut :: String -> Value
decodeOut out = fromMaybe (error ("not valid JSON: " <> out)) (decode (BLC.pack out))

expectObject :: Value -> Object
expectObject (Object o) = o
expectObject v = error ("expected a JSON object, got: " <> show v)

expectField :: Key -> Object -> String
expectField k o = case KM.lookup k o of
    Just (String t) -> T.unpack t
    other -> error ("expected string field " <> show k <> ", got: " <> show other)
