{-# LANGUAGE ScopedTypeVariables #-}

module Icarium.Dispatch.Reviewer (
    ReviewResult (..),
    runReviewer,
    loadReviewerPrompt,
    defaultReviewerPrompt,
    parseReviewVerdictFromText,
) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, handle, try)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (Handle, hIsEOF, hPutStrLn, stderr)
import System.Posix.Types (CPid (..))
import System.Process.Typed (
    byteStringInput,
    createPipe,
    getPid,
    getStdout,
    proc,
    setCreateGroup,
    setEnv,
    setStdin,
    setStdout,
    setWorkingDir,
    waitExitCode,
    withProcessWait,
 )

import Icarium.Dispatch.Claude (
    killGroupGracefully,
    raceTimeout,
    timeoutSentinel,
    withLogHandle,
 )
import Icarium.Dispatch.LogResult (LogResult (..), readLogResult)
import Icarium.Types (ReviewVerdict (..))

data ReviewResult = ReviewResult
    { rrVerdict :: ReviewVerdict
    , rrFindings :: Text
    , rrLogPath :: FilePath
    }

defaultReviewerPrompt :: Text
defaultReviewerPrompt =
    "You are a code reviewer. You will be given a task description and a git diff.\n\
    \Review the diff against the task description and check:\n\
    \- The implementation addresses the task requirements\n\
    \- No obvious bugs, broken logic, or security issues\n\
    \- The changes appear complete (not partial or placeholder)\n\
    \\n\
    \Respond with ONLY a YAML block in this exact format:\n\
    \\n\
    \```yaml\n\
    \status: pass\n\
    \findings: []\n\
    \```\n\
    \\n\
    \Or with findings:\n\
    \\n\
    \```yaml\n\
    \status: warn\n\
    \findings:\n\
    \  - severity: warn\n\
    \    file: src/Foo.hs\n\
    \    message: \"Description of the concern\"\n\
    \```\n\
    \\n\
    \status values:\n\
    \  pass - implementation matches the task, no significant issues\n\
    \  warn - acceptable but has minor concerns; merge proceeds\n\
    \  fail - does not match the task, incomplete, or has serious issues\n\
    \\n\
    \Output ONLY the yaml block."

loadReviewerPrompt :: Maybe Text -> IO (Maybe Text)
loadReviewerPrompt Nothing = pure Nothing
loadReviewerPrompt (Just path) = do
    r <- try (TIO.readFile (T.unpack path)) :: IO (Either SomeException Text)
    case r of
        Left e -> do
            hPutStrLn stderr ("icarium: reviewer prompt_path read failed: " <> show e)
            pure Nothing
        Right t -> pure (Just t)

buildReviewerStdin :: Text -> Text -> Text -> Text -> Text
buildReviewerStdin sysPrompt taskTitle taskBody diffText =
    T.unlines
        [ sysPrompt
        , ""
        , "# Task: " <> taskTitle
        , ""
        , if T.null taskBody then "(no body)" else taskBody
        , ""
        , "# Diff"
        , ""
        , "```diff"
        , diffText
        , "```"
        ]

{- | Anchors verdict parsing to the last fenced @```yaml@ block in the text.
A @status:@ line found outside any fenced block, or in a bare @```@ block,
is ignored; fail-closed if no valid yaml block/status is found.
-}
parseReviewVerdictFromText :: Text -> ReviewVerdict
parseReviewVerdictFromText t =
    fromMaybe RVFail $ do
        block <- lastYamlBlock (T.lines t)
        line <- find (T.isPrefixOf "status:" . T.strip) block
        let val = T.strip (T.drop 7 (T.strip line))
        case val of
            "pass" -> Just RVPass
            "warn" -> Just RVWarn
            "fail" -> Just RVFail
            _ -> Nothing

-- | Extracts the lines of the last closed @```yaml@ fenced block, if any.
lastYamlBlock :: [Text] -> Maybe [Text]
lastYamlBlock = go Nothing Nothing
  where
    go :: Maybe (Bool, [Text]) -> Maybe [Text] -> [Text] -> Maybe [Text]
    go Nothing lastBlock [] = lastBlock
    go (Just _) lastBlock [] = lastBlock
    go Nothing lastBlock (line : rest)
        | "```" `T.isPrefixOf` T.strip line =
            let fenceArg = T.toLower (T.strip (T.drop 3 (T.strip line)))
             in go (Just (fenceArg == "yaml", [])) lastBlock rest
        | otherwise = go Nothing lastBlock rest
    go (Just (isYaml, acc)) lastBlock (line : rest)
        | "```" `T.isPrefixOf` T.strip line =
            let lastBlock' = if isYaml then Just (reverse acc) else lastBlock
             in go Nothing lastBlock' rest
        | otherwise = go (Just (isYaml, line : acc)) lastBlock rest

runReviewer ::
    -- | directory the reviewer runs in (its Read tool sees branch state)
    FilePath ->
    -- | model name
    Text ->
    -- | system prompt override (Nothing = use default)
    Maybe Text ->
    -- | task title
    Text ->
    -- | task body
    Text ->
    -- | git diff text
    Text ->
    -- | path to write reviewer JSONL log
    FilePath ->
    -- | wall-clock limit in minutes
    Int ->
    IO ReviewResult
runReviewer workDir model mSysPrompt taskTitle taskBody diffText reviewerLogPath maxMinutes = do
    let sysPrompt = fromMaybe defaultReviewerPrompt mSysPrompt
        stdinText = buildReviewerStdin sysPrompt taskTitle taskBody diffText
        stdinBytes = BL.fromStrict (TE.encodeUtf8 stdinText)
        args =
            [ "-p"
            , "--model"
            , T.unpack model
            , "--output-format"
            , "stream-json"
            , "--verbose"
            , "--tools"
            , "Read"
            , "--allowedTools"
            , "Read"
            , "--disable-slash-commands"
            , "--permission-mode"
            , "dontAsk"
            , "--strict-mcp-config"
            ]
    hPutStrLn stderr "[reviewer] running..."
    exit <- runReviewerProcess workDir stdinBytes args reviewerLogPath maxMinutes
    mLR <- readLogResult reviewerLogPath
    let responseText = case exit of
            ExitFailure 124 -> "reviewer timed out"
            ExitSuccess -> fromMaybe "" (mLR >>= lrResultText)
            _ -> fromMaybe "reviewer agent failed" (mLR >>= lrResultText)
        verdict = case exit of
            ExitSuccess -> parseReviewVerdictFromText responseText
            _ -> RVFail
    pure
        ReviewResult
            { rrVerdict = verdict
            , rrFindings = responseText
            , rrLogPath = reviewerLogPath
            }

runReviewerProcess :: FilePath -> BL.ByteString -> [String] -> FilePath -> Int -> IO ExitCode
runReviewerProcess workDir stdinBytes args logPath maxMinutes = do
    parentEnv <- getEnvironment
    let pcfg =
            setStdin (byteStringInput stdinBytes) $
                setStdout createPipe $
                    setEnv parentEnv $
                        setCreateGroup True $
                            setWorkingDir workDir $
                                proc "claude" args
        maxUsecs = maxMinutes * 60 * 1_000_000
    withLogHandle logPath $ \logH ->
        withProcessWait pcfg $ \p -> do
            mPid <- getPid p
            _ <- forkIO (drainToLog (getStdout p) logH)
            result <- raceTimeout maxUsecs (waitExitCode p)
            case result of
                Right exit -> pure exit
                Left () -> do
                    mapM_ (killGroupGracefully . CPid . fromIntegral) mPid
                    pure timeoutSentinel

drainToLog :: Handle -> Handle -> IO ()
drainToLog src logH =
    handle (\(_ :: SomeException) -> pure ()) loop
  where
    loop = do
        eof <- hIsEOF src
        if eof
            then pure ()
            else do
                line <- BC.hGetLine src
                BC.hPutStrLn logH line
                loop
