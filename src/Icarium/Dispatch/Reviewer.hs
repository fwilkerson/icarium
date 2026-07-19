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

{- | The two-axis brief (ADR 0004), adapted from the @code-review@ skill's
Standards and Spec sub-agent prompts. The harness is one Read-only agent, so
the axes are two passes in one context rather than parallel sub-agents; each
finding tags its axis so retry prompts stay legible.
-}
defaultReviewerPrompt :: Text
defaultReviewerPrompt =
    "You are a code reviewer. You will be given a task description and a git diff.\n\
    \You have the Read tool: the working tree is the branch under review, so read\n\
    \any file you need for context, including the repo's own standards docs.\n\
    \\n\
    \Review the diff along TWO INDEPENDENT AXES. A change can pass one and fail\n\
    \the other; do not let one axis mask the other, and do not merge or rerank\n\
    \findings across them.\n\
    \\n\
    \# Axis 1: spec\n\
    \\n\
    \Does the diff faithfully implement the task body? Report:\n\
    \- requirements the task asked for that are missing or only partial\n\
    \- behaviour in the diff that was not asked for (scope creep)\n\
    \- requirements that look implemented but where the implementation is wrong\n\
    \Quote the task line each finding is about.\n\
    \\n\
    \# Axis 2: standards\n\
    \\n\
    \Does the code follow this repo's documented standards? Read what the repo\n\
    \documents (CLAUDE.md, CONTRIBUTING.md, docs/) and cite the file and rule for\n\
    \each breach. Skip anything tooling already enforces (formatter, linter,\n\
    \compiler, type system) -- the gates run separately.\n\
    \\n\
    \On top of the repo's docs, always carry this smell baseline (Fowler,\n\
    \_Refactoring_ ch.3). A documented repo standard OVERRIDES the baseline: where\n\
    \the repo endorses something the baseline would flag, suppress the smell.\n\
    \Baseline smells are ALWAYS judgement calls -- label them as such (\"possible\n\
    \Feature Envy\"), never as hard violations, and quote the hunk:\n\
    \\n\
    \- Mysterious Name -- a name that doesn't reveal what it does or holds.\n\
    \- Duplicated Code -- the same logic shape in more than one hunk or file.\n\
    \- Feature Envy -- a function reaching into another type's data more than its own.\n\
    \- Data Clumps -- the same few fields or params always travelling together.\n\
    \- Primitive Obsession -- a primitive or string standing in for a domain concept.\n\
    \- Repeated Switches -- the same case-cascade on the same type recurring.\n\
    \- Shotgun Surgery -- one logical change forcing scattered edits across many files.\n\
    \- Divergent Change -- one module edited for several unrelated reasons.\n\
    \- Speculative Generality -- abstraction or hooks for needs the task doesn't have.\n\
    \- Message Chains -- long navigation the caller shouldn't depend on.\n\
    \- Middle Man -- a layer that mostly just delegates onward.\n\
    \- Refused Bequest -- an implementer ignoring most of what it inherits.\n\
    \\n\
    \# Reporting bar\n\
    \\n\
    \Report every issue you find, including ones you are uncertain about or\n\
    \consider minor -- attach a severity rather than withholding. The status\n\
    \verdict is the filter, not the findings list: minor concerns belong in the\n\
    \findings under warn, not omitted.\n\
    \\n\
    \# Output\n\
    \\n\
    \Respond with ONLY a YAML block in this exact format:\n\
    \\n\
    \```yaml\n\
    \status: pass\n\
    \findings: []\n\
    \```\n\
    \\n\
    \Or with findings, each tagged with the axis it came from:\n\
    \\n\
    \```yaml\n\
    \status: warn\n\
    \findings:\n\
    \  - axis: spec\n\
    \    severity: fail\n\
    \    file: src/Foo.hs\n\
    \    message: \"Task asks for X; the diff never implements it\"\n\
    \  - axis: standards\n\
    \    severity: warn\n\
    \    file: src/Bar.hs\n\
    \    message: \"possible Duplicated Code: the parse loop repeats parseFoo\"\n\
    \```\n\
    \\n\
    \axis values: spec | standards\n\
    \\n\
    \status is the verdict over BOTH axes -- the worst outcome on either wins:\n\
    \  pass - faithful to the task and consistent with repo standards\n\
    \  warn - acceptable but has minor concerns; merge proceeds\n\
    \  fail - misses/contradicts the task, is incomplete, or breaks a documented\n\
    \         standard in a way that must be fixed before merge\n\
    \A baseline smell alone is a warn, not a fail; it is a judgement call.\n\
    \\n\
    \Output ONLY the yaml block."

{- | Load the reviewer system prompt override. Fails closed: an unreadable
@prompt_path@ is an error naming the path, not a silent fallback to
'defaultReviewerPrompt'. Must be called before the worker starts, so a
broken config never lets a weakened reviewer gate the merge.
-}
loadReviewerPrompt :: Maybe Text -> IO (Either Text (Maybe Text))
loadReviewerPrompt Nothing = pure (Right Nothing)
loadReviewerPrompt (Just path) = do
    r <- try (TIO.readFile (T.unpack path)) :: IO (Either SomeException Text)
    case r of
        Left e -> pure (Left ("reviewer prompt_path unreadable: " <> path <> ": " <> T.pack (show e)))
        Right t -> pure (Right (Just t))

{- | @bodyReport@ is the body-change tamper report
('Icarium.Dispatch.BodyDiff.renderBodyReport'); it precedes the task
body so the reviewer reads the provenance of the text before the text.
-}
buildReviewerStdin :: Text -> Text -> Text -> Text -> Text -> Text
buildReviewerStdin sysPrompt bodyReport taskTitle taskBody diffText =
    T.unlines
        [ sysPrompt
        , ""
        , "# Task body change report"
        , ""
        , bodyReport
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
    -- | body-change tamper report
    Text ->
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
runReviewer workDir model mSysPrompt bodyReport taskTitle taskBody diffText reviewerLogPath maxMinutes = do
    let sysPrompt = fromMaybe defaultReviewerPrompt mSysPrompt
        stdinText = buildReviewerStdin sysPrompt bodyReport taskTitle taskBody diffText
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
