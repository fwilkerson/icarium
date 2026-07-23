module Icarium.Render.Dispatch (
    renderDispatch,
    DispatchRow (..),
    renderDispatchList,
    renderDispatchDuration,
    renderDispatchStats,
    renderRunSummary,
    renderRunOutcome,
    renderRecoveryNotes,
    renderRecovered,
    renderLanded,
    renderStillParked,
    renderMergeAttempt,
    renderMergeTally,
    fmtSecs,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime)
import Text.Printf (printf)

import Icarium.Dispatch.LogResult (LogResult (..), LogUsage (..), fmtMs)
import Icarium.Dispatch.Merge (MergeOutcome (..))
import Icarium.Dispatch.Outcome (DispatchResult (..))
import Icarium.Dispatch.Payload (WorkerPayload (..), workerStatusText)
import Icarium.Heartbeat (DispatchHealth (..))
import Icarium.Render.Internal
import Icarium.Repo.Dispatch (DispatchStats (..))
import Icarium.Text (tshow)
import Icarium.Time (parseDbTime)
import Icarium.Types

renderDispatch :: Dispatch -> Maybe Task -> [Context] -> Maybe Text -> Text
renderDispatch d mt ks mRetryId =
    T.unlines $
        [ field "id" (dispatchId d)
        , field "task_id" (dispatchTaskId d)
        , field "task_title" (maybe "(task missing)" taskTitle mt)
        , field "branch" (dispatchBranch d)
        , field "base_branch" (dispatchBaseBranch d)
        , field "base_sha" (dispatchBaseSha d)
        , field "pid" (maybe "" (T.pack . show) (dispatchPid d))
        , field "model" (dispatchModel d)
        , field "effort" (effortText (dispatchEffort d))
        , field "started_at" (dispatchStartedAt d)
        , field "heartbeat_at" (dispatchHeartbeat d)
        , field "ended_at" (fromMaybe "" (dispatchEndedAt d))
        , field "outcome" (maybe "open" dispatchOutcomeText (dispatchOutcome d))
        , field "review" (maybe "" reviewVerdictText (dispatchReviewVerdict d))
        , field "body_changed" (maybe "" (\b -> if b then "yes" else "no") (dispatchBodyChanged d))
        , field "merged" mergedField
        , field "merge_sha" (fromMaybe "" (dispatchMergeSha d))
        , field "last_commit" (fromMaybe "" (dispatchLastCommit d))
        , field "log_path" (fromMaybe "" (dispatchLogPath d))
        , field "reviewer_log" (fromMaybe "" (dispatchReviewerLogPath d))
        , field "notes" (fromMaybe "" (dispatchNotes d))
        ]
            ++ tokensLine
            ++ retryLine
            ++ [ ""
               , "Context added:"
               ]
            ++ contextLines
  where
    field k v = padr 14 (k <> ":") <> " " <> v
    mergedField = case (dispatchOutcome d, dispatchMergeSha d) of
        (Just OSuccess, Nothing) -> "no (parked; land with `icarium dispatch merge`)"
        (_, Just _) -> fromMaybe "yes" (dispatchMergedAt d)
        _ -> ""
    tokensLine = case (dispatchTokensIn d, dispatchTokensOut d, dispatchTokensCacheRead d) of
        (Just i, Just o, Just c) ->
            [ field
                "tokens"
                ( "in "
                    <> T.pack (show i)
                    <> " / out "
                    <> T.pack (show o)
                    <> " / cache_read "
                    <> T.pack (show c)
                )
            ]
        _ -> []
    retryLine = case (dispatchOutcome d, mRetryId) of
        (Just OFailure, Just rid) -> [field "retry_dispatch" rid]
        _ -> []
    contextLines = case ks of
        [] -> ["  (none)"]
        _ -> map (\k -> "  " <> T.take 10 (contextId k) <> "  " <> contextTitle k) ks

data DispatchRow = DispatchRow
    { drDispatch :: Dispatch
    , drTaskTitle :: Text
    , drCtxCount :: Int
    , drDuration :: Text
    }

renderDispatchList :: Bool -> [DispatchRow] -> Text
renderDispatchList _ [] = "(no dispatches)\n"
renderDispatchList utf8 rows = T.unlines $ map renderRow rows
  where
    titleWidth = min recommendedTitleMax (maxLen 5 (map (T.length . drTaskTitle) rows))
    durWidth = maxLen 2 (map (T.length . drDuration) rows)
    ctxWidth = maxLen 0 (map (T.length . fmtCtx . drCtxCount) rows)

    fmtCtx 0 = ""
    fmtCtx n = "[ctx:" <> T.pack (show n) <> "]"

    renderRow dr =
        let d = drDispatch dr
            didPart = "  " <> padr 10 (T.take 10 (dispatchId d))
            tidPart = padr 10 (T.take 10 (dispatchTaskId d))
            titPart = padr titleWidth (truncateTitle utf8 titleWidth (drTaskTitle dr))
            durPart = padr durWidth (drDuration dr)
            ctx = fmtCtx (drCtxCount dr)
            ctxPart = if ctxWidth == 0 then "" else "  " <> padr ctxWidth ctx
            badge = outcomeBadge d
         in didPart <> "   " <> tidPart <> "  " <> titPart <> "  " <> durPart <> ctxPart <> "  " <> badge

-- | Parked = succeeded but not yet landed on base.
outcomeBadge :: Dispatch -> Text
outcomeBadge d = case dispatchOutcome d of
    Nothing -> "[open]"
    Just OSuccess
        | Nothing <- dispatchMergeSha d -> "[parked]"
        | otherwise -> "[success]"
    Just OFailure -> "[failure]"
    Just OInterrupted -> "[interrupted]"

-- | Script-friendly spend/outcome summary for @dispatch stats@.
renderDispatchStats :: Maybe Text -> DispatchStats -> Text
renderDispatchStats mSince s =
    T.unlines
        [ field "since" (fromMaybe "(all)" mSince)
        , field "dispatches" (tshow (dsTotal s))
        , field "success" (tshow (dsSuccess s))
        , field "failure" (tshow (dsFailure s))
        , field "interrupted" (tshow (dsInterrupted s))
        , field "open" (tshow (dsOpen s))
        , field "tokens_in" (tshow (dsTokensIn s))
        , field "tokens_out" (tshow (dsTokensOut s))
        , field "tokens_cache_read" (tshow (dsTokensCacheRead s))
        , field "missing_tokens" (tshow (dsMissingTokens s))
        ]
  where
    field k v = padr 18 (k <> ":") <> " " <> v

renderDispatchDuration :: UTCTime -> Dispatch -> Text
renderDispatchDuration now d =
    case parseDbTime (dispatchStartedAt d) of
        Nothing -> ""
        Just start ->
            let (diff, isOpen) = case dispatchEndedAt d >>= parseDbTime of
                    Just end -> (diffUTCTime end start, False)
                    Nothing -> (diffUTCTime now start, True)
                secs = max 0 (round (toRational diff) :: Int)
                body = fmtSecs secs
             in if isOpen then body <> " (running)" else body

{- | The post-run summary block for @dispatch run@. The caller supplies the
log result and the changed-file list; the latter must be read before
auto-merge deletes the dispatch branch.
-}
renderRunSummary :: DispatchResult -> Maybe LogResult -> [Text] -> Text
renderRunSummary r mLog files =
    T.unlines $
        [ ""
        , field "dispatch" (fromMaybe "(dry-run)" (dresDispatchId r))
        , field "outcome" (dispatchOutcomeText (dresOutcome r))
        , field "branch" (dresBranch r)
        , field "notes" (dresNotes r)
        ]
            <> foldMap (\p -> [field "worker" (workerLine p)]) (dresPayload r)
            <> foldMap logLines mLog
            <> fileLines
  where
    field k v = padr 9 (k <> ":") <> " " <> v
    pad = T.replicate 10 " "

    workerLine p =
        T.intercalate "; " $
            [workerStatusText (wpStatus p) <> maybe "" (": " <>) (wpBlockReason p)]
                <> [ tshow n <> " for future agents"
                   | let n = length (wpForFutureAgents p)
                   , n > 0
                   ]

    logLines lr =
        [ field "turns" (maybe "-" tshow (lrNumTurns lr))
        , field
            "duration"
            ( maybe "-" fmtMs (lrDurationMs lr)
                <> maybe "" (\a -> " (api: " <> fmtMs a <> ")") (lrDurationApiMs lr)
            )
        , field "cost" (maybe "-" (T.pack . printf "$%.4f") (lrCostUsd lr))
        , field "tokens" (fmtTokens (lrUsage lr))
        ]

    fmtTokens Nothing = "-"
    fmtTokens (Just u) =
        "in "
            <> maybe "-" tshow (luInputTokens u)
            <> " / out "
            <> maybe "-" tshow (luOutputTokens u)
            <> " / cache "
            <> maybe "-" tshow (luCacheReads u)

    fileLines = case files of
        [] -> []
        _ ->
            let shown = take 10 files
                extra = length files - length shown
                items = shown <> [tshow extra <> " more" | extra > 0]
             in [field "files" (T.intercalate ("\n" <> pad) items)]

-- | One-line outcome for the drain loop's progress log.
renderRunOutcome :: DispatchResult -> Text
renderRunOutcome r =
    dispatchOutcomeText (dresOutcome r) <> " \x2014 " <> dresNotes r

{- | Structured recovery notes for an interrupted dispatch. Stored, not just
printed: they land in @dispatches.notes@ and the task's @block_reason@.
'Nothing' for the worktree means it did not survive the crash, so there is
nothing to say about uncommitted state.
-}
renderRecoveryNotes :: DispatchHealth -> Maybe Bool -> Text -> Text
renderRecoveryNotes health mDirty lastCommit =
    T.intercalate "; " $
        [ "interrupted"
        , "alive=" <> boolText (dhAlive health)
        , "stale=" <> boolText (dhStale health)
        ]
            <> foldMap (\dirty -> ["uncommitted=" <> boolText dirty, "worktree=removed"]) mDirty
            <> ["last_commit=" <> lastCommit]
  where
    boolText b = if b then "yes" else "no"

renderRecovered :: Dispatch -> Text -> Text
renderRecovered d notes =
    "dispatch:"
        <> dispatchId d
        <> "  task:"
        <> dispatchTaskId d
        <> "  branch:"
        <> dispatchBranch d
        <> "  "
        <> notes

renderLanded :: Dispatch -> Text -> Text
renderLanded d sha =
    "merged "
        <> shortId d
        <> ": "
        <> dispatchBaseBranch d
        <> " -> "
        <> T.take 10 sha

renderStillParked :: Dispatch -> Text -> Text
renderStillParked d note =
    "dispatch "
        <> shortId d
        <> " parked: "
        <> note
        <> "; fix and run `icarium dispatch merge "
        <> shortId d
        <> "`"

-- | One line per attempt in @dispatch merge --all@.
renderMergeAttempt :: Dispatch -> MergeOutcome -> Text
renderMergeAttempt d = \case
    MergeLanded sha -> renderLanded d sha
    MergeBlocked _ note -> "blocked " <> shortId d <> ": " <> note
    MergeStopped note -> "stopped " <> shortId d <> ": " <> note

renderMergeTally :: Int -> Int -> Int -> Int -> Text
renderMergeTally total landed blocked unattempted =
    T.intercalate "; " $
        [tshow landed <> " of " <> tshow total <> " landed"]
            <> [tshow blocked <> " still parked" | blocked > 0]
            <> [tshow unattempted <> " not attempted" | unattempted > 0]

shortId :: Dispatch -> Text
shortId = T.take 10 . dispatchId

fmtSecs :: Int -> Text
fmtSecs s
    | s < 60 = T.pack (show s) <> "s"
    | s < 3600 = T.pack (show (s `div` 60)) <> "m"
    | otherwise =
        T.pack (show (s `div` 3600))
            <> "h "
            <> T.pack (show ((s `mod` 3600) `div` 60))
            <> "m"
