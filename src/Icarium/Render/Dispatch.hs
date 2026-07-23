module Icarium.Render.Dispatch (
    renderDispatch,
    DispatchRow (..),
    renderDispatchList,
    renderDispatchStats,
    fmtSecs,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Icarium.Render.Internal
import Icarium.Repo.Dispatch (DispatchStats (..))
import Icarium.Text (tshow)
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

fmtSecs :: Int -> Text
fmtSecs s
    | s < 60 = T.pack (show s) <> "s"
    | s < 3600 = T.pack (show (s `div` 60)) <> "m"
    | otherwise =
        T.pack (show (s `div` 3600))
            <> "h "
            <> T.pack (show ((s `mod` 3600) `div` 60))
            <> "m"
