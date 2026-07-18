module Icarium.Render (
    renderTaskHuman,
    renderTaskPrompt,
    TaskRow (..),
    renderTaskList,
    mkBar,
    renderContext,
    ContextRow (..),
    renderContextList,
    CurationQueueRow (..),
    renderCurationQueue,
    formatLinkedCount,
    renderEdgeLine,
    renderCategory,
    recommendedTitleMax,
    renderDispatch,
    DispatchRow (..),
    renderDispatchList,
    renderDispatchStats,
    fmtSecs,
    SearchHitRow (..),
    renderSearchList,
) where

import Data.List (sortBy)
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T

import Icarium.Repo.Dispatch (DispatchStats (..))
import Icarium.Repo.Search (SearchHit (..))
import Icarium.Types

-- =============================================================
-- Task list row (carries per-task data for grouped list view)
-- =============================================================

data TaskRow = TaskRow
    { trTask :: Task
    , trCats :: [Category]
    , trDeps :: Int -- count of depends_on edges from this task
    , trRefs :: Int -- count of references edges from this task
    }

-- =============================================================
-- Task rendering
-- =============================================================

{- | Human-facing task view. Shows metadata + body file path + linked nodes.

@utf8@: True → Unicode tree glyphs; False → ASCII fallback.
@bodyPath@: path to the body file on disk (agents @Read@ this to see content).
@retiredIds@: refs among these ids get a [retired] marker in the links tree.
-}
renderTaskHuman :: Bool -> Task -> Text -> [Context] -> [Task] -> [Category] -> [Text] -> Text
renderTaskHuman utf8 t bodyPath refs deps cats retiredIds =
    T.unlines $
        [ "id:        " <> taskId t
        , "title:     " <> taskTitle t
        , "state:     " <> taskStateText (taskState t)
        , priorityLine t
        ]
            <> blockReasonLines t
            <> noCommitLine t
            <> claimLines t
            <> [ "created:   " <> taskCreatedAt t
               , "updated:   " <> taskUpdatedAt t
               , "body:      " <> bodyPath
               ]
            <> categoriesBlock cats
            <> [""]
            <> linksSection utf8 t deps refs retiredIds

priorityLine :: Task -> Text
priorityLine t = case taskPriority t of
    Just p -> "priority:  " <> T.pack (show p)
    Nothing -> "priority:  -"

blockReasonLines :: Task -> [Text]
blockReasonLines t = case taskBlockReason t of
    Just r | not (T.null r) -> ["block_reason: " <> r]
    _ -> []

noCommitLine :: Task -> [Text]
noCommitLine t
    | taskNoCommit t = ["no-commit:   yes"]
    | otherwise = []

-- | Set by `task claim`; absent for tasks nobody claimed.
claimLines :: Task -> [Text]
claimLines t =
    ["owner:     " <> w | w <- maybeToList (taskClaimedBy t)]
        <> ["claimed:   " <> a | a <- maybeToList (taskClaimedAt t)]

{- | Combined links tree replacing the old flat dep/ref sections.

All depends-on edges first (sorted by target id ASC), then references.
Last edge gets └─ (or \- in ASCII mode); others get ├─ (or +-).
-}
linksSection :: Bool -> Task -> [Task] -> [Context] -> [Text] -> [Text]
linksSection _ _ [] [] _ = ["## Links", "", "(none)", ""]
linksSection utf8 t deps refs retiredIds = ["## Links", "", rootLine] <> edgeLines <> [""]
  where
    rootLine = T.take 10 (taskId t) <> "  " <> taskTitle t

    sortedDeps = sortBy (\a b -> compare (taskId a) (taskId b)) deps
    sortedRefs = sortBy (\a b -> compare (contextId a) (contextId b)) refs

    allEdges :: [Either Task Context]
    allEdges = map Left sortedDeps <> map Right sortedRefs
    n = length allEdges

    kindWidth = max (T.length "depends-on") (T.length "references")

    branchG = if utf8 then "├─" else "+-"
    lastG = if utf8 then "└─" else "\\-"

    mkEdge i e =
        let g = if i == n - 1 then lastG else branchG
            kindStr = either (const "depends-on") (const "references") e
            idStr = either (T.take 10 . taskId) (T.take 10 . contextId) e
            titStr = either taskTitle contextTitle e
            suffix = case e of
                Left dep -> "  [" <> taskStateText (taskState dep) <> "]"
                Right ref -> if contextId ref `elem` retiredIds then "  [retired]" else ""
         in g <> " " <> padr kindWidth kindStr <> "  " <> idStr <> "  " <> titStr <> suffix

    edgeLines = zipWith mkEdge [0 ..] allEdges

{- | The shared task-content block: @task show --prompt@ prints it verbatim,
and the dispatch prompt builds on it (appending the working agreement —
'Icarium.Dispatch.Agreement' — which is deliberately absent here so
interactive builders never inherit headless lane rules).
Callers must pass a Task whose body reflects the on-disk body file
(dispatch: 'Icarium.Bodies.Sweep.refreshTaskBody'; @task show --prompt@:
'Icarium.Bodies.readBody' overlay).
@refs@ = explicit references (always rendered); @catMatched@ = auto-pulled
by category (rendered under a separate hedged section, omitted if empty).
-}
renderTaskPrompt :: Task -> [Context] -> [Context] -> [Task] -> Text
renderTaskPrompt t refs catMatched deps =
    T.unlines $
        [ "# Task " <> taskId t
        , ""
        , "**" <> taskTitle t <> "**"
        , ""
        , if T.null (taskBody t) then "(no body)" else taskBody t
        , ""
        ]
            <> promptDeps deps
            <> promptRefs refs
            <> promptRelated catMatched

promptDeps :: [Task] -> [Text]
promptDeps [] = []
promptDeps ds =
    "## Completed dependencies"
        : ""
        : map (\d -> "- " <> taskId d <> "  " <> taskTitle d) ds
        ++ [""]

promptRefs :: [Context] -> [Text]
promptRefs [] = []
promptRefs ks =
    "## Referenced context"
        : ""
        : concatMap
            ( \k ->
                [ "### " <> contextTitle k <> " (" <> contextId k <> ")"
                , ""
                , contextBody k
                , ""
                ]
            )
            ks

promptRelated :: [Context] -> [Text]
promptRelated [] = []
promptRelated ks =
    [ "## Related context"
    , ""
    , "These entries share categories with this task. They may not all apply directly — use judgment."
    , ""
    ]
        <> concatMap
            ( \k ->
                [ "### " <> contextTitle k <> " (" <> contextId k <> ")"
                , ""
                , contextBody k
                , ""
                ]
            )
            ks

{- | Recommended maximum title length (matches git commit subject convention).
Titles exceeding this are truncated with an ellipsis in list views.
-}
recommendedTitleMax :: Int
recommendedTitleMax = 72

-- =============================================================
-- Flat task list rendering
-- =============================================================

{- | Render a flat, priority-sorted task list with state badges.

@useUnicode@: True → Unicode ellipsis; False → ASCII fallback.
Rows are sorted priority DESC, ULID DESC; null priority sorts last.
Done and abandoned are hidden by default — callers filter before passing.
-}
renderTaskList :: Bool -> [TaskRow] -> Text
renderTaskList _ [] = "(no tasks)\n"
renderTaskList useUnicode rows = T.unlines $ concatMap renderRow sorted
  where
    sorted = sortBy cmpRow rows

    cmpRow a b =
        let pa = taskPriority (trTask a)
            pb = taskPriority (trTask b)
         in case (pa, pb) of
                (Nothing, Nothing) -> compareUlid b a
                (Just _, Nothing) -> LT
                (Nothing, Just _) -> GT
                (Just x, Just y) -> case compare y x of
                    EQ -> compareUlid b a
                    o -> o
    compareUlid r1 r2 = compare (taskId (trTask r1)) (taskId (trTask r2))

    titleWidth = min recommendedTitleMax (maxLen 5 (map (T.length . taskTitle . trTask) rows))
    catWidth = maxLen 3 (map (T.length . formatCats . trCats) rows)

    renderRow row =
        let t = trTask row
            idPart = "  " <> padr 10 (T.take 10 (taskId t))
            titPart = padr titleWidth (truncateTitle useUnicode titleWidth (taskTitle t))
            barPart = mkBar (taskPriority t)
            catPart = padr catWidth (formatCats (trCats row))
            ec = formatEdgeCounts (trDeps row) (trRefs row)
            edgePart = if T.null ec then "" else "  " <> ec
            badge = "  [" <> stateBadgeText (taskState t) <> "]"
            mainLine = idPart <> "  " <> titPart <> "  " <> barPart <> "  " <> catPart <> edgePart <> badge
            hangLine = case (taskState t, taskBlockReason t) of
                (Blocked, Just r)
                    | not (T.null r) ->
                        [T.replicate 14 " " <> truncateReason r]
                _ -> []
         in mainLine : hangLine

    truncateReason r
        | T.length r <= 60 = r
        | otherwise = T.take 57 r <> "..."

stateBadgeText :: TaskState -> Text
stateBadgeText InProgress = "in-progress"
stateBadgeText s = taskStateText s

{- | 5-cell Unicode priority bar. ■ filled, ◧ half-filled, □ empty,
space-separated. Represents the 0–10 priority range in half-square steps.
-}
mkBar :: Maybe Int -> Text
mkBar mp =
    let p = max 0 (min 10 (fromMaybe 0 mp))
        full = p `div` 2
        half = p `mod` 2
        empty = 5 - full - half
        cells = replicate full "■" ++ replicate half "◧" ++ replicate empty "□"
     in T.intercalate " " cells

{- | Truncate a title to fit within @width@ characters, appending an ellipsis
when truncation occurs. UTF-8 mode uses the single-char @…@; ASCII uses @...@.
-}
truncateTitle :: Bool -> Int -> Text -> Text
truncateTitle utf8 width title
    | T.length title <= width = title
    | utf8 = T.take (width - 1) title <> "…"
    | otherwise = T.take (width - 3) title <> "..."

-- | Format categories as [dom/disc], [-/disc], [dom/-], or [-].
formatCats :: [Category] -> Text
formatCats cats =
    let dom = listToMaybe [categoryName c | c <- cats, categoryAxis c == Domain]
        disc = listToMaybe [categoryName c | c <- cats, categoryAxis c == Discipline]
     in case (dom, disc) of
            (Nothing, Nothing) -> "[-]"
            (Just d, Nothing) -> "[" <> d <> "/-]"
            (Nothing, Just di) -> "[-/" <> di <> "]"
            (Just d, Just di) -> "[" <> d <> "/" <> di <> "]"

-- | Edge count annotation. Empty string when both counts are 0.
formatEdgeCounts :: Int -> Int -> Text
formatEdgeCounts 0 0 = ""
formatEdgeCounts d r =
    "[" <> T.intercalate " " parts <> "]"
  where
    parts =
        ["deps:" <> T.pack (show d) | d > 0]
            <> ["refs:" <> T.pack (show r) | r > 0]

-- =============================================================
-- Context rendering
-- =============================================================

{- | @mEvent@: the entry's latest curation event; drives the derived
status line (current/retired) and the curated line.
-}
renderContext :: Context -> [Category] -> Text -> Maybe CurationEvent -> Text
renderContext k cats bodyPath mEvent =
    T.unlines $
        [ "id:       " <> contextId k
        , "title:    " <> contextTitle k
        , "status:   " <> (if isRetired mEvent then "retired" else "current")
        ]
            <> curatedLine
            <> [ "created:  " <> contextCreatedAt k
               , "updated:  " <> contextUpdatedAt k
               , "body:     " <> bodyPath
               ]
            <> categoriesBlock cats
  where
    curatedLine = case mEvent of
        Nothing -> []
        Just e ->
            [ "curated:  "
                <> dispositionText (curationDisposition e)
                <> "  "
                <> curationCreatedAt e
                <> maybe "" ("  artifact: " <>) (curationArtifact e)
            ]

-- | Derived visibility: current = never curated or latest 'keep'.
isRetired :: Maybe CurationEvent -> Bool
isRetired = maybe False (dispositionRetires . curationDisposition)

data ContextRow = ContextRow
    { crContext :: Context
    , crCats :: [Category]
    , crLinked :: Int
    , crRetired :: Bool
    }

-- | Format a linked-count badge. Empty string when count is 0.
formatLinkedCount :: Int -> Text
formatLinkedCount 0 = ""
formatLinkedCount n = "[linked:" <> T.pack (show n) <> "]"

renderContextList :: Bool -> [ContextRow] -> Text
renderContextList _ [] = "(no context)\n"
renderContextList utf8 rows = T.unlines $ map row rows
  where
    titleWidth = min recommendedTitleMax (maxLen recommendedTitleMax (map (T.length . contextTitle . crContext) rows))
    catWidth = maxLen 3 (map (T.length . formatCats . crCats) rows)

    row cr =
        let k = crContext cr
            idPart = "  " <> T.take 10 (contextId k)
            titPart = padr titleWidth (truncateTitle utf8 titleWidth (contextTitle k))
            catPart = padr catWidth (formatCats (crCats cr))
            linked = formatLinkedCount (crLinked cr)
            linkedPart = if T.null linked then "" else "  " <> linked
            retiredPart = if crRetired cr then "  [retired]" else ""
         in idPart <> "  " <> titPart <> "  " <> catPart <> linkedPart <> retiredPart

-- =============================================================
-- Curation queue rendering
-- =============================================================

data CurationQueueRow = CurationQueueRow
    { cqContext :: Context
    , cqCats :: [Category]
    , cqLastEvent :: Maybe CurationEvent -- Nothing = never curated
    }

renderCurationQueue :: Bool -> [CurationQueueRow] -> Text
renderCurationQueue _ [] = "(nothing to curate)\n"
renderCurationQueue utf8 rows = T.unlines $ map row rows
  where
    titleWidth = min recommendedTitleMax (maxLen recommendedTitleMax (map (T.length . contextTitle . cqContext) rows))
    catWidth = maxLen 3 (map (T.length . formatCats . cqCats) rows)

    row cq =
        let k = cqContext cq
            idPart = "  " <> T.take 10 (contextId k)
            titPart = padr titleWidth (truncateTitle utf8 titleWidth (contextTitle k))
            catPart = padr catWidth (formatCats (cqCats cq))
            lastPart = case cqLastEvent cq of
                Nothing -> "never curated"
                Just e -> dispositionText (curationDisposition e) <> " " <> curationCreatedAt e
         in idPart <> "  " <> titPart <> "  " <> catPart <> "  " <> lastPart

renderEdgeLine :: Edge -> Text
renderEdgeLine e =
    T.take 10 (edgeId e)
        <> "  "
        <> edgeKindDisplay (edgeKind e)
        <> "  "
        <> nodeKindText (edgeSrcKind e)
        <> ":"
        <> T.take 10 (edgeSrcId e)
        <> "  ->  "
        <> nodeKindText (edgeDstKind e)
        <> ":"
        <> T.take 10 (edgeDstId e)

renderCategory :: Category -> Text
renderCategory c =
    categoryAxisText (categoryAxis c)
        <> "  "
        <> categoryName c
        <> "  ("
        <> categoryId c
        <> ")"

categoriesBlock :: [Category] -> [Text]
categoriesBlock [] = []
categoriesBlock cats =
    "Categories:"
        : concatMap axisLine [Domain, Discipline]
  where
    axisLine axis =
        let names = [categoryName c | c <- cats, categoryAxis c == axis]
         in [ "  " <> padr 12 (categoryAxisText axis <> ":") <> T.intercalate ", " names
            | not (null names)
            ]

-- =============================================================
-- Dispatch rendering
-- =============================================================

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
    tshow = T.pack . show

fmtSecs :: Int -> Text
fmtSecs s
    | s < 60 = T.pack (show s) <> "s"
    | s < 3600 = T.pack (show (s `div` 60)) <> "m"
    | otherwise =
        T.pack (show (s `div` 3600))
            <> "h "
            <> T.pack (show ((s `mod` 3600) `div` 60))
            <> "m"

-- =============================================================
-- Search rendering
-- =============================================================

data SearchHitRow = SearchHitRow
    { shrHit :: SearchHit
    , shrCats :: [Category]
    }

{- | Render search results. Each hit shows kind letter, id, title, cats,
status badge; body-match hits get an indented snippet line underneath.
When @total > length rows@ a footer shows the full match count.

@useUnicode@: Unicode ellipsis in truncated titles.
@isTty@: ANSI bold for match highlight; otherwise @**...**@.
@noSnippet@: suppress the snippet line entirely.
@q@: the original query string (used for snippet highlight).
@total@: total matches before the limit was applied.
-}
renderSearchList :: Bool -> Bool -> Bool -> Text -> Int -> [SearchHitRow] -> Text
renderSearchList _ _ _ _ _ [] = "(no matches)\n"
renderSearchList useUnicode isTty noSnippet q total rows =
    T.unlines $ concatMap renderRow rows ++ footerLines
  where
    shown = length rows
    footerLines
        | total > shown =
            [ "  ... showing "
                <> T.pack (show shown)
                <> " of "
                <> T.pack (show total)
                <> " matches (use --limit to see more)"
            ]
        | otherwise = []
    titleWidth = min recommendedTitleMax (maxLen 5 (map (T.length . hitTitle . shrHit) rows))
    catWidth = maxLen 3 (map (T.length . formatCats . shrCats) rows)

    snippetIndent = T.replicate 17 " "

    renderRow sr =
        let h = shrHit sr
            kindPart = "  " <> kindLetter (hitKind h) <> "  "
            idPart = padr 10 (T.take 10 (hitId h)) <> "  "
            titPart = padr titleWidth (truncateTitle useUnicode titleWidth (hitTitle h))
            catPart = padr catWidth (formatCats (shrCats sr))
            matchSrc = matchSourceLabel h
            badge = searchBadge h
            badgePart = if T.null badge then "" else "  " <> badge
            mainLine = kindPart <> idPart <> titPart <> "  " <> catPart <> "  " <> matchSrc <> badgePart
            snippetLine
                | noSnippet = []
                | hitTitleMatch h = []
                | otherwise =
                    let snip = extractSnippet isTty q (hitBody h)
                     in [snippetIndent <> snip | not (T.null snip)]
         in mainLine : snippetLine

kindLetter :: NodeKind -> Text
kindLetter TaskNode = "T"
kindLetter ContextNode = "C"

matchSourceLabel :: SearchHit -> Text
matchSourceLabel h
    | hitTitleMatch h && hitBodyMatch h = "[t+b]"
    | hitTitleMatch h = "[t]"
    | hitBodyMatch h = "[b]"
    | otherwise = "[?]"

searchBadge :: SearchHit -> Text
searchBadge h = case hitKind h of
    TaskNode -> case hitState h of
        Just s -> "[" <> stateBadgeText s <> "]"
        Nothing -> ""
    ContextNode -> if hitRetired h then "[retired]" else ""

extractSnippet :: Bool -> Text -> Text -> Text
extractSnippet _ _ "" = ""
extractSnippet isTty q body =
    let flat = T.unwords (T.words body)
        qLower = T.toLower q
        flatLower = T.toLower flat
        (before, rest) = T.breakOn qLower flatLower
     in if T.null rest
            then ""
            else
                let pos = T.length before
                    qLen = T.length q
                    half = 40 :: Int
                    start = max 0 (pos - half)
                    end = min (T.length flat) (pos + qLen + half)
                    winLen = end - start
                    snippet = T.take winLen (T.drop start flat)
                    prefix = if start > 0 then "..." else ""
                    suffix = if end < T.length flat then "..." else ""
                    inSnippet = pos - start
                    (preMatch, afterSnippet) = T.splitAt inSnippet snippet
                    matchText = T.take qLen afterSnippet
                    postMatch = T.drop qLen afterSnippet
                 in prefix <> preMatch <> bold isTty matchText <> postMatch <> suffix

bold :: Bool -> Text -> Text
bold True t = "\ESC[1m" <> t <> "\ESC[0m"
bold False t = "**" <> t <> "**"

-- =============================================================
-- Utilities
-- =============================================================

maxLen :: Int -> [Int] -> Int
maxLen def [] = def
maxLen _ xs = maximum xs

padr :: Int -> Text -> Text
padr n s = s <> T.replicate (max 0 (n - T.length s)) " "
