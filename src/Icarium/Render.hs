module Icarium.Render
    ( renderTaskHuman
    , renderTaskPrompt
    , TaskRow (..)
    , renderTaskList
    , renderKnowledge
    , renderKnowledgeList
    , renderEdgeLine
    , renderCategory
    , recommendedTitleMax
    ) where

import           Data.List     (sortBy)
import           Data.Maybe    (fromMaybe, listToMaybe)
import           Data.Text     (Text)
import qualified Data.Text     as T

import           Icarium.Types

-- =============================================================
-- Task list row (carries per-task data for grouped list view)
-- =============================================================

data TaskRow = TaskRow
    { trTask :: Task
    , trCats :: [Category]
    , trDeps :: Int   -- count of depends_on edges from this task
    , trRefs :: Int   -- count of references edges from this task
    }

-- =============================================================
-- Task rendering
-- =============================================================

-- | Human-facing task view. Shows metadata + body + linked nodes.
renderTaskHuman :: Task -> [Knowledge] -> [Task] -> [Category] -> Text
renderTaskHuman t refs deps cats = T.unlines $
    [ "id:        " <> taskId t
    , "title:     " <> taskTitle t
    , "state:     " <> taskStateText (taskState t)
    , priorityLine t
    ]
    <> blockReasonLines t
    <> [ "created:   " <> taskCreatedAt t
       , "updated:   " <> taskUpdatedAt t
       ]
    <> categoriesBlock cats
    <> [ ""
       , "## Body"
       , ""
       , if T.null (taskBody t) then "(no body)" else taskBody t
       , ""
       ]
    <> depSection deps
    <> refSection refs

priorityLine :: Task -> Text
priorityLine t = case taskPriority t of
    Just p  -> "priority:  " <> T.pack (show p)
    Nothing -> "priority:  -"

blockReasonLines :: Task -> [Text]
blockReasonLines t = case taskBlockReason t of
    Just r  | not (T.null r) -> ["block_reason: " <> r]
    _                        -> []

depSection :: [Task] -> [Text]
depSection []   = ["## Dependencies", "", "(none)", ""]
depSection deps = ["## Dependencies", ""]
    <> map (\d -> "- [" <> taskStateText (taskState d) <> "] "
                       <> T.take 10 (taskId d) <> "  " <> taskTitle d) deps
    <> [""]

refSection :: [Knowledge] -> [Text]
refSection []   = ["## Referenced knowledge", "", "(none)", ""]
refSection refs = ["## Referenced knowledge", ""]
    <> concatMap (\k ->
        [ "### " <> T.take 10 (knowledgeId k) <> "  " <> knowledgeTitle k
            <> if knowledgeStale k then "  [STALE]" else ""
        , ""
        , knowledgeBody k
        , ""
        ]) refs

-- | The exact prompt the dispatcher will send to the headless agent.
-- Sharing this with @task show --prompt@ keeps the two in lockstep.
-- @refs@ = explicit references (always rendered); @catMatched@ = auto-pulled
-- by category (rendered under a separate hedged section, omitted if empty).
renderTaskPrompt :: Task -> [Knowledge] -> [Knowledge] -> [Task] -> Text
renderTaskPrompt t refs catMatched deps = T.unlines $
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
    <> workingAgreement t

promptDeps :: [Task] -> [Text]
promptDeps [] = []
promptDeps ds =
    "## Completed dependencies"
    : ""
    : map (\d -> "- " <> taskId d <> "  " <> taskTitle d) ds
    ++ [""]

promptRefs :: [Knowledge] -> [Text]
promptRefs [] = []
promptRefs ks =
    "## Referenced knowledge"
    : ""
    : concatMap (\k ->
        [ "### " <> knowledgeTitle k <> " (" <> knowledgeId k <> ")"
        , ""
        , knowledgeBody k
        , ""
        ]) ks

promptRelated :: [Knowledge] -> [Text]
promptRelated [] = []
promptRelated ks =
    [ "## Related knowledge"
    , ""
    , "These entries share categories with this task. They may not all apply directly — use judgment."
    , ""
    ]
    <> concatMap (\k ->
        [ "### " <> knowledgeTitle k <> " (" <> knowledgeId k <> ")"
        , ""
        , knowledgeBody k
        , ""
        ]) ks

workingAgreement :: Task -> [Text]
workingAgreement t =
    [ "## Working agreement"
    , ""
    , "You are a headless dispatch working on this task. Guardrails:"
    , ""
    , "- All task/knowledge mutation MUST go through the `icarium` CLI."
    , "- If blocked:  `./bin/icarium task update " <> taskId t <> " --state blocked --block-reason '<why>'`"
    , "- Record anything you learn that future tasks should know as knowledge:"
    , "    `./bin/icarium know add '<title>' --body-stdin`"
    , "- Commit your code before exiting; the program marks the task done after the gates pass and the FF-merge succeeds."
    , ""
    ]

-- | Recommended maximum title length (matches git commit subject convention).
-- Titles exceeding this are truncated with an ellipsis in list views.
recommendedTitleMax :: Int
recommendedTitleMax = 72

-- =============================================================
-- Grouped task list rendering
-- =============================================================

-- | Render tasks grouped by state.
--
-- @useUnicode@: True → ● / · bars; False → # / . bars (ASCII fallback).
-- @filterStates@: the states explicitly requested by the caller.
--   * Empty → default view: show all four primary group headers (even if
--     empty) plus any secondary groups that have tasks.
--   * Length == 1 → single-state filter: suppress the group header.
--   * Length > 1 → multi-state filter: show a header for each requested
--     state (even if empty).
renderTaskList :: Bool -> [TaskRow] -> [TaskState] -> Text
renderTaskList useUnicode rows filterStates
    | null rows && singleFilter = "(no tasks)\n"
    | otherwise = T.intercalate "\n" (map renderBlock stateGroups)
  where
    singleFilter = length filterStates == 1

    -- Display order: primary groups always present; secondary only if occupied.
    primaryStates   = [Ready, Planned, Blocked, Idea]
    secondaryStates = [InProgress, Abandoned, Done]

    inState s = filter (\r -> taskState (trTask r) == s) rows

    stateGroups = case filterStates of
        [] -> primaryStates
           ++ filter (\s -> not (null (inState s))) secondaryStates
        _  -> filterStates

    -- Sort within a group: priority DESC (higher = first), NULL last;
    -- then ULID DESC as tiebreaker (newer first).
    sortGroup = sortBy cmpRow
      where
        cmpRow a b =
            let pa = taskPriority (trTask a)
                pb = taskPriority (trTask b)
            in case (pa, pb) of
                (Nothing, Nothing) -> compareUlid b a
                (Just _,  Nothing) -> LT
                (Nothing, Just _)  -> GT
                (Just x,  Just y)  -> case compare y x of
                    EQ -> compareUlid b a
                    o  -> o
        compareUlid r1 r2 = compare (taskId (trTask r1)) (taskId (trTask r2))

    -- Global column widths across all rows. Title capped at recommendedTitleMax.
    titleWidth = min recommendedTitleMax (maxLen 5 (map (T.length . taskTitle . trTask) rows))
    catWidth   = maxLen 3 (map (T.length . formatCats . trCats) rows)

    maxLen def [] = def
    maxLen _   xs = maximum xs

    -- Render one state group as a newline-terminated block of lines.
    renderBlock s =
        let groupRows = sortGroup (inState s)
            n         = length groupRows
            header    = if singleFilter then []
                        else [T.toUpper (taskStateText s) <> "  (" <> T.pack (show n) <> ")"]
            rowLines  = map (renderRow s) groupRows
        in T.unlines (header ++ rowLines)

    renderRow s row =
        let t       = trTask row
            idPart  = "  " <> padr 10 (T.take 10 (taskId t))
            titPart = padr titleWidth (truncateTitle useUnicode titleWidth (taskTitle t))
            barPart = case s of
                Blocked -> truncateReason (fromMaybe "" (taskBlockReason t))
                _       -> mkBar useUnicode (taskPriority t)
            catStr  = formatCats (trCats row)
            catPart = padr catWidth catStr
            edgePart = case s of
                Blocked -> ""
                _       -> let ec = formatEdgeCounts (trDeps row) (trRefs row)
                            in if T.null ec then "" else "  " <> ec
        in idPart <> "  " <> titPart <> "  " <> barPart <> "  " <> catPart <> edgePart

    truncateReason r
        | T.length r <= 60 = r
        | otherwise        = T.take 57 r <> "..."

-- | 10-cell priority bar. UTF-8 mode: ● filled, · empty. ASCII mode: # / .
mkBar :: Bool -> Maybe Int -> Text
mkBar utf8 Nothing  = T.replicate 10 dot
  where dot = if utf8 then "·" else "."
mkBar utf8 (Just p) = T.replicate filled bullet <> T.replicate empty dot
  where
    filled = max 0 (min 10 p)
    empty  = 10 - filled
    bullet = if utf8 then "●" else "#"
    dot    = if utf8 then "·" else "."

-- | Truncate a title to fit within @width@ characters, appending an ellipsis
-- when truncation occurs. UTF-8 mode uses the single-char @…@; ASCII uses @...@.
truncateTitle :: Bool -> Int -> Text -> Text
truncateTitle utf8 width title
    | T.length title <= width = title
    | utf8                    = T.take (width - 1) title <> "…"
    | otherwise               = T.take (width - 3) title <> "..."

-- | Format categories as [dom/disc], [-/disc], [dom/-], or [-].
formatCats :: [Category] -> Text
formatCats cats =
    let dom  = listToMaybe [categoryName c | c <- cats, categoryAxis c == Domain]
        disc = listToMaybe [categoryName c | c <- cats, categoryAxis c == Discipline]
    in case (dom, disc) of
        (Nothing, Nothing) -> "[-]"
        (Just d,  Nothing) -> "[" <> d <> "/-]"
        (Nothing, Just di) -> "[-/" <> di <> "]"
        (Just d,  Just di) -> "[" <> d <> "/" <> di <> "]"

-- | Edge count annotation. Empty string when both counts are 0.
formatEdgeCounts :: Int -> Int -> Text
formatEdgeCounts 0 0 = ""
formatEdgeCounts d r =
    "[" <> T.intercalate " " parts <> "]"
  where
    parts = (if d > 0 then ["deps:" <> T.pack (show d)] else [])
         <> (if r > 0 then ["refs:" <> T.pack (show r)] else [])

-- =============================================================
-- Knowledge rendering
-- =============================================================

renderKnowledge :: Knowledge -> [Category] -> Text
renderKnowledge k cats = T.unlines $
    [ "id:       " <> knowledgeId k
    , "title:    " <> knowledgeTitle k
    , "stale:    " <> (if knowledgeStale k then "yes" else "no")
    , "created:  " <> knowledgeCreatedAt k
    , "updated:  " <> knowledgeUpdatedAt k
    ]
    <> categoriesBlock cats
    <> [ ""
       , "## Body"
       , ""
       , if T.null (knowledgeBody k) then "(no body)" else knowledgeBody k
       ]

renderKnowledgeList :: Bool -> [Knowledge] -> Text
renderKnowledgeList _ [] = "(no knowledge)\n"
renderKnowledgeList utf8 ks = T.unlines $ header : map row ks
  where
    header = padr 12 "id" <> "  " <> padr 6 "stale" <> "  title"
    row k = padr 12 (T.take 10 (knowledgeId k)) <> "  "
         <> padr 6 (if knowledgeStale k then "yes" else "no") <> "  "
         <> truncateTitle utf8 recommendedTitleMax (knowledgeTitle k)

renderEdgeLine :: Edge -> Text
renderEdgeLine e =
    T.take 10 (edgeId e) <> "  "
    <> edgeKindText (edgeKind e) <> "  "
    <> nodeKindText (edgeSrcKind e) <> ":" <> T.take 10 (edgeSrcId e)
    <> "  ->  "
    <> nodeKindText (edgeDstKind e) <> ":" <> T.take 10 (edgeDstId e)

renderCategory :: Category -> Text
renderCategory c =
    categoryAxisText (categoryAxis c) <> "  " <> categoryName c
    <> "  (" <> categoryId c <> ")"

categoriesBlock :: [Category] -> [Text]
categoriesBlock [] = []
categoriesBlock cats =
    "Categories:"
    : concatMap axisLine [Domain, Discipline]
  where
    axisLine axis =
        let names = [categoryName c | c <- cats, categoryAxis c == axis]
        in if null names then []
           else ["  " <> padr 12 (categoryAxisText axis <> ":") <> T.intercalate ", " names]

-- =============================================================
-- Utilities
-- =============================================================

padr :: Int -> Text -> Text
padr n s = s <> T.replicate (max 0 (n - T.length s)) " "
