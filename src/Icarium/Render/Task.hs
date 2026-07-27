module Icarium.Render.Task (
    renderTaskHuman,
    renderTaskPrompt,
    untaggedPromptWarning,
    untaggedAddNudge,
    emptyBodyNudge,
    TaskRow (..),
    renderTaskList,
    mkBar,
) where

import Data.List (sortBy)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T

import Icarium.Render.Internal
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
renderTaskHuman :: Bool -> Task -> Text -> [Context] -> [Task] -> [Task] -> [Category] -> [Text] -> Text
renderTaskHuman utf8 t bodyPath refs deps derived cats retiredIds =
    T.unlines $
        [ "id:        " <> taskId t
        , "title:     " <> taskTitle t
        , "state:     " <> taskStateText (taskState t)
        , priorityLine t
        ]
            <> blockReasonLines t
            <> noCommitLine t
            <> routingLines t
            <> claimLines t
            <> [ "created:   " <> taskCreatedAt t
               , "updated:   " <> taskUpdatedAt t
               , "body:      " <> bodyPath
               ]
            <> categoriesBlock cats
            <> [""]
            <> linksSection utf8 t deps derived refs retiredIds

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

{- | Dispatch routing overrides. Absent when the task inherits the
@[dispatch]@ defaults, so the lines mean "this task is special".
-}
routingLines :: Task -> [Text]
routingLines t =
    ["model:     " <> m | m <- maybeToList (rtModel (taskRouting t))]
        <> ["effort:    " <> effortText e | e <- maybeToList (rtEffort (taskRouting t))]

-- | Set by `task claim`; absent for tasks nobody claimed.
claimLines :: Task -> [Text]
claimLines t =
    ["owner:     " <> w | w <- maybeToList (taskClaimedBy t)]
        <> ["claimed:   " <> a | a <- maybeToList (taskClaimedAt t)]

{- | Combined links tree replacing the old flat dep/ref sections.

All depends-on edges first (sorted by target id ASC), then references.
Last edge gets └─ (or \- in ASCII mode); others get ├─ (or +-).
-}
linksSection :: Bool -> Task -> [Task] -> [Task] -> [Context] -> [Text] -> [Text]
linksSection _ _ [] [] [] _ = ["## Links", "", "(none)", ""]
linksSection utf8 t deps derived refs retiredIds = ["## Links", "", rootLine] <> edgeLines <> [""]
  where
    rootLine = T.take 10 (taskId t) <> "  " <> taskTitle t

    byId = sortBy (\a b -> compare (taskId a) (taskId b))
    sortedRefs = sortBy (\a b -> compare (contextId a) (contextId b)) refs

    -- Left carries the kind: two task→task kinds now share the column, and the
    -- spelling stays owned by 'edgeKindDisplay' rather than repeated here.
    allEdges :: [Either (EdgeKind, Task) Context]
    allEdges =
        map (\d -> Left (DependsOn, d)) (byId deps)
            <> map (\d -> Left (DerivedFrom, d)) (byId derived)
            <> map Right sortedRefs
    n = length allEdges

    kindWidth =
        maximum (map (T.length . edgeKindDisplay) [DependsOn, References, DerivedFrom])

    branchG = if utf8 then "├─" else "+-"
    lastG = if utf8 then "└─" else "\\-"

    mkEdge i e =
        let g = if i == n - 1 then lastG else branchG
            kindStr = either (edgeKindDisplay . fst) (const (edgeKindDisplay References)) e
            idStr = either (T.take 10 . taskId . snd) (T.take 10 . contextId) e
            titStr = either (taskTitle . snd) contextTitle e
            suffix = case e of
                Left (_, dep) -> "  [" <> taskStateText (taskState dep) <> "]"
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

{- | Stderr lines for a prompt rendered from a task with no retrieval axis:
the block is about to go out with zero auto-pulled context. Emitted by both
@task show --prompt@ and dispatch, so the signal reads the same either way.
Either axis alone clears the guard, so the suggested command asks for one.
-}
untaggedPromptWarning :: Text -> [Text]
untaggedPromptWarning tid =
    [ "warn: " <> tid <> " has no domain or discipline — no context auto-pulled."
    , "warn:   icarium task update " <> tid <> " --domain <name>"
    ]

-- | Advisory nudge at @task add@, same guard as 'untaggedPromptWarning'.
untaggedAddNudge :: Text -> [Text]
untaggedAddNudge tid =
    [ "# next: Untagged — no context will auto-pull."
    , "#       icarium task update " <> tid <> " --domain <name>"
    ]

-- | Shown on @add@ with no body: where to Write it, how to edit it later.
emptyBodyNudge :: NodeKind -> Text -> FilePath -> [Text]
emptyBodyNudge kind nid fp =
    [ "# next: Write your markdown to " <> T.pack fp
    , "# to edit later: Read $(icarium " <> nodeKindCli kind <> " path " <> nid <> ") then Edit"
    ]

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

-- =============================================================
-- Flat task list rendering
-- =============================================================

{- | Render a flat, priority-sorted task list with state badges.

@useUnicode@: True → Unicode ellipsis; False → ASCII fallback.
Rows are sorted priority DESC, ULID ASC; null priority sorts last.
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
                (Nothing, Nothing) -> compareUlid a b
                (Just _, Nothing) -> LT
                (Nothing, Just _) -> GT
                (Just x, Just y) -> case compare y x of
                    EQ -> compareUlid a b
                    o -> o
    -- Oldest first within a priority, matching the queue's ordering — so a
    -- rendered queue and `task next` cannot disagree about the head row.
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

-- | Edge count annotation. Empty string when both counts are 0.
formatEdgeCounts :: Int -> Int -> Text
formatEdgeCounts 0 0 = ""
formatEdgeCounts d r =
    "[" <> T.intercalate " " parts <> "]"
  where
    parts =
        ["deps:" <> T.pack (show d) | d > 0]
            <> ["refs:" <> T.pack (show r) | r > 0]
