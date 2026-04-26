module Icarium.Render
    ( renderTaskHuman
    , renderTaskPrompt
    , renderTaskList
    , renderKnowledge
    , renderKnowledgeList
    , renderEdgeLine
    , renderCategory
    ) where

import           Data.Text     (Text)
import qualified Data.Text     as T

import           Icarium.Types

-- =============================================================
-- Task rendering
-- =============================================================

-- | Human-facing task view. Shows metadata + body + linked nodes.
renderTaskHuman :: Task -> [Knowledge] -> [Task] -> [Category] -> Text
renderTaskHuman t refs deps cats = T.unlines $
    [ "id:        " <> T.take 10 (taskId t)
    , "slug:      " <> maybe "-" id (taskSlug t)
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

-- =============================================================
-- List rendering (human)
-- =============================================================

renderTaskList :: [Task] -> Text
renderTaskList [] = "(no tasks)\n"
renderTaskList ts = T.unlines $ header : map row ts
  where
    header = padr 12 "id" <> "  "
          <> padr 10 "state" <> "  "
          <> padr 4  "pri"   <> "  "
          <> "title"
    row t = padr 12 (T.take 10 (taskId t)) <> "  "
         <> padr 10 (taskStateText (taskState t)) <> "  "
         <> padr 4  (prio (taskPriority t)) <> "  "
         <> taskTitle t
    prio Nothing  = "-"
    prio (Just p) = T.pack (show p)

renderKnowledge :: Knowledge -> [Category] -> Text
renderKnowledge k cats = T.unlines $
    [ "id:       " <> T.take 10 (knowledgeId k)
    , "slug:     " <> maybe "-" id (knowledgeSlug k)
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

renderKnowledgeList :: [Knowledge] -> Text
renderKnowledgeList [] = "(no knowledge)\n"
renderKnowledgeList ks = T.unlines $ header : map row ks
  where
    header = padr 12 "id" <> "  " <> padr 6 "stale" <> "  title"
    row k = padr 12 (T.take 10 (knowledgeId k)) <> "  "
         <> padr 6 (if knowledgeStale k then "yes" else "no") <> "  "
         <> knowledgeTitle k

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

padr :: Int -> Text -> Text
padr n s = s <> T.replicate (max 0 (n - T.length s)) " "
