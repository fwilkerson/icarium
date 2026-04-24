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
renderTaskHuman :: Task -> [Knowledge] -> [Task] -> Text
renderTaskHuman t refs deps = T.unlines $
    [ "id:        " <> taskId t
    , "title:     " <> taskTitle t
    , "state:     " <> taskStateText (taskState t)
    , priorityLine t
    ]
    <> blockReasonLines t
    <> [ "created:   " <> taskCreatedAt t
       , "updated:   " <> taskUpdatedAt t
       , ""
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
                       <> taskId d <> "  " <> taskTitle d) deps
    <> [""]

refSection :: [Knowledge] -> [Text]
refSection []   = ["## Referenced knowledge", "", "(none)", ""]
refSection refs = ["## Referenced knowledge", ""]
    <> concatMap (\k ->
        [ "### " <> knowledgeId k <> "  " <> knowledgeTitle k
            <> if knowledgeStale k then "  [STALE]" else ""
        , ""
        , knowledgeBody k
        , ""
        ]) refs

-- | The exact prompt the dispatcher will send to the headless agent.
-- Sharing this with @task show --prompt@ keeps the two in lockstep.
renderTaskPrompt :: Task -> [Knowledge] -> [Task] -> Text
renderTaskPrompt t refs deps = T.unlines $
    [ "# Task " <> taskId t
    , ""
    , "**" <> taskTitle t <> "**"
    , ""
    , if T.null (taskBody t) then "(no body)" else taskBody t
    , ""
    ]
    <> promptDeps deps
    <> promptRefs refs
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

workingAgreement :: Task -> [Text]
workingAgreement t =
    [ "## Working agreement"
    , ""
    , "You are a headless dispatch working on this task. Guardrails:"
    , ""
    , "- All task/knowledge mutation MUST go through the `icarium` CLI."
    , "- Mark start:  `icarium task update " <> taskId t <> " --state in_progress`"
    , "- Mark done:   `icarium task update " <> taskId t <> " --state done`"
    , "- If blocked:  `icarium task update " <> taskId t <> " --state blocked --block-reason '<why>'`"
    , "- Record anything you learn that future tasks should know as knowledge:"
    , "    `icarium know add '<title>' --body-stdin`"
    , "- Commit code before marking done."
    , ""
    ]

-- =============================================================
-- List rendering (human)
-- =============================================================

renderTaskList :: [Task] -> Text
renderTaskList [] = "(no tasks)\n"
renderTaskList ts = T.unlines $ header : map row ts
  where
    header = padr 28 "id" <> "  "
          <> padr 10 "state" <> "  "
          <> padr 4  "pri"   <> "  "
          <> "title"
    row t = padr 28 (taskId t) <> "  "
         <> padr 10 (taskStateText (taskState t)) <> "  "
         <> padr 4  (prio (taskPriority t)) <> "  "
         <> taskTitle t
    prio Nothing  = "-"
    prio (Just p) = T.pack (show p)

renderKnowledge :: Knowledge -> Text
renderKnowledge k = T.unlines $
    [ "id:       " <> knowledgeId k
    , "title:    " <> knowledgeTitle k
    , "stale:    " <> (if knowledgeStale k then "yes" else "no")
    , "created:  " <> knowledgeCreatedAt k
    , "updated:  " <> knowledgeUpdatedAt k
    , ""
    , "## Body"
    , ""
    , if T.null (knowledgeBody k) then "(no body)" else knowledgeBody k
    ]

renderKnowledgeList :: [Knowledge] -> Text
renderKnowledgeList [] = "(no knowledge)\n"
renderKnowledgeList ks = T.unlines $ header : map row ks
  where
    header = padr 28 "id" <> "  " <> padr 6 "stale" <> "  title"
    row k = padr 28 (knowledgeId k) <> "  "
         <> padr 6 (if knowledgeStale k then "yes" else "no") <> "  "
         <> knowledgeTitle k

renderEdgeLine :: Edge -> Text
renderEdgeLine e =
    edgeId e <> "  "
    <> edgeKindText (edgeKind e) <> "  "
    <> nodeKindText (edgeSrcKind e) <> ":" <> edgeSrcId e
    <> "  ->  "
    <> nodeKindText (edgeDstKind e) <> ":" <> edgeDstId e

renderCategory :: Category -> Text
renderCategory c =
    categoryAxisText (categoryAxis c) <> "  " <> categoryName c
    <> "  (" <> categoryId c <> ")"

padr :: Int -> Text -> Text
padr n s = s <> T.replicate (max 0 (n - T.length s)) " "
