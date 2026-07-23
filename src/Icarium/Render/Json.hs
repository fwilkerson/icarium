{- | Machine-readable rendering for the read surface (@--json@).

The shape is a contract: snake_case keys drawn from the domain
vocabulary, lists as JSON arrays (@[]@ when empty, never prose).
Bodies are never included — the body file is the canonical way to read
content, so the show renderers carry @body_path@ instead.

Encodings are built with 'E.pairs' rather than 'Data.Aeson.object' so key
order is the order written here, not hash order.
-}
module Icarium.Render.Json (
    renderTaskListJson,
    renderTaskShowJson,
    renderContextListJson,
    renderContextShowJson,
    renderCurationQueueJson,
    renderSearchJson,
) where

import Data.Aeson.Encoding (Encoding, Series)
import Data.Aeson.Encoding qualified as E
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)

import Icarium.Render.Context (ContextRow (..), CurationQueueRow (..), isRetired)
import Icarium.Render.Search (SearchHitRow (..))
import Icarium.Render.Task (TaskRow (..))
import Icarium.Repo.Search (SearchHit (..))
import Icarium.Types

-- =============================================================
-- Tasks
-- =============================================================

renderTaskListJson :: [TaskRow] -> BL.ByteString
renderTaskListJson = enc . E.list taskRowEnc

taskRowEnc :: TaskRow -> Encoding
taskRowEnc r =
    E.pairs $
        taskCore (trTask r)
            <> catsSeries (trCats r)
            <> E.pair "depends_on_count" (E.int (trDeps r))
            <> E.pair "references_count" (E.int (trRefs r))

-- | Mirrors 'Icarium.Render.renderTaskHuman': metadata, body path, links.
renderTaskShowJson :: Task -> Text -> [Context] -> [Task] -> [Task] -> [Category] -> [Text] -> BL.ByteString
renderTaskShowJson t bodyPath refs deps derived cats retiredIds =
    enc . E.pairs $
        taskCore t
            <> E.pair "body_path" (E.text bodyPath)
            <> catsSeries cats
            <> E.pair "depends_on" (E.list taskLinkEnc deps)
            <> E.pair "derived_from" (E.list taskLinkEnc derived)
            <> E.pair "references" (E.list (contextLinkEnc retiredIds) refs)

taskCore :: Task -> Series
taskCore t =
    E.pair "id" (E.text (taskId t))
        <> E.pair "title" (E.text (taskTitle t))
        <> E.pair "state" (E.text (taskStateText (taskState t)))
        <> E.pair "priority" (maybe E.null_ E.int (taskPriority t))
        <> E.pair "block_reason" (maybeText (taskBlockReason t))
        <> E.pair "no_commit" (E.bool (taskNoCommit t))
        <> E.pair "model" (maybeText (taskModel t))
        <> E.pair "effort" (maybeText (effortText <$> taskEffort t))
        <> E.pair "created_at" (E.text (taskCreatedAt t))
        <> E.pair "updated_at" (E.text (taskUpdatedAt t))

taskLinkEnc :: Task -> Encoding
taskLinkEnc t =
    E.pairs $
        E.pair "id" (E.text (taskId t))
            <> E.pair "title" (E.text (taskTitle t))
            <> E.pair "state" (E.text (taskStateText (taskState t)))

-- =============================================================
-- Contexts
-- =============================================================

renderContextListJson :: [ContextRow] -> BL.ByteString
renderContextListJson = enc . E.list contextRowEnc

contextRowEnc :: ContextRow -> Encoding
contextRowEnc r =
    E.pairs $
        contextCore (crContext r)
            <> E.pair "retired" (E.bool (crRetired r))
            <> catsSeries (crCats r)
            <> E.pair "linked_count" (E.int (crLinked r))

-- | @mEvent@: latest curation event; @retired@ + @curation@ derive from it.
renderContextShowJson :: Context -> [Category] -> Text -> Maybe CurationEvent -> BL.ByteString
renderContextShowJson cx cats bodyPath mEvent =
    enc . E.pairs $
        contextCore cx
            <> E.pair "retired" (E.bool (isRetired mEvent))
            <> E.pair "curation" (maybe E.null_ curationEnc mEvent)
            <> E.pair "body_path" (E.text bodyPath)
            <> catsSeries cats

curationEnc :: CurationEvent -> Encoding
curationEnc e =
    E.pairs $
        E.pair "disposition" (E.text (dispositionText (curationDisposition e)))
            <> E.pair "artifact" (maybeText (curationArtifact e))
            <> E.pair "note" (maybeText (curationNote e))
            <> E.pair "created_at" (E.text (curationCreatedAt e))

-- | @last_curation@ is null for never-curated entries.
renderCurationQueueJson :: [CurationQueueRow] -> BL.ByteString
renderCurationQueueJson = enc . E.list queueRowEnc
  where
    queueRowEnc r =
        E.pairs $
            contextCore (cqContext r)
                <> catsSeries (cqCats r)
                <> E.pair "last_curation" (maybe E.null_ curationEnc (cqLastEvent r))

contextCore :: Context -> Series
contextCore cx =
    E.pair "id" (E.text (contextId cx))
        <> E.pair "title" (E.text (contextTitle cx))
        <> E.pair "created_at" (E.text (contextCreatedAt cx))
        <> E.pair "updated_at" (E.text (contextUpdatedAt cx))

contextLinkEnc :: [Text] -> Context -> Encoding
contextLinkEnc retiredIds cx =
    E.pairs $
        E.pair "id" (E.text (contextId cx))
            <> E.pair "title" (E.text (contextTitle cx))
            <> E.pair "retired" (E.bool (contextId cx `elem` retiredIds))

-- =============================================================
-- Search
-- =============================================================

{- | An object, not a bare array: @total@ is the full match count, so a
consumer using @--limit@ can detect truncation (the human footer's
"showing X of Y" equivalent). Hits are in rank order. No snippet:
snippets are body content, and the body file stays the canonical source.
-}
renderSearchJson :: Int -> [SearchHitRow] -> BL.ByteString
renderSearchJson total rows =
    enc . E.pairs $
        E.pair "total" (E.int total)
            <> E.pair "hits" (E.list searchHitEnc rows)

searchHitEnc :: SearchHitRow -> Encoding
searchHitEnc r =
    let h = shrHit r
     in E.pairs $
            E.pair "id" (E.text (hitId h))
                <> E.pair "kind" (E.text (nodeKindText (hitKind h)))
                <> E.pair "title" (E.text (hitTitle h))
                <> E.pair "state" (maybe E.null_ (E.text . taskStateText) (hitState h))
                <> E.pair "retired" (E.bool (hitRetired h))
                <> E.pair "updated_at" (E.text (hitUpdatedAt h))
                <> E.pair "title_match" (E.bool (hitTitleMatch h))
                <> E.pair "body_match" (E.bool (hitBodyMatch h))
                <> catsSeries (shrCats r)

-- =============================================================
-- Shared
-- =============================================================

catsSeries :: [Category] -> Series
catsSeries cats = E.pair "categories" (E.list catEnc cats)

catEnc :: Category -> Encoding
catEnc cat =
    E.pairs $
        E.pair "axis" (E.text (categoryAxisText (categoryAxis cat)))
            <> E.pair "name" (E.text (categoryName cat))

maybeText :: Maybe Text -> Encoding
maybeText = maybe E.null_ E.text

enc :: Encoding -> BL.ByteString
enc = E.encodingToLazyByteString
