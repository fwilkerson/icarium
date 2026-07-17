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
    renderSearchJson,
) where

import Data.Aeson.Encoding (Encoding, Series)
import Data.Aeson.Encoding qualified as E
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)

import Icarium.Render (ContextRow (..), SearchHitRow (..), TaskRow (..))
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
renderTaskShowJson :: Task -> Text -> [Context] -> [Task] -> [Category] -> BL.ByteString
renderTaskShowJson t bodyPath refs deps cats =
    enc . E.pairs $
        taskCore t
            <> E.pair "body_path" (E.text bodyPath)
            <> catsSeries cats
            <> E.pair "depends_on" (E.list taskLinkEnc deps)
            <> E.pair "references" (E.list contextLinkEnc refs)

taskCore :: Task -> Series
taskCore t =
    E.pair "id" (E.text (taskId t))
        <> E.pair "title" (E.text (taskTitle t))
        <> E.pair "state" (E.text (taskStateText (taskState t)))
        <> E.pair "priority" (maybe E.null_ E.int (taskPriority t))
        <> E.pair "block_reason" (maybeText (taskBlockReason t))
        <> E.pair "no_commit" (E.bool (taskNoCommit t))
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
            <> catsSeries (crCats r)
            <> E.pair "linked_count" (E.int (crLinked r))

renderContextShowJson :: Context -> [Category] -> Text -> BL.ByteString
renderContextShowJson cx cats bodyPath =
    enc . E.pairs $
        contextCore cx
            <> E.pair "body_path" (E.text bodyPath)
            <> catsSeries cats

contextCore :: Context -> Series
contextCore cx =
    E.pair "id" (E.text (contextId cx))
        <> E.pair "title" (E.text (contextTitle cx))
        <> E.pair "stale" (E.bool (contextStale cx))
        <> E.pair "created_at" (E.text (contextCreatedAt cx))
        <> E.pair "updated_at" (E.text (contextUpdatedAt cx))

contextLinkEnc :: Context -> Encoding
contextLinkEnc cx =
    E.pairs $
        E.pair "id" (E.text (contextId cx))
            <> E.pair "title" (E.text (contextTitle cx))
            <> E.pair "stale" (E.bool (contextStale cx))

-- =============================================================
-- Search
-- =============================================================

{- | Hits only, in rank order. No snippet: snippets are body content, and
the body file stays the canonical source for that.
-}
renderSearchJson :: [SearchHitRow] -> BL.ByteString
renderSearchJson = enc . E.list searchHitEnc

searchHitEnc :: SearchHitRow -> Encoding
searchHitEnc r =
    let h = shrHit r
     in E.pairs $
            E.pair "id" (E.text (hitId h))
                <> E.pair "kind" (E.text (nodeKindText (hitKind h)))
                <> E.pair "title" (E.text (hitTitle h))
                <> E.pair "state" (maybe E.null_ (E.text . taskStateText) (hitState h))
                <> E.pair "stale" (E.bool (hitStale h))
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
