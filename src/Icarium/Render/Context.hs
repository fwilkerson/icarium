module Icarium.Render.Context (
    renderContext,
    isRetired,
    ContextRow (..),
    renderContextList,
    CurationQueueRow (..),
    renderCurationQueue,
    formatLinkedCount,
) where

import Data.Text (Text)
import Data.Text qualified as T

import Icarium.Render.Internal
import Icarium.Types

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
