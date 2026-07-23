-- | One-line renderers for the graph plumbing entities: edges and categories.
module Icarium.Render.Graph (
    renderEdgeLine,
    renderCategory,
) where

import Data.Text (Text)
import Data.Text qualified as T

import Icarium.Types

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
