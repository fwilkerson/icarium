-- | Layout primitives shared by the per-entity renderers.
module Icarium.Render.Internal (
    recommendedTitleMax,
    maxLen,
    padr,
    truncateTitle,
    formatCats,
    categoriesBlock,
    stateBadgeText,
) where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Icarium.Types

{- | Recommended maximum title length (matches git commit subject convention).
Titles exceeding this are truncated with an ellipsis in list views.
-}
recommendedTitleMax :: Int
recommendedTitleMax = 72

maxLen :: Int -> [Int] -> Int
maxLen def [] = def
maxLen _ xs = maximum xs

padr :: Int -> Text -> Text
padr n s = s <> T.replicate (max 0 (n - T.length s)) " "

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

categoriesBlock :: [Category] -> [Text]
categoriesBlock [] = []
categoriesBlock cats =
    "Categories:"
        : concatMap axisLine [Domain, Discipline, Kind]
  where
    axisLine axis =
        let names = [categoryName c | c <- cats, categoryAxis c == axis]
         in [ "  " <> padr 12 (categoryAxisText axis <> ":") <> T.intercalate ", " names
            | not (null names)
            ]

-- | Badges read as the CLI spells states, not as the DB stores them.
stateBadgeText :: TaskState -> Text
stateBadgeText = taskStateCli
