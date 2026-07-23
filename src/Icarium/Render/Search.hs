module Icarium.Render.Search (
    SearchHitRow (..),
    renderSearchList,
) where

import Data.Text (Text)
import Data.Text qualified as T

import Icarium.Render.Internal
import Icarium.Repo.Search (SearchHit (..))
import Icarium.Types

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
