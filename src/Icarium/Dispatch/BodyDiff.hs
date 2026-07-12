{- | Section-level diff of a task body across a dispatch run — the tamper
signal for the reviewer. The worker can legitimately append a ## Proof
or ## Notes section; any other change to worker-writable text (edited
acceptance criteria, dropped provenance) must surface as a reviewable
fact rather than pass silently (issue #12).

The rendered report is prepended to reviewer stdin and deliberately
contains no fenced code blocks: body sections may carry fences of their
own, and reviewer verdict parsing is fence-anchored.
-}
module Icarium.Dispatch.BodyDiff (
    BodyDiff (..),
    diffBody,
    bodyChanged,
    renderBodyReport,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

{- | Section titles are display text; the preamble (text before the
first ## heading) is carried as 'Nothing' and rendered as @(preamble)@.
-}
data BodyDiff = BodyDiff
    { bdEdited :: [(Maybe Text, Text, Text)]
    -- ^ title, old text, new text
    , bdRemoved :: [(Maybe Text, Text)]
    -- ^ title, old text
    , bdAdded :: [(Maybe Text, Text)]
    {- ^ title, new text — newly-added Proof/Notes sections are exempt
    and never appear here
    -}
    }
    deriving (Show, Eq)

bodyChanged :: BodyDiff -> Bool
bodyChanged d = not (null (bdEdited d) && null (bdRemoved d) && null (bdAdded d))

{- | Compare two bodies section-by-section on ## headings (H2 only).
Section text is compared stripped of surrounding whitespace, so pure
reflow of blank lines does not flag. Title matching ignores case and
surrounding whitespace; duplicate titles within one body have their text
concatenated. Only *newly-added* Proof/Notes sections are exempt — an
edited or removed one still counts.
-}
diffBody :: Text -> Text -> BodyDiff
diffBody old new =
    BodyDiff
        { bdEdited =
            [ (title, oldText, newText)
            | (key, title, oldText) <- oldSecs
            , Just newText <- [lookup3 key newSecs]
            , oldText /= newText
            ]
        , bdRemoved =
            [ (title, oldText)
            | (key, title, oldText) <- oldSecs
            , Nothing <- [lookup3 key newSecs]
            ]
        , bdAdded =
            [ (title, newText)
            | (key, title, newText) <- newSecs
            , key `notElem` [Just "proof", Just "notes"]
            , Nothing <- [lookup3 key oldSecs]
            ]
        }
  where
    oldSecs = sections old
    newSecs = sections new
    lookup3 k xs = case [v | (k', _, v) <- xs, k' == k] of
        [] -> Nothing
        (v : _) -> Just v

{- | (match key, display title, stripped text) per section, in body order.
Key 'Nothing' = preamble; a whitespace-only preamble yields no section.
-}
sections :: Text -> [(Maybe Text, Maybe Text, Text)]
sections body = dedupe (preamble <> map section rest)
  where
    (pre, rest) = breakSections (T.lines body)
    preText = T.strip (T.unlines pre)
    preamble = [(Nothing, Nothing, preText) | not (T.null preText)]
    section (heading, ls) =
        let title = T.strip (T.drop 3 heading)
         in (Just (T.toLower title), Just title, T.strip (T.unlines ls))
    dedupe [] = []
    dedupe ((k, t, v) : xs) =
        let (same, others) = span' k xs
         in (k, t, T.intercalate "\n\n" (filter (not . T.null) (v : same))) : dedupe others
    span' k xs = ([v | (k', _, v) <- xs, k' == k], [x | x@(k', _, _) <- xs, k' /= k])

-- | Split lines into (preamble lines, [(heading line, section lines)]).
breakSections :: [Text] -> ([Text], [(Text, [Text])])
breakSections ls = case break isHeading ls of
    (pre, []) -> (pre, [])
    (pre, h : rest) -> (pre, go h rest)
  where
    isHeading l = "## " `T.isPrefixOf` l
    go h rest = case break isHeading rest of
        (body, []) -> [(h, body)]
        (body, h' : rest') -> (h, body) : go h' rest'

{- | The report text: a yes/no line, then delimited old/new blocks per
change. Section text is rendered with every line quoted (@> @) so
worker-written content can neither open a fenced block nor impersonate
the reviewer-stdin structure (@# Task@ / @# Diff@ headings).
-}
renderBodyReport :: BodyDiff -> Text
renderBodyReport d
    | not (bodyChanged d) = "task body changed during run: no"
    | otherwise =
        T.intercalate "\n\n" $
            "task body changed during run: yes"
                : [ block ("changed section: " <> disp t) [("old", o), ("new", n)]
                  | (t, o, n) <- bdEdited d
                  ]
                    <> [block ("removed section: " <> disp t) [("old", o)] | (t, o) <- bdRemoved d]
                    <> [block ("added section: " <> disp t) [("new", n)] | (t, n) <- bdAdded d]
  where
    disp = fromMaybe "(preamble)"
    block header parts =
        T.intercalate "\n" $
            header : concat [("--- " <> lbl <> " ---") : quote txt | (lbl, txt) <- parts]
    quote = map ("> " <>) . T.lines
