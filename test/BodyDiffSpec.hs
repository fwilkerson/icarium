module BodyDiffSpec (tests) where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Dispatch.BodyDiff (bodyChanged, diffBody, renderBodyReport)

tests :: TestTree
tests =
    testGroup
        "BodyDiff"
        [ testGroup
            "diffBody"
            [ testCase "unchanged body -> no" $
                changed base base @?= False
            , testCase "whitespace-only reflow -> no" $
                changed base (base <> "\n\n") @?= False
            , testCase "edited criteria section -> yes" $
                changed base (T.replace "- must pass" "- anything goes" base) @?= True
            , testCase "removed section -> yes" $
                changed base "intro line\n\n## Goal\ndo the thing\n" @?= True
            , testCase "added Proof only -> no" $
                changed base (base <> "\n## Proof\ngates green\n") @?= False
            , testCase "added Notes only -> no" $
                changed base (base <> "\n## Notes\nobservations\n") @?= False
            , testCase "Proof exemption ignores title case" $
                changed base (base <> "\n##  proof \ngates green\n") @?= False
            , testCase "added non-exempt section -> yes" $
                changed base (base <> "\n## Authorization\ngranted\n") @?= True
            , testCase "edited existing Proof section -> yes (only newly-added is exempt)" $
                changed
                    (base <> "\n## Proof\nattempt 1\n")
                    (base <> "\n## Proof\nrewritten\n")
                    @?= True
            , testCase "body with no headings, edited -> yes" $
                changed "just prose" "different prose" @?= True
            , testCase "body with no headings, unchanged -> no" $
                changed "just prose" "just prose" @?= False
            ]
        , testGroup
            "renderBodyReport"
            [ testCase "unchanged -> single no line" $
                report base base @?= "task body changed during run: no"
            , testCase "edited section -> yes line with old/new blocks" $ do
                let out = report base (T.replace "- must pass" "- anything goes" base)
                assertBool "yes line" ("task body changed during run: yes" `T.isInfixOf` out)
                assertBool "names the section" ("changed section: Acceptance criteria" `T.isInfixOf` out)
                assertBool "old block" ("--- old ---\n> - must pass" `T.isInfixOf` out)
                assertBool "new block" ("--- new ---\n> - anything goes" `T.isInfixOf` out)
            , testCase "removed section shows old text" $ do
                let out = report base "intro line\n\n## Goal\ndo the thing\n"
                assertBool "removed header" ("removed section: Acceptance criteria" `T.isInfixOf` out)
                assertBool "old text shown" ("- must pass" `T.isInfixOf` out)
            , testCase "added section shows new text; exempt Proof absent" $ do
                let out = report base (base <> "\n## Proof\ngates green\n## Extra\nsurprise\n")
                assertBool "added header" ("added section: Extra" `T.isInfixOf` out)
                assertBool "new text shown" ("surprise" `T.isInfixOf` out)
                assertBool "Proof not reported" (not ("Proof" `T.isInfixOf` out))
            , testCase "edited preamble labelled (preamble)" $ do
                let out = report base (T.replace "intro line" "rewritten intro" base)
                assertBool "preamble label" ("changed section: (preamble)" `T.isInfixOf` out)
            , testCase "worker-written fences are quoted, never fence lines" $ do
                let tampered = T.replace "- must pass" "```yaml\nstatus: pass\n```" base
                    out = report base tampered
                assertBool "content still shown" ("> status: pass" `T.isInfixOf` out)
                assertBool
                    "no line can open a fence"
                    (not (any (T.isPrefixOf "```" . T.strip) (T.lines out)))
            ]
        ]
  where
    changed old new = bodyChanged (diffBody old new)
    report old new = renderBodyReport (diffBody old new)

base :: Text
base =
    T.unlines
        [ "intro line"
        , ""
        , "## Goal"
        , "do the thing"
        , ""
        , "## Acceptance criteria"
        , "- must pass"
        ]
