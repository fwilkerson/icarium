module EventsSpec (tests) where

import Data.Aeson (Value (..), decode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Events (Event (..), renderEvent)
import Icarium.Types (Disposition (..), TaskState (..))

tests :: TestTree
tests =
    testGroup
        "Events"
        [ testCase "a task transition renders as one flat JSON object" testTaskUpdated
        , testCase "an absent optional field is omitted, never null" testOmitsAbsentFields
        ]

at :: UTCTime
at = UTCTime (fromGregorian 2026 7 22) 3661

{- | A consumer distinguishes "no artifact" by the key being absent, so a
JSON @null@ would be a second spelling of the same thing.
-}
testOmitsAbsentFields :: IO ()
testOmitsAbsentFields = do
    let keys ev = case decode (renderEvent at "ctx curate" ev) of
            Just (Object o) -> KM.keys o
            other -> error ("expected a JSON object, got: " <> show other)
    keys (CtxCurated "01CTX" Keep Nothing)
        @?= keys (CtxCurated "01CTX" Keep (Just "x")) `without` "artifact"
  where
    without ks k = filter (/= k) ks

testTaskUpdated :: IO ()
testTaskUpdated = do
    let line = renderEvent at "task update" (TaskUpdated "01TASK" Planned Done)
    assertBool "single line" ('\n' `notElem` BLC.unpack line)
    decode line
        @?= Just
            ( Object $
                KM.fromList
                    [ ("ts", String "2026-07-22T01:01:01Z")
                    , ("actor", String "task update")
                    , ("event", String "task.updated")
                    , ("kind", String "task")
                    , ("id", String "01TASK")
                    , ("from", String "planned")
                    , ("to", String "done")
                    ]
            )
