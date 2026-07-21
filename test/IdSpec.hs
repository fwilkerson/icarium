module IdSpec (tests) where

import Control.Monad (replicateM)
import Data.List (sort)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Id (encodeUlid, newId)

tests :: TestTree
tests =
    testGroup
        "Id"
        [ testGroup "encodeUlid" testEncodeUlid
        , testGroup "newId" testNewId
        ]

-- Expected values derived from the ULID spec, not from this implementation.
-- "7ZZZ..." is the spec's documented maximum ULID; "01ARYZ6S41" is the
-- timestamp prefix the spec's README gives for 1469918176385.
testEncodeUlid :: [TestTree]
testEncodeUlid =
    [ testCase "all-zero encodes to 26 zeros" $
        encodeUlid 0 0 @?= "00000000000000000000000000"
    , testCase "maximum value matches the spec's max ULID" $
        encodeUlid tsMax randMax @?= "7ZZZZZZZZZZZZZZZZZZZZZZZZZ"
    , testCase "known timestamp encodes to the spec's prefix" $
        T.take 10 (encodeUlid 1469918176385 0) @?= "01ARYZ6S41"
    , testCase "timestamp lands in the first 10 characters" $
        encodeUlid 1 0 @?= "00000000010000000000000000"
    , testCase "randomness lands in the last 16 characters" $
        encodeUlid 0 1 @?= "00000000000000000000000001"
    , testCase "always 26 characters" $
        let lengths =
                [ T.length (encodeUlid ms r)
                | ms <- [0, 1, 1469918176385, tsMax]
                , r <- [0, 1, 255, randMax]
                ]
         in assertBool "every encoding is 26 chars" (all (== 26) lengths)
    , testCase "only Crockford base32 characters are emitted" $
        let emitted = Set.fromList (T.unpack (encodeUlid tsMax randMax <> encodeUlid 0 0))
         in assertBool "no character outside the alphabet" (emitted `Set.isSubsetOf` alphabet)
    , testCase "alphabet excludes I, L, O and U" $
        assertBool "ambiguous letters absent" (not (any (`Set.member` alphabet) ("ILOU" :: String)))
    , -- Masking rather than rejecting is what keeps the output width fixed.
      testCase "timestamp above 48 bits wraps" $
        encodeUlid (tsMax + 1) 0 @?= encodeUlid 0 0
    , testCase "randomness above 80 bits wraps" $
        encodeUlid 0 (randMax + 1) @?= encodeUlid 0 0
    , testCase "encoding is lexicographically ordered by timestamp" $
        let encoded = [encodeUlid ms 0 | ms <- [0, 1, 4095, 1469918176385, tsMax]]
         in sort encoded @?= encoded
    , testCase "a later timestamp outsorts a larger random component" $
        assertBool "timestamp dominates" (encodeUlid 1 0 > encodeUlid 0 randMax)
    ]
  where
    alphabet = Set.fromList "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

testNewId :: [TestTree]
testNewId =
    [ testCase "generates a 26-character id" $ do
        eid <- newId
        T.length eid @?= 26
    , testCase "1000 ids are unique" $ do
        eids <- replicateM 1000 newId
        Set.size (Set.fromList eids) @?= 1000
    , testCase "ids share a timestamp prefix within a run" $ do
        -- Generated back to back, so the 48-bit ms field must agree on at
        -- least its high bits; this catches a timestamp wired up wrong.
        a <- newId
        b <- newId
        T.take 6 a @?= (T.take 6 b :: Text)
    ]

tsMax :: Integer
tsMax = 2 ^ (48 :: Int) - 1

randMax :: Integer
randMax = 2 ^ (80 :: Int) - 1
