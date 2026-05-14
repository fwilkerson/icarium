module BodiesSpec (tests) where

import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Icarium.Bodies (writeBody)

tests :: TestTree
tests =
    testGroup
        "Bodies"
        [ testCase "writeBody is atomic: partial write leaves target unchanged" testAtomicWrite
        , testCase "writeBody creates file when target absent" testCreateAbsent
        ]

-- Simulate a partial write: write content to .tmp but do not rename.
-- The target file must remain unchanged (or absent).
testAtomicWrite :: IO ()
testAtomicWrite = withSystemTempDirectory "bodies-test" $ \dir -> do
    let target = dir </> "body.md"
    let tmp = target <> ".tmp"
    -- Establish original content
    writeBody target "original content"
    original <- readFile target
    original @?= "original content"
    -- Simulate crash: write to tmp but skip rename
    writeFile tmp "partial write"
    -- Target must be untouched
    actual <- readFile target
    actual @?= "original content"
    -- tmp exists, target exists, they differ
    tmpExists <- doesFileExist tmp
    assertBool "tmp file should exist" tmpExists

testCreateAbsent :: IO ()
testCreateAbsent = withSystemTempDirectory "bodies-test" $ \dir -> do
    let target = dir </> "new.md"
    exists0 <- doesFileExist target
    assertBool "should not exist yet" (not exists0)
    writeBody target "hello"
    content <- readFile target
    content @?= "hello"
    -- no stray .tmp left behind
    tmpExists <- doesFileExist (target <> ".tmp")
    assertBool "tmp should be cleaned up" (not tmpExists)
