module Main (main) where

import           Test.Tasty (defaultMain, testGroup)

import qualified CliSpec
import qualified GuardSpec
import qualified RenderSpec
import qualified RepoSpec
import qualified TickSpec

main :: IO ()
main = defaultMain $ testGroup "icarium"
    [ CliSpec.tests
    , TickSpec.tests
    , GuardSpec.tests
    , RenderSpec.tests
    , RepoSpec.tests
    ]
