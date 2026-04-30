module Main (main) where

import           Test.Tasty    (defaultMain, testGroup)

import qualified CliSpec
import qualified GuardSpec
import qualified HeartbeatSpec
import qualified RenderSpec
import qualified RepoSpec
import qualified TickSpec
import qualified TimeoutSpec

main :: IO ()
main = defaultMain $ testGroup "icarium"
    [ CliSpec.tests
    , TickSpec.tests
    , GuardSpec.tests
    , HeartbeatSpec.tests
    , RenderSpec.tests
    , RepoSpec.tests
    , TimeoutSpec.tests
    ]
