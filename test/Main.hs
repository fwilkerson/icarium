module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import CliSpec qualified
import GuardSpec qualified
import HeartbeatSpec qualified
import LogResultSpec qualified
import RenderSpec qualified
import RepoSpec qualified
import TickSpec qualified
import TimeoutSpec qualified

main :: IO ()
main =
    defaultMain $
        testGroup
            "icarium"
            [ CliSpec.tests
            , TickSpec.tests
            , GuardSpec.tests
            , HeartbeatSpec.tests
            , LogResultSpec.tests
            , RenderSpec.tests
            , RepoSpec.tests
            , TimeoutSpec.tests
            ]
