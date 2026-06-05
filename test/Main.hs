module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import BodiesSpec qualified
import CliSpec qualified
import GuardSpec qualified
import HeartbeatSpec qualified
import LogResultSpec qualified
import NodeSpec qualified
import PostClaudeSpec qualified
import RenderSpec qualified
import RepoSpec qualified
import TickSpec qualified
import TimeoutSpec qualified

main :: IO ()
main =
    defaultMain $
        testGroup
            "icarium"
            [ BodiesSpec.tests
            , CliSpec.tests
            , TickSpec.tests
            , GuardSpec.tests
            , HeartbeatSpec.tests
            , LogResultSpec.tests
            , NodeSpec.tests
            , PostClaudeSpec.tests
            , RenderSpec.tests
            , RepoSpec.tests
            , TimeoutSpec.tests
            ]
