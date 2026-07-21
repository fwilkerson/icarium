module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import BodiesSpec qualified
import BodyDiffSpec qualified
import CliSpec qualified
import GitSpec qualified
import GuardSpec qualified
import HeartbeatSpec qualified
import IdSpec qualified
import LogResultSpec qualified
import NodeSpec qualified
import PayloadSpec qualified
import PostClaudeSpec qualified
import RenderSpec qualified
import RepoSpec qualified
import ReviewerSpec qualified
import TickSpec qualified
import TimeoutSpec qualified

main :: IO ()
main =
    defaultMain $
        testGroup
            "icarium"
            [ BodiesSpec.tests
            , BodyDiffSpec.tests
            , CliSpec.tests
            , GitSpec.tests
            , TickSpec.tests
            , GuardSpec.tests
            , HeartbeatSpec.tests
            , IdSpec.tests
            , LogResultSpec.tests
            , NodeSpec.tests
            , PayloadSpec.tests
            , PostClaudeSpec.tests
            , RenderSpec.tests
            , RepoSpec.tests
            , ReviewerSpec.tests
            , TimeoutSpec.tests
            ]
