module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import BodiesSpec qualified
import BodyDiffSpec qualified
import CategoriesSpec qualified
import CliSpec qualified
import ContextSpec qualified
import DecideSpec qualified
import DispatchRepoSpec qualified
import DrainSpec qualified
import EventsSpec qualified
import GateSpec qualified
import GitSpec qualified
import HeartbeatSpec qualified
import IdSpec qualified
import LogResultSpec qualified
import NodeSpec qualified
import PayloadSpec qualified
import PostClaudeSpec qualified
import PromptSpec qualified
import QueueSpec qualified
import RenderSpec qualified
import RepoSpec qualified
import ResolverSpec qualified
import RoutingSpec qualified
import SchemaSpec qualified
import SearchSpec qualified
import TickSpec qualified
import TimeoutSpec qualified
import WorktreeSpec qualified

main :: IO ()
main =
    defaultMain $
        testGroup
            "icarium"
            [ BodiesSpec.tests
            , BodyDiffSpec.tests
            , CategoriesSpec.tests
            , CliSpec.tests
            , DecideSpec.tests
            , DrainSpec.tests
            , EventsSpec.tests
            , GateSpec.tests
            , GitSpec.tests
            , TickSpec.tests
            , HeartbeatSpec.tests
            , IdSpec.tests
            , LogResultSpec.tests
            , NodeSpec.tests
            , PayloadSpec.tests
            , PostClaudeSpec.tests
            , PromptSpec.tests
            , RenderSpec.tests
            , RepoSpec.tests
            , RoutingSpec.tests
            , SchemaSpec.tests
            , ResolverSpec.tests
            , SearchSpec.tests
            , ContextSpec.tests
            , QueueSpec.tests
            , DispatchRepoSpec.tests
            , TimeoutSpec.tests
            , WorktreeSpec.tests
            ]
