module Icarium.Types
    ( TaskState(..), taskStateText, parseTaskState
    , Effort(..), effortText, parseEffort
    , EdgeKind(..), edgeKindText, parseEdgeKind
    , NodeKind(..), nodeKindText, parseNodeKind
    ) where

import Data.Text (Text)

data TaskState = Idea | Planned | Ready | Done | Blocked | Abandoned
    deriving (Show, Eq)

taskStateText :: TaskState -> Text
taskStateText = \case
    Idea      -> "idea"
    Planned   -> "planned"
    Ready     -> "ready"
    Done      -> "done"
    Blocked   -> "blocked"
    Abandoned -> "abandoned"

parseTaskState :: Text -> Maybe TaskState
parseTaskState = \case
    "idea"      -> Just Idea
    "planned"   -> Just Planned
    "ready"     -> Just Ready
    "done"      -> Just Done
    "blocked"   -> Just Blocked
    "abandoned" -> Just Abandoned
    _           -> Nothing

data Effort = Low | Medium | High
    deriving (Show, Eq)

effortText :: Effort -> Text
effortText = \case
    Low    -> "low"
    Medium -> "medium"
    High   -> "high"

parseEffort :: Text -> Maybe Effort
parseEffort = \case
    "low"    -> Just Low
    "medium" -> Just Medium
    "high"   -> Just High
    _        -> Nothing

data EdgeKind = DependsOn | References | DerivedFrom | Supersedes
    deriving (Show, Eq)

edgeKindText :: EdgeKind -> Text
edgeKindText = \case
    DependsOn    -> "depends_on"
    References   -> "references"
    DerivedFrom  -> "derived_from"
    Supersedes   -> "supersedes"

parseEdgeKind :: Text -> Maybe EdgeKind
parseEdgeKind = \case
    "depends_on"   -> Just DependsOn
    "references"   -> Just References
    "derived_from" -> Just DerivedFrom
    "supersedes"   -> Just Supersedes
    _              -> Nothing

data NodeKind = TaskNode | KnowledgeNode
    deriving (Show, Eq)

nodeKindText :: NodeKind -> Text
nodeKindText TaskNode      = "task"
nodeKindText KnowledgeNode = "knowledge"

parseNodeKind :: Text -> Maybe NodeKind
parseNodeKind = \case
    "task"      -> Just TaskNode
    "knowledge" -> Just KnowledgeNode
    _           -> Nothing
