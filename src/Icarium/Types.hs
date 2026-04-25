module Icarium.Types
    ( -- * Enums
      TaskState(..), taskStateText, parseTaskState
    , Effort(..), effortText, parseEffort
    , EdgeKind(..), edgeKindText, parseEdgeKind
    , NodeKind(..), nodeKindText, parseNodeKind
    , CategoryAxis(..), categoryAxisText, parseCategoryAxis
    , DispatchOutcome(..), dispatchOutcomeText, parseDispatchOutcome

      -- * Records
    , Task(..)
    , Knowledge(..)
    , Edge(..)
    , Category(..)
    , Dispatch(..)
    ) where

import           Data.Aeson                       (FromJSON (..), ToJSON (..), object, withObject,
                                                   withText, (.:), (.:?), (.=))
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Typeable                    (Typeable)
import           Database.SQLite.Simple           (FromRow (..), field)
import           Database.SQLite.Simple.FromField (Field, FromField (..), ResultError (..),
                                                   returnError)
import           Database.SQLite.Simple.Ok        (Ok)
import           Database.SQLite.Simple.ToField   (ToField (..))

-- =============================================================
-- Enums
-- =============================================================

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

data Effort = Low | Medium | High deriving (Show, Eq)

effortText :: Effort -> Text
effortText = \case Low -> "low"; Medium -> "medium"; High -> "high"

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
    DependsOn   -> "depends_on"
    References  -> "references"
    DerivedFrom -> "derived_from"
    Supersedes  -> "supersedes"

parseEdgeKind :: Text -> Maybe EdgeKind
parseEdgeKind = \case
    "depends_on"   -> Just DependsOn
    "references"   -> Just References
    "derived_from" -> Just DerivedFrom
    "supersedes"   -> Just Supersedes
    _              -> Nothing

data NodeKind = TaskNode | KnowledgeNode deriving (Show, Eq)

nodeKindText :: NodeKind -> Text
nodeKindText TaskNode      = "task"
nodeKindText KnowledgeNode = "knowledge"

parseNodeKind :: Text -> Maybe NodeKind
parseNodeKind = \case
    "task"      -> Just TaskNode
    "knowledge" -> Just KnowledgeNode
    _           -> Nothing

data CategoryAxis = Domain | Discipline deriving (Show, Eq)

categoryAxisText :: CategoryAxis -> Text
categoryAxisText Domain     = "domain"
categoryAxisText Discipline = "discipline"

parseCategoryAxis :: Text -> Maybe CategoryAxis
parseCategoryAxis = \case
    "domain"     -> Just Domain
    "discipline" -> Just Discipline
    _            -> Nothing

-- =============================================================
-- FromField / ToField for enums
-- =============================================================

enumFromField :: Typeable a => String -> (Text -> Maybe a) -> Field -> Ok a
enumFromField label p f = do
    t <- fromField f
    case p t of
        Just x  -> pure x
        Nothing -> returnError ConversionFailed f ("invalid " <> label <> ": " <> T.unpack t)

instance FromField TaskState    where fromField = enumFromField "task state"    parseTaskState
instance FromField Effort       where fromField = enumFromField "effort"        parseEffort
instance FromField EdgeKind     where fromField = enumFromField "edge kind"     parseEdgeKind
instance FromField NodeKind     where fromField = enumFromField "node kind"     parseNodeKind
instance FromField CategoryAxis where fromField = enumFromField "category axis" parseCategoryAxis

instance ToField TaskState    where toField = toField . taskStateText
instance ToField Effort       where toField = toField . effortText
instance ToField EdgeKind     where toField = toField . edgeKindText
instance ToField NodeKind     where toField = toField . nodeKindText
instance ToField CategoryAxis where toField = toField . categoryAxisText

-- =============================================================
-- Records (FromRow column order matches SELECT ordering used in Repo)
-- =============================================================

data Task = Task
    { taskId          :: Text
    , taskTitle       :: Text
    , taskBody        :: Text
    , taskState       :: TaskState
    , taskPriority    :: Maybe Int
    , taskBlockReason :: Maybe Text
    , taskCreatedAt   :: Text
    , taskUpdatedAt   :: Text
    } deriving (Show)

instance FromRow Task where
    fromRow = Task <$> field <*> field <*> field <*> field
                   <*> field <*> field <*> field <*> field

data Knowledge = Knowledge
    { knowledgeId        :: Text
    , knowledgeTitle     :: Text
    , knowledgeBody      :: Text
    , knowledgeStale     :: Bool
    , knowledgeCreatedAt :: Text
    , knowledgeUpdatedAt :: Text
    } deriving (Show)

instance FromRow Knowledge where
    fromRow = Knowledge <$> field <*> field <*> field
                        <*> (intToBool <$> field)
                        <*> field <*> field
      where
        intToBool :: Int -> Bool
        intToBool 0 = False
        intToBool _ = True

data Edge = Edge
    { edgeId        :: Text
    , edgeKind      :: EdgeKind
    , edgeSrcKind   :: NodeKind
    , edgeSrcId     :: Text
    , edgeDstKind   :: NodeKind
    , edgeDstId     :: Text
    , edgeCreatedAt :: Text
    } deriving (Show)

instance FromRow Edge where
    fromRow = Edge <$> field <*> field <*> field <*> field
                   <*> field <*> field <*> field

data Category = Category
    { categoryId   :: Text
    , categoryAxis :: CategoryAxis
    , categoryName :: Text
    } deriving (Show)

instance FromRow Category where
    fromRow = Category <$> field <*> field <*> field

data DispatchOutcome = OSuccess | OFailure | OInterrupted deriving (Show, Eq)

dispatchOutcomeText :: DispatchOutcome -> Text
dispatchOutcomeText = \case
    OSuccess     -> "success"
    OFailure     -> "failure"
    OInterrupted -> "interrupted"

parseDispatchOutcome :: Text -> Maybe DispatchOutcome
parseDispatchOutcome = \case
    "success"     -> Just OSuccess
    "failure"     -> Just OFailure
    "interrupted" -> Just OInterrupted
    _             -> Nothing

instance FromField DispatchOutcome where
    fromField = enumFromField "dispatch outcome" parseDispatchOutcome
instance ToField DispatchOutcome where
    toField = toField . dispatchOutcomeText

-- | A row in the @dispatches@ table. Columns kept in the order the
-- schema declares them so @FromRow@ lines up with @SELECT *@.
data Dispatch = Dispatch
    { dispatchId         :: Text
    , dispatchTaskId     :: Text
    , dispatchBranch     :: Text
    , dispatchBaseBranch :: Text
    , dispatchBaseSha    :: Text
    , dispatchPid        :: Maybe Int
    , dispatchModel      :: Text
    , dispatchEffort     :: Effort
    , dispatchStartedAt  :: Text
    , dispatchHeartbeat  :: Text
    , dispatchEndedAt    :: Maybe Text
    , dispatchOutcome    :: Maybe DispatchOutcome
    , dispatchMergeSha   :: Maybe Text
    , dispatchLastCommit :: Maybe Text
    , dispatchNotes      :: Maybe Text
    , dispatchLogPath    :: Maybe Text
    } deriving (Show)

instance FromRow Dispatch where
    fromRow = Dispatch <$> field <*> field <*> field <*> field
                       <*> field <*> field <*> field <*> field
                       <*> field <*> field <*> field <*> field
                       <*> field <*> field <*> field <*> field

-- =============================================================
-- ToJSON instances (column names match DB snake_case names)
-- =============================================================

instance ToJSON Task where
    toJSON Task{..} = object
        [ "id"           .= taskId
        , "title"        .= taskTitle
        , "body"         .= taskBody
        , "state"        .= taskStateText taskState
        , "priority"     .= taskPriority
        , "block_reason" .= taskBlockReason
        , "created_at"   .= taskCreatedAt
        , "updated_at"   .= taskUpdatedAt
        ]

instance ToJSON Knowledge where
    toJSON Knowledge{..} = object
        [ "id"         .= knowledgeId
        , "title"      .= knowledgeTitle
        , "body"       .= knowledgeBody
        , "stale"      .= knowledgeStale
        , "created_at" .= knowledgeCreatedAt
        , "updated_at" .= knowledgeUpdatedAt
        ]

instance ToJSON Edge where
    toJSON Edge{..} = object
        [ "id"         .= edgeId
        , "kind"       .= edgeKindText edgeKind
        , "src_kind"   .= nodeKindText edgeSrcKind
        , "src_id"     .= edgeSrcId
        , "dst_kind"   .= nodeKindText edgeDstKind
        , "dst_id"     .= edgeDstId
        , "created_at" .= edgeCreatedAt
        ]

instance ToJSON Category where
    toJSON Category{..} = object
        [ "id"   .= categoryId
        , "axis" .= categoryAxisText categoryAxis
        , "name" .= categoryName
        ]

instance ToJSON Dispatch where
    toJSON Dispatch{..} = object
        [ "id"           .= dispatchId
        , "task_id"      .= dispatchTaskId
        , "branch"       .= dispatchBranch
        , "base_branch"  .= dispatchBaseBranch
        , "base_sha"     .= dispatchBaseSha
        , "pid"          .= dispatchPid
        , "model"        .= dispatchModel
        , "effort"       .= effortText dispatchEffort
        , "started_at"   .= dispatchStartedAt
        , "heartbeat_at" .= dispatchHeartbeat
        , "ended_at"     .= dispatchEndedAt
        , "outcome"      .= fmap dispatchOutcomeText dispatchOutcome
        , "merge_sha"    .= dispatchMergeSha
        , "last_commit"  .= dispatchLastCommit
        , "notes"        .= dispatchNotes
        , "log_path"     .= dispatchLogPath
        ]

-- =============================================================
-- FromJSON instances (mirror the ToJSON field names above)
-- =============================================================

parseEnum :: String -> (Text -> Maybe a) -> Text -> Either String a
parseEnum label p t = maybe (Left $ "invalid " <> label <> ": " <> T.unpack t) Right (p t)

instance FromJSON TaskState where
    parseJSON = withText "TaskState" $ \t ->
        either fail pure (parseEnum "task state" parseTaskState t)

instance FromJSON Effort where
    parseJSON = withText "Effort" $ \t ->
        either fail pure (parseEnum "effort" parseEffort t)

instance FromJSON EdgeKind where
    parseJSON = withText "EdgeKind" $ \t ->
        either fail pure (parseEnum "edge kind" parseEdgeKind t)

instance FromJSON NodeKind where
    parseJSON = withText "NodeKind" $ \t ->
        either fail pure (parseEnum "node kind" parseNodeKind t)

instance FromJSON CategoryAxis where
    parseJSON = withText "CategoryAxis" $ \t ->
        either fail pure (parseEnum "category axis" parseCategoryAxis t)

instance FromJSON DispatchOutcome where
    parseJSON = withText "DispatchOutcome" $ \t ->
        either fail pure (parseEnum "dispatch outcome" parseDispatchOutcome t)

instance FromJSON Task where
    parseJSON = withObject "Task" $ \o -> Task
        <$> o .:  "id"
        <*> o .:  "title"
        <*> o .:  "body"
        <*> o .:  "state"
        <*> o .:? "priority"
        <*> o .:? "block_reason"
        <*> o .:  "created_at"
        <*> o .:  "updated_at"

instance FromJSON Knowledge where
    parseJSON = withObject "Knowledge" $ \o -> Knowledge
        <$> o .:  "id"
        <*> o .:  "title"
        <*> o .:  "body"
        <*> o .:  "stale"
        <*> o .:  "created_at"
        <*> o .:  "updated_at"

instance FromJSON Edge where
    parseJSON = withObject "Edge" $ \o -> Edge
        <$> o .:  "id"
        <*> o .:  "kind"
        <*> o .:  "src_kind"
        <*> o .:  "src_id"
        <*> o .:  "dst_kind"
        <*> o .:  "dst_id"
        <*> o .:  "created_at"

instance FromJSON Category where
    parseJSON = withObject "Category" $ \o -> Category
        <$> o .: "id"
        <*> o .: "axis"
        <*> o .: "name"

instance FromJSON Dispatch where
    parseJSON = withObject "Dispatch" $ \o -> Dispatch
        <$> o .:  "id"
        <*> o .:  "task_id"
        <*> o .:  "branch"
        <*> o .:  "base_branch"
        <*> o .:  "base_sha"
        <*> o .:? "pid"
        <*> o .:  "model"
        <*> o .:  "effort"
        <*> o .:  "started_at"
        <*> o .:  "heartbeat_at"
        <*> o .:? "ended_at"
        <*> o .:? "outcome"
        <*> o .:? "merge_sha"
        <*> o .:? "last_commit"
        <*> o .:? "notes"
        <*> o .:? "log_path"
