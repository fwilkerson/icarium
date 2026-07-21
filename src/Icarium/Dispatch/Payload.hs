{- | The structured return payloads dispatch participants hand back, and the
JSON Schemas that constrain them.

Two roots, not one. A shared root would have to make every worker-only and
reviewer-only field optional, which destroys the required-when-blocked
guarantee that is the point; worker and reviewer are separate @claude -p@
invocations anyway. The builder combinators are shared, the root object is not.

Constrained decoding guarantees /shape/, never /content/. Users may override
the working agreement and the reviewer prompt, but icarium owns
@--json-schema@ — so "what belongs in this field" lives in each property's
@description@, the one channel an override cannot weaken.

Per ADR 0008 a participant reports what it observed and icarium decides what it
means: the worker reports a submission, not a success, and the reviewer reports
findings with no verdict ('verdictFromFindings' derives it).
-}
module Icarium.Dispatch.Payload (
    -- * Schemas
    Schema,
    workerSchema,
    reviewerSchema,
    jsonSchemaArgs,

    -- * Worker payload
    WorkerStatus (..),
    workerStatusText,
    FutureNote (..),
    WorkerPayload (..),
    decodeWorkerPayload,

    -- * Reviewer payload
    FindingAxis (..),
    Severity (..),
    Finding (..),
    ReviewerPayload (..),
    decodeReviewerPayload,
    verdictFromFindings,
) where

import Data.Aeson (FromJSON (..), Value, eitherDecodeStrict, withObject, withText, (.:), (.:?))
import Data.Aeson.Encoding (Encoding, Series)
import Data.Aeson.Encoding qualified as E
import Data.Aeson.Key qualified as K
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Icarium.Types (ReviewVerdict (..))

-- =============================================================
-- Builder combinators
-- =============================================================

-- | A JSON Schema fragment, in the serialized form that lands in argv.
type Schema = Encoding

{- | A JSON Schema @if@/@then@ pair: when @rwProp@ equals @rwEquals@, the
listed properties become required. Verified to be enforced by constrained
decoding, not merely accepted — so no Haskell-side check mirrors it.
-}
data RequiredWhen = RequiredWhen
    { rwProp :: Text
    , rwEquals :: Text
    , rwRequires :: [Text]
    }

data Prop = Prop
    { pName :: Text
    , pRequired :: Bool
    , pSchema :: Schema
    }

req :: Text -> Schema -> Prop
req name = Prop name True

opt :: Text -> Schema -> Prop
opt name = Prop name False

{- | Closed object: an unlisted property is rejected rather than passed
through, so a hallucinated field fails decoding instead of reaching the gate.
The required list is derived from the props, not restated beside them.
-}
objectS :: [Prop] -> Maybe RequiredWhen -> Schema
objectS props mWhen =
    E.pairs $
        E.pair "type" (E.text "object")
            <> E.pair "additionalProperties" (E.bool False)
            <> E.pair "required" (E.list E.text [pName p | p <- props, pRequired p])
            <> E.pair "properties" (E.pairs (foldMap prop props))
            <> maybe mempty conditional mWhen
  where
    prop p = E.pair (K.fromText (pName p)) (pSchema p)
    conditional RequiredWhen{..} =
        E.pair "if" (wrap "properties" (wrap rwProp (wrap "const" (E.text rwEquals))))
            <> E.pair "then" (wrap "required" (E.list E.text rwRequires))
      where
        wrap k v = E.pairs (E.pair (K.fromText k) v)

stringS :: Text -> Schema
stringS desc = E.pairs (E.pair "type" (E.text "string") <> described desc)

enumS :: Text -> [Text] -> Schema
enumS desc values = E.pairs (E.pair "enum" (E.list E.text values) <> described desc)

arrayS :: Text -> Schema -> Schema
arrayS desc items = E.pairs (E.pair "type" (E.text "array") <> E.pair "items" items <> described desc)

described :: Text -> Series
described = E.pair "description" . E.text

-- | The @--json-schema@ flag takes inline JSON, not a path (CLI 2.1.216).
jsonSchemaArgs :: Schema -> [Text]
jsonSchemaArgs s =
    ["--json-schema", TE.decodeUtf8 (BL.toStrict (E.encodingToLazyByteString s))]

-- =============================================================
-- Worker schema
-- =============================================================

workerSchema :: Schema
workerSchema =
    objectS
        [ req "status" statusS
        , opt "block_reason" blockReasonS
        , req "for_future_agents" forFutureAgentsS
        ]
        (Just (RequiredWhen "status" "blocked" ["block_reason"]))
  where
    statusS =
        enumS
            "What you are handing back. `submitted`: you did the work and \
            \committed it. It claims nothing about whether the work is \
            \acceptable -- gates and a reviewer run after you, and they decide. \
            \`blocked`: you could not proceed within policy and are stopping."
            ["submitted", "blocked"]
    blockReasonS =
        stringS
            "Why you cannot proceed, specific enough that a human reading only \
            \this line knows what to change. Required when status is `blocked`."
    forFutureAgentsS =
        arrayS
            "Durable knowledge a future agent working in this area would need, \
            \written for someone who was not here. Return `[]` if you learned \
            \nothing that outlives this task -- that is the common case and is \
            \expected. Do not restate the task, narrate what you did, or record \
            \anything already true in the repo's docs."
            noteS
    noteS =
        objectS
            [ req "title" (stringS "One line naming the thing that is now known.")
            , req "body" (stringS "Markdown. The constraint, invariant, or gotcha itself, plus enough of why for a reader to know when it stops applying.")
            ]
            Nothing

-- =============================================================
-- Reviewer schema
-- =============================================================

reviewerSchema :: Schema
reviewerSchema =
    objectS
        [req "findings" findingsS]
        Nothing
  where
    findingsS =
        arrayS
            "Every issue you found, including ones you are uncertain about or \
            \consider minor -- attach a severity rather than withholding. \
            \Return `[]` when the diff is faithful to the task and consistent \
            \with the repo's standards; that is the pass case."
            findingS
    findingS =
        objectS
            [ req "axis" axisS
            , req "severity" severityS
            , opt "file" fileS
            , req "message" messageS
            ]
            (Just (RequiredWhen "axis" "standards" ["file"]))
    axisS =
        enumS
            "`spec`: the diff does not faithfully implement the task body \
            \(missing, wrong, or unasked-for behaviour). `standards`: the code \
            \breaks a standard this repo documents."
            ["spec", "standards"]
    severityS =
        enumS
            "`warn`: a concern worth recording that does not have to be fixed \
            \before the branch lands -- judgement calls belong here. `fail`: \
            \must be fixed before merge."
            ["warn", "fail"]
    fileS =
        stringS
            "Repo-relative path the finding is about. Required on `standards` \
            \findings, which are always in something. Omit it on a `spec` \
            \finding about an absence rather than inventing a plausible path."
    messageS =
        stringS
            "What is wrong, in one or two sentences. On `spec`, quote the task \
            \line it is about; on `standards`, cite the documented rule."

-- =============================================================
-- Worker payload
-- =============================================================

data WorkerStatus = WSubmitted | WBlocked deriving (Show, Eq)

parseWorkerStatus :: Text -> Maybe WorkerStatus
parseWorkerStatus = \case
    "submitted" -> Just WSubmitted
    "blocked" -> Just WBlocked
    _ -> Nothing

workerStatusText :: WorkerStatus -> Text
workerStatusText = \case
    WSubmitted -> "submitted"
    WBlocked -> "blocked"

instance FromJSON WorkerStatus where
    parseJSON = enumFromJSON "worker status" parseWorkerStatus

data FutureNote = FutureNote
    { fnTitle :: Text
    , fnBody :: Text
    }
    deriving (Show, Eq)

instance FromJSON FutureNote where
    parseJSON = withObject "FutureNote" $ \o ->
        FutureNote <$> o .: "title" <*> o .: "body"

data WorkerPayload = WorkerPayload
    { wpStatus :: WorkerStatus
    , wpBlockReason :: Maybe Text
    , wpForFutureAgents :: [FutureNote]
    }
    deriving (Show, Eq)

instance FromJSON WorkerPayload where
    parseJSON = withObject "WorkerPayload" $ \o ->
        WorkerPayload
            <$> o .: "status"
            <*> o .:? "block_reason"
            <*> o .: "for_future_agents"

decodeWorkerPayload :: Text -> Either Text WorkerPayload
decodeWorkerPayload = decodePayload

-- =============================================================
-- Reviewer payload
-- =============================================================

data FindingAxis = AxisSpec | AxisStandards deriving (Show, Eq)

parseFindingAxis :: Text -> Maybe FindingAxis
parseFindingAxis = \case
    "spec" -> Just AxisSpec
    "standards" -> Just AxisStandards
    _ -> Nothing

instance FromJSON FindingAxis where
    parseJSON = enumFromJSON "finding axis" parseFindingAxis

-- | Ordered worst-last: 'verdictFromFindings' takes the 'maximum'.
data Severity = SevWarn | SevFail deriving (Show, Eq, Ord)

parseSeverity :: Text -> Maybe Severity
parseSeverity = \case
    "warn" -> Just SevWarn
    "fail" -> Just SevFail
    _ -> Nothing

instance FromJSON Severity where
    parseJSON = enumFromJSON "severity" parseSeverity

data Finding = Finding
    { findingAxis :: FindingAxis
    , findingSeverity :: Severity
    , findingFile :: Maybe Text
    , findingMessage :: Text
    }
    deriving (Show, Eq)

instance FromJSON Finding where
    parseJSON = withObject "Finding" $ \o ->
        Finding
            <$> o .: "axis"
            <*> o .: "severity"
            <*> o .:? "file"
            <*> o .: "message"

newtype ReviewerPayload = ReviewerPayload {rpFindings :: [Finding]}
    deriving (Show, Eq)

instance FromJSON ReviewerPayload where
    parseJSON = withObject "ReviewerPayload" $ \o ->
        ReviewerPayload <$> o .: "findings"

decodeReviewerPayload :: Text -> Either Text ReviewerPayload
decodeReviewerPayload = decodePayload

{- | The verdict is the worst severity among the findings; no findings is a
pass (ADR 0008). Process-level fail-closed posture -- reviewer timeout or
non-zero exit -- is separate and lives in "Icarium.Dispatch.Reviewer".
-}
verdictFromFindings :: [Finding] -> ReviewVerdict
verdictFromFindings [] = RVPass
verdictFromFindings fs = case maximum (map findingSeverity fs) of
    SevWarn -> RVWarn
    SevFail -> RVFail

decodePayload :: (FromJSON a) => Text -> Either Text a
decodePayload t =
    case eitherDecodeStrict (TE.encodeUtf8 t) of
        Left e -> Left (T.pack e)
        Right v -> Right v

{- | Mirrors 'Icarium.Types.enumFromField': one shared failure message, a
plain @Text -> Maybe a@ table per enum.
-}
enumFromJSON :: String -> (Text -> Maybe a) -> Value -> Parser a
enumFromJSON label p = withText label $ \t ->
    maybe (fail ("unknown " <> label <> ": " <> T.unpack t)) pure (p t)
