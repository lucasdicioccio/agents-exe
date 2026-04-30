{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Core session types shared across the agent system.

This module contains the fundamental session types that need to be
accessible from multiple parts of the codebase, including tool execution
contexts. Keeping these types in a separate module helps avoid circular
dependencies between System.Agents.Session.Base and System.Agents.Tools.Context.
-}
module System.Agents.Session.Types (
    -- * Identifiers
    SessionId (..),
    TurnId (..),
    newSessionId,
    newTurnId,

    -- * Core types
    Session (..),
    Turn (..),
    UserTurnContent (..),
    LlmTurnContent (..),
    PartialUserTurnContent (..),

    -- * Execution mode
    ExecutionMode (..),

    -- * Async/Continuation types
    ContinuationToken (..),
    newContinuationToken,
    CacheKey (..),

    -- * Byte usage tracking
    StepByteUsage (..),
    calculateStepByteUsage,
    sessionTotalBytes,

    -- * Content types
    SystemPrompt (..),
    LlmResponse (..),
    LlmToolCall (..),
    UserQuery (..),
    UserToolResponse (..),

    -- * Tool definitions
    SystemTool (..),
    SystemToolDefinition (..),
    SystemToolDefinitionV1 (..),

    -- * Signal types (trajectory analysis)
    InteractionSignals (..),
    ExecutionSignals (..),
    EnvironmentSignals (..),
    TrajectorySignals (..),
    StepSignals (..),
    defaultInteractionSignals,
    defaultExecutionSignals,
    defaultEnvironmentSignals,
    defaultTrajectorySignals,

    -- * Session progress tracking
    SessionProgress (..),
    OnSessionProgress,
    ignoreSessionProgress,

    -- * Migration helpers
    migrateSessionV1ToV2,

    -- * Re-exports for convenience
    TokenUsage (..),
) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson.Types
import Data.Aeson.Types ((.!=))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)

import System.Agents.LLMs.OpenAI (TokenUsage (..))
import System.Agents.Media.Types (ContentPart, MediaAttachment (..))
import System.Agents.ToolSchema

-------------------------------------------------------------------------------
-- Identifiers
-------------------------------------------------------------------------------

newtype SessionId = SessionId UUID
    deriving (Show, Ord, Eq, FromJSON, ToJSON)

newSessionId :: IO SessionId
newSessionId =
    SessionId <$> UUID.nextRandom

newtype TurnId = TurnId UUID
    deriving (Show, Ord, Eq, FromJSON, ToJSON)

newTurnId :: IO TurnId
newTurnId =
    TurnId <$> UUID.nextRandom

-------------------------------------------------------------------------------
-- Async/Continuation Types
-------------------------------------------------------------------------------

{- | Unique token identifying a yielded tool call.

Used to resume a paused tool execution. Tokens are UUID-based for
global uniqueness across machines.
-}
newtype ContinuationToken = ContinuationToken UUID
    deriving (Show, Eq, Ord, Generic)

instance ToJSON ContinuationToken where
    toJSON (ContinuationToken uuid) = Aeson.toJSON $ UUID.toText uuid

instance FromJSON ContinuationToken where
    parseJSON val = do
        txt <- Aeson.parseJSON val
        case UUID.fromText txt of
            Just uuid -> pure $ ContinuationToken uuid
            Nothing -> fail "Invalid UUID for ContinuationToken"

-- | Generate a new unique continuation token.
newContinuationToken :: IO ContinuationToken
newContinuationToken = ContinuationToken <$> UUID.nextRandom

{- | Cache key derived from tool call.

The key uniquely identifies a tool call based on:
* Tool name (e.g., "bash", "sqlite_query")
* Arguments hash (normalized JSON representation)

This content-addressable approach ensures that identical tool calls
(with the same arguments) map to the same cache key.
-}
data CacheKey = CacheKey
    { ckToolName :: Text
    -- ^ The name of the tool (e.g., "bash_command")
    , ckArgumentsHash :: Text
    -- ^ Hash of normalized arguments
    }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON CacheKey where
    toJSON key =
        Aeson.object
            [ "toolName" .= key.ckToolName
            , "argumentsHash" .= key.ckArgumentsHash
            ]

instance FromJSON CacheKey where
    parseJSON = Aeson.withObject "CacheKey" $ \v ->
        CacheKey
            <$> v .: "toolName"
            <*> v .: "argumentsHash"

-------------------------------------------------------------------------------
-- Execution Mode
-------------------------------------------------------------------------------

{- | Execution mode for the agent.

Determines how tool calls are executed within a session:
* 'Synchronous' - All tool calls execute immediately (traditional behavior)
* 'Asynchronous' - Tool calls execute one at a time, allowing pausing/resuming
-}
data ExecutionMode
    = Synchronous
    | Asynchronous
    deriving (Show, Eq, Ord, Generic)

instance ToJSON ExecutionMode
instance FromJSON ExecutionMode

-------------------------------------------------------------------------------
-- Signal Types (Trajectory Analysis)
-------------------------------------------------------------------------------

{- | Interaction signals derived from user-assistant natural language discourse.

Based on: "Signals: Trajectory Sampling and Triage for Agentic Interactions"
(Chen et al., 2026)

These signals capture learning-oriented patterns in the conversation:
- Misalignment: Semantic/intent mismatches (rephrasing, corrections)
- Stagnation: Discourse without visible progress (near-duplicates, circular)
- Disengagement: Withdrawal of cooperative intent (exit requests, negative stance)
- Satisfaction: Successful convergence (gratitude, confirmations)
-}
data InteractionSignals = InteractionSignals
    { sigMisalignmentCount :: Int
    -- ^ Semantic/intent mismatches detected (rephrasing, corrections)
    , sigStagnationCount :: Int
    -- ^ Discourse without progress (near-duplicates, circular explanations)
    , sigDisengagementDetected :: Bool
    -- ^ User withdrawal markers detected (exit requests, negative stance)
    , sigSatisfactionDetected :: Bool
    -- ^ Success confirmation markers detected (gratitude, confirmations)
    }
    deriving (Show, Eq, Generic)

instance FromJSON InteractionSignals
instance ToJSON InteractionSignals

-- | Default empty interaction signals.
defaultInteractionSignals :: InteractionSignals
defaultInteractionSignals =
    InteractionSignals
        { sigMisalignmentCount = 0
        , sigStagnationCount = 0
        , sigDisengagementDetected = False
        , sigSatisfactionDetected = False
        }

{- | Execution signals derived from structured runtime events (tool calls, API responses).

These signals capture learning-oriented patterns in tool execution:
- Failure: Action attempts not yielding usable outcomes
- Loop: Repetitive execution without progress (retries, oscillations)
-}
data ExecutionSignals = ExecutionSignals
    { sigFailureCount :: Int
    -- ^ Tool/action attempts with non-advancing outcomes
    , sigLoopDetected :: Bool
    -- ^ Repetitive execution patterns without progress
    , sigLoopToolSequence :: [Text]
    -- ^ Tool names involved in detected loop (for diagnostics)
    }
    deriving (Show, Eq, Generic)

instance FromJSON ExecutionSignals
instance ToJSON ExecutionSignals

-- | Default empty execution signals.
defaultExecutionSignals :: ExecutionSignals
defaultExecutionSignals =
    ExecutionSignals
        { sigFailureCount = 0
        , sigLoopDetected = False
        , sigLoopToolSequence = []
        }

{- | Environment signals for system-level boundary conditions.

These signals capture diagnosis-oriented patterns:
- Exhaustion: Context overflows, rate limits, API failures, resource boundaries
-}
data EnvironmentSignals = EnvironmentSignals
    { sigExhaustionCount :: Int
    -- ^ Resource boundary hits (rate limits, context overflows)
    , sigExhaustionTypes :: [Text]
    -- ^ Types of exhaustion detected (e.g., "rate_limit", "context_overflow")
    }
    deriving (Show, Eq, Generic)

instance FromJSON EnvironmentSignals
instance ToJSON EnvironmentSignals

-- | Default empty environment signals.
defaultEnvironmentSignals :: EnvironmentSignals
defaultEnvironmentSignals =
    EnvironmentSignals
        { sigExhaustionCount = 0
        , sigExhaustionTypes = []
        }

{- | Combined trajectory signals for a session.

Aggregates all signal categories and provides a composite informativeness score.
Higher scores indicate trajectories more likely to contain actionable insights
for learning and improvement.
-}
data TrajectorySignals = TrajectorySignals
    { trajInteraction :: InteractionSignals
    , trajExecution :: ExecutionSignals
    , trajEnvironment :: EnvironmentSignals
    , trajInformativenessScore :: Int
    {- ^ Composite informativeness score (0-100)
    Higher = more likely to contain actionable insights
    -}
    }
    deriving (Show, Eq, Generic)

instance FromJSON TrajectorySignals
instance ToJSON TrajectorySignals

-- | Default empty trajectory signals.
defaultTrajectorySignals :: TrajectorySignals
defaultTrajectorySignals =
    TrajectorySignals
        { trajInteraction = defaultInteractionSignals
        , trajExecution = defaultExecutionSignals
        , trajEnvironment = defaultEnvironmentSignals
        , trajInformativenessScore = 0
        }

{- | Per-step signal tracking.

Stores signal information for individual turns, similar to how StepByteUsage
tracks per-turn byte usage. Uses Maybe for backward compatibility.
-}
data StepSignals = StepSignals
    { stepInteractionSignals :: Maybe InteractionSignals
    , stepExecutionSignals :: Maybe ExecutionSignals
    , stepEnvironmentSignals :: Maybe EnvironmentSignals
    }
    deriving (Show, Eq, Generic)

instance FromJSON StepSignals
instance ToJSON StepSignals

-------------------------------------------------------------------------------
-- Byte Usage Tracking
-------------------------------------------------------------------------------

{- | Byte usage breakdown for a single step.

Tracks the amount of data exchanged during a single turn of conversation,
broken down by category for cost transparency and debugging.

Also includes optional token usage from the LLM provider for accurate
billing and usage tracking.
-}
data StepByteUsage = StepByteUsage
    { stepTotalBytes :: Int
    -- ^ Total bytes for this step
    , stepInputBytes :: Int
    -- ^ Bytes in the input (prompt + context + query)
    , stepOutputBytes :: Int
    -- ^ Bytes in the LLM output response
    , stepReasoningBytes :: Int
    -- ^ Bytes in reasoning/thinking content (if model supports it)
    , stepToolBytes :: Int
    -- ^ Bytes in tool call responses
    , stepTokenUsage :: Maybe TokenUsage
    -- ^ Optional token usage from LLM provider for accurate billing
    }
    deriving (Show, Ord, Eq, Generic)

{- | Custom ToJSON for StepByteUsage that maintains backward compatibility.
Fields are only included if they have values (non-zero or Just).
-}
instance ToJSON StepByteUsage where
    toJSON usage =
        Aeson.object $
            [ "stepTotalBytes" .= usage.stepTotalBytes
            , "stepInputBytes" .= usage.stepInputBytes
            , "stepOutputBytes" .= usage.stepOutputBytes
            , "stepReasoningBytes" .= usage.stepReasoningBytes
            , "stepToolBytes" .= usage.stepToolBytes
            ]
                ++ ["stepTokenUsage" .= tu | Just tu <- [usage.stepTokenUsage]]

{- | Custom FromJSON for StepByteUsage that handles backward compatibility.
Missing fields are filled with default values (0 for Int, Nothing for Maybe).
-}
instance FromJSON StepByteUsage where
    parseJSON = Aeson.withObject "StepByteUsage" $ \v ->
        StepByteUsage
            <$> v .: "stepTotalBytes"
            <*> v .: "stepInputBytes"
            <*> v .: "stepOutputBytes"
            <*> v .: "stepReasoningBytes"
            <*> v .: "stepToolBytes"
            <*> v .:? "stepTokenUsage"

{- | Helper to calculate total bytes from components.
Use this when you have individual components but want to ensure
consistency between total and sum of parts.

Includes optional token usage from the LLM provider.
-}
calculateStepByteUsage :: Int -> Int -> Int -> Int -> Maybe TokenUsage -> StepByteUsage
calculateStepByteUsage input output reasoning tool mTokenUsage =
    StepByteUsage
        { stepTotalBytes = input + output + reasoning + tool
        , stepInputBytes = input
        , stepOutputBytes = output
        , stepReasoningBytes = reasoning
        , stepToolBytes = tool
        , stepTokenUsage = mTokenUsage
        }

{- | Calculate total bytes for an entire session.
Returns the sum of all step totals across all turns.
-}
sessionTotalBytes :: Session -> Int
sessionTotalBytes session =
    sum
        [ stepTotalBytes usage
        | turn <- session.turns
        , usage <- maybeToList (turnByteUsage turn)
        ]
  where
    maybeToList :: Maybe a -> [a]
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

    turnByteUsage :: Turn -> Maybe StepByteUsage
    turnByteUsage (UserTurn _ usage) = usage
    turnByteUsage (LlmTurn _ usage) = usage
    turnByteUsage (PartialUserTurn _ usage) = usage

-------------------------------------------------------------------------------
-- Core content types
-------------------------------------------------------------------------------

-- | A text prompt given.
newtype SystemPrompt = SystemPrompt Text
    deriving (Show, Ord, Eq, FromJSON, ToJSON)

-- | LLM response.
data LlmResponse = LlmResponse
    { responseText :: Maybe Text
    , responseThinking :: Maybe Text
    -- ^ Separate thinking/reasoning content from models like o1/o3 and Claude
    , rawResponse :: Aeson.Value
    , responseTokenUsage :: Maybe TokenUsage
    -- ^ Token usage from the LLM response for accurate billing
    }
    deriving (Show, Ord, Eq, Generic)

-- | Custom ToJSON for LlmResponse that maintains backward compatibility.
instance ToJSON LlmResponse where
    toJSON rsp =
        Aeson.object $
            [ "responseText" .= rsp.responseText
            , "responseThinking" .= rsp.responseThinking
            , "rawResponse" .= rsp.rawResponse
            ]
                ++ ["responseTokenUsage" .= tu | Just tu <- [rsp.responseTokenUsage]]

-- | Custom FromJSON for LlmResponse that handles backward compatibility.
instance FromJSON LlmResponse where
    parseJSON = Aeson.withObject "LlmResponse" $ \v ->
        LlmResponse
            <$> v .: "responseText"
            <*> v .: "responseThinking"
            <*> v .: "rawResponse"
            <*> v .:? "responseTokenUsage"

-- | LLM tool-call.
newtype LlmToolCall = LlmToolCall Aeson.Value
    deriving (Show, Ord, Eq, FromJSON, ToJSON)

-- Minimal tool structure loosely inspired by json-schema.
-- Caveat here is that we want at a same time to have a tool has a bag of function and as some json-serializable object.
data SystemToolDefinitionV1 = SystemToolDefinitionV1
    { name :: Text
    , llmName :: Text
    , description :: Text
    , properties :: [ParamProperty]
    , raw :: Aeson.Value
    }
    deriving (Show, Ord, Eq, Generic)
instance FromJSON SystemToolDefinitionV1
instance ToJSON SystemToolDefinitionV1

data SystemToolDefinition
    = V0 Aeson.Value
    | V1 SystemToolDefinitionV1
    deriving (Show, Ord, Eq, Generic)
instance FromJSON SystemToolDefinition
instance ToJSON SystemToolDefinition

-- | System Tool.
newtype SystemTool = SystemTool SystemToolDefinition
    deriving (Show, Ord, Eq, FromJSON, ToJSON)

-- | User query - can include text and media attachments.
data UserQuery = UserQuery
    { queryText :: Text
    -- ^ The text query/prompt
    , queryMedia :: [MediaAttachment]
    -- ^ Optional media attachments (images, audio, video, etc.)
    }
    deriving (Show, Ord, Eq, Generic)

{- | Custom ToJSON for UserQuery with backward compatibility.
Legacy format was just a text string.
-}
instance ToJSON UserQuery where
    toJSON (UserQuery text []) = Aeson.toJSON text
    toJSON (UserQuery text media) =
        Aeson.object
            [ "text" .= text
            , "media" .= media
            ]

-- | Custom FromJSON for UserQuery with backward compatibility.
instance FromJSON UserQuery where
    parseJSON v = parseNewFormat v <|> parseLegacyFormat v
      where
        parseNewFormat = Aeson.withObject "UserQuery" $ \obj -> do
            text <- obj .: "text"
            media <- obj .:? "media" >>= pure . maybe [] id
            pure $ UserQuery text media
        parseLegacyFormat val = do
            text <- Aeson.parseJSON val
            pure $ UserQuery text []

-------------------------------------------------------------------------------
-- UserToolResponse - Multi-modal response type
-------------------------------------------------------------------------------

{- | Tool response from the user/agent system to the LLM.

This type supports multi-modal responses with four variants:

* 'TextResponse' - Plain UTF-8 text (most common case)
* 'JsonResponse' - Structured JSON data from API tools, SQLite, etc.
* 'MediaResponse' - Single binary media item (base64-encoded)
* 'MixedResponse' - Multi-modal: alternating text and media parts

Design principles:
* LLM-agnostic core representation
* Backwards compatible JSON serialization
* Explicit type tags for clarity
-}
data UserToolResponse
    = -- | Plain UTF-8 text response
      TextResponse Text
    | -- | Structured JSON data
      JsonResponse Aeson.Value
    | -- | Single binary media (base64-encoded)
      MediaResponse MediaAttachment
    | -- | Multi-modal: alternating text and media parts
      MixedResponse [ContentPart]
    deriving (Show, Ord, Eq, Generic)

{- | Custom ToJSON for UserToolResponse.

New format uses explicit type tags:
* {"type": "text", "content": "..."}
* {"type": "json", "content": {...}}
* {"type": "media", "mimeType": "...", "base64Data": "...", ...}
* {"type": "mixed", "parts": [...]}

Legacy format (newtype wrapper) is no longer produced, but can still be parsed.
-}
instance ToJSON UserToolResponse where
    toJSON (TextResponse txt) =
        Aeson.object
            [ "type" .= ("text" :: Text)
            , "content" .= txt
            ]
    toJSON (JsonResponse val) =
        Aeson.object
            [ "type" .= ("json" :: Text)
            , "content" .= val
            ]
    toJSON (MediaResponse media) =
        Aeson.object $
            [ "type" .= ("media" :: Text)
            , "mimeType" .= media.mediaMimeType
            , "base64Data" .= media.mediaBase64Data
            ]
                ++ ["filename" .= fname | Just fname <- [media.mediaFilename]]
    toJSON (MixedResponse parts) =
        Aeson.object
            [ "type" .= ("mixed" :: Text)
            , "parts" .= parts
            ]

{- | Custom FromJSON for UserToolResponse with backwards compatibility.

Handles both:
* New format: {"type": "text", "content": "..."}
* Legacy format: {"_userToolResponse": <any json>} or plain values

Legacy sessions stored UserToolResponse as a newtype wrapper around Aeson.Value.
These are now converted to TextResponse or JsonResponse based on content.
-}
instance FromJSON UserToolResponse where
    parseJSON v = parseNewFormat v <|> parseLegacyFormat v
      where
        -- New format with explicit type tags
        parseNewFormat = Aeson.withObject "UserToolResponse" $ \obj -> do
            typeTag <- obj .: "type"
            case typeTag :: Text of
                "text" -> TextResponse <$> obj .: "content"
                "json" -> JsonResponse <$> obj .: "content"
                "media" ->
                    MediaResponse
                        <$> ( MediaAttachment
                                <$> obj .: "mimeType"
                                <*> obj .: "base64Data"
                                <*> obj .:? "filename"
                            )
                "mixed" -> MixedResponse <$> obj .: "parts"
                _ -> fail $ "Unknown UserToolResponse type: " <> Text.unpack typeTag

        -- Legacy format: try to parse as the old newtype wrapper
        parseLegacyFormat val =
            case val of
                Aeson.Object obj ->
                    -- Check for legacy wrapper field
                    case KeyMap.lookup "_userToolResponse" obj of
                        Just innerVal -> parseLegacyValue innerVal
                        Nothing -> parseLegacyValue val
                _ -> parseLegacyValue val

        -- Convert legacy Aeson.Value to appropriate response type
        parseLegacyValue :: Aeson.Value -> Aeson.Types.Parser UserToolResponse
        parseLegacyValue val =
            case val of
                Aeson.String txt -> pure $ TextResponse txt
                other -> pure $ JsonResponse other

-------------------------------------------------------------------------------
-- Turn and Session types
-------------------------------------------------------------------------------

-- Bundle user-turn content.
-- todo: medias
data UserTurnContent
    = UserTurnContent
    { userPrompt :: SystemPrompt
    , userTools :: [SystemTool]
    , userQuery :: Maybe UserQuery
    , userToolResponses :: [(LlmToolCall, UserToolResponse)]
    }
    deriving (Show, Ord, Eq, Generic)
instance FromJSON UserTurnContent
instance ToJSON UserTurnContent

-- Bundle llm-turn content.
data LlmTurnContent
    = LlmTurnContent
    { llmResponse :: LlmResponse
    , llmToolCalls :: [LlmToolCall]
    }
    deriving (Show, Ord, Eq, Generic)
instance FromJSON LlmTurnContent
instance ToJSON LlmTurnContent

{- | Partial user-turn content for async execution.

Represents an incomplete user turn where some tool calls have completed
and others are still pending (either not yet executed or yielded for
external completion).
-}
data PartialUserTurnContent = PartialUserTurnContent
    { pUserPrompt :: SystemPrompt
    -- ^ System prompt used
    , pUserTools :: [SystemTool]
    -- ^ Tools available
    , pUserQuery :: Maybe UserQuery
    -- ^ User query if any
    , pCompletedResponses :: [(LlmToolCall, UserToolResponse)]
    -- ^ Completed tool calls with their responses
    , pPendingCalls :: [LlmToolCall]
    -- ^ Tool calls that still need execution
    , pPendingContinuations :: [(ContinuationToken, CacheKey)]
    -- ^ Continuation tokens for yielded tool calls (async mode)
    }
    deriving (Show, Ord, Eq, Generic)

instance ToJSON PartialUserTurnContent where
    toJSON content =
        Aeson.object
            [ "userPrompt" .= content.pUserPrompt
            , "userTools" .= content.pUserTools
            , "userQuery" .= content.pUserQuery
            , "completedResponses" .= content.pCompletedResponses
            , "pendingCalls" .= content.pPendingCalls
            , "pendingContinuations" .= content.pPendingContinuations
            ]

instance FromJSON PartialUserTurnContent where
    parseJSON = Aeson.withObject "PartialUserTurnContent" $ \v ->
        PartialUserTurnContent
            <$> v .: "userPrompt"
            <*> v .: "userTools"
            <*> v .:? "userQuery"
            <*> v .: "completedResponses"
            <*> v .: "pendingCalls"
            <*> v .:? "pendingContinuations" .!= []

{- | Unification.

Each turn now optionally includes 'StepByteUsage' for tracking
data exchange sizes. The 'Maybe' allows backward compatibility
with sessions that were created before byte tracking was added.

Version 2 additions:
* 'PartialUserTurn' - Represents incomplete tool execution in async mode
-}
data Turn
    = UserTurn UserTurnContent (Maybe StepByteUsage)
    | LlmTurn LlmTurnContent (Maybe StepByteUsage)
    | PartialUserTurn PartialUserTurnContent (Maybe StepByteUsage)
    deriving (Show, Ord, Eq, Generic)

{- | Custom ToJSON instance that matches the FromJSON format.
Produces objects with "tag", "contents", and optional "byteUsage" fields.
-}
instance ToJSON Turn where
    toJSON (UserTurn contents byteUsage) =
        Aeson.object $
            ["tag" .= ("UserTurn" :: Text), "contents" .= contents]
                ++ ["byteUsage" .= usage | Just usage <- [byteUsage]]
    toJSON (LlmTurn contents byteUsage) =
        Aeson.object $
            ["tag" .= ("LlmTurn" :: Text), "contents" .= contents]
                ++ ["byteUsage" .= usage | Just usage <- [byteUsage]]
    toJSON (PartialUserTurn contents byteUsage) =
        Aeson.object $
            ["tag" .= ("PartialUserTurn" :: Text), "contents" .= contents]
                ++ ["byteUsage" .= usage | Just usage <- [byteUsage]]

{- | Legacy Turn structure without byte usage tracking.
Used for backward compatibility when parsing old sessions.
-}
data Turn_v0
    = UserTurn_v0 UserTurnContent
    | LlmTurn_v0 LlmTurnContent
    deriving (Show, Ord, Eq, Generic)

instance FromJSON Turn_v0

{- | Custom FromJSON instance for Turn that handles retro-compatibility.
First tries to parse as the new format (with Maybe StepByteUsage),
then falls back to the old Turn_v0 format (without byte usage).
-}
instance FromJSON Turn where
    parseJSON v = parseNew v <|> parseOld v
      where
        parseNew = Aeson.withObject "Turn" $ \obj -> do
            tag <- obj .: "tag"
            case tag :: Text of
                "UserTurn" -> do
                    contents <- obj .: "contents"
                    byteUsage <- obj .:? "byteUsage"
                    pure $ UserTurn contents byteUsage
                "LlmTurn" -> do
                    contents <- obj .: "contents"
                    byteUsage <- obj .:? "byteUsage"
                    pure $ LlmTurn contents byteUsage
                "PartialUserTurn" -> do
                    contents <- obj .: "contents"
                    byteUsage <- obj .:? "byteUsage"
                    pure $ PartialUserTurn contents byteUsage
                _ -> fail $ "Unknown Turn tag: " ++ show tag

        parseOld = Aeson.withObject "Turn" $ \obj -> do
            -- Try parsing as old format (v0)
            turnV0 <- Aeson.parseJSON (Aeson.Object obj)
            case turnV0 of
                UserTurn_v0 content -> pure $ UserTurn content Nothing
                LlmTurn_v0 content -> pure $ LlmTurn content Nothing

-------------------------------------------------------------------------------
-- Session with versioning
-------------------------------------------------------------------------------

{- | A session representing a conversation with the LLM.

Sessions track the conversation history (turns), identifiers for correlation,
and an optional version field for feature compatibility.

Version history:
* Nothing / missing - Legacy session (pre-media support)
* Just 1 - Media support enabled (base64-encoded binary content)
* Just 2 - Async/resumable execution support
-}
data Session
    = Session
    { turns :: [Turn]
    , sessionId :: SessionId
    , forkedFromSessionId :: Maybe SessionId
    , turnId :: TurnId
    , sessionVersion :: Maybe Int
    {- ^ Optional session version for feature compatibility:
    Nothing = legacy (pre-media), Just 1 = media support, Just 2 = async support
    -}
    , sessionExecutionMode :: Maybe ExecutionMode
    {- ^ Execution mode used for this session.
    Nothing = default to Synchronous for backward compatibility.
    -}
    }
    deriving (Show, Ord, Eq, Generic)

-- | Custom ToJSON for Session that omits optional fields when Nothing.
instance ToJSON Session where
    toJSON s =
        Aeson.object $
            [ "turns" .= s.turns
            , "sessionId" .= s.sessionId
            , "forkedFromSessionId" .= s.forkedFromSessionId
            , "turnId" .= s.turnId
            ]
                ++ ["sessionVersion" .= v | Just v <- [s.sessionVersion]]
                ++ ["sessionExecutionMode" .= m | Just m <- [s.sessionExecutionMode]]

-- | Custom FromJSON for Session that handles missing fields.
instance FromJSON Session where
    parseJSON = Aeson.withObject "Session" $ \v ->
        Session
            <$> v .: "turns"
            <*> v .: "sessionId"
            <*> v .:? "forkedFromSessionId"
            <*> v .: "turnId"
            <*> v .:? "sessionVersion"
            <*> v .:? "sessionExecutionMode"

-------------------------------------------------------------------------------
-- Session Migration
-------------------------------------------------------------------------------

{- | Migrate a session from version 1 to version 2.

Adds async/resumable execution support by:
* Setting version to Just 2
* Defaulting execution mode to Synchronous for existing sessions
-}
migrateSessionV1ToV2 :: Session -> Session
migrateSessionV1ToV2 session =
    session
        { sessionVersion = Just 2
        , sessionExecutionMode = Just Synchronous
        }

-------------------------------------------------------------------------------
-- Session Progress Tracking
-------------------------------------------------------------------------------

{- | Represents the progress of a session through its lifecycle.
This type is used with 'OnSessionProgress' callbacks to track
session state changes in a decoupled manner.
-}
data SessionProgress
    = -- | Emitted when a new session is started
      SessionStarted Session
    | -- | Emitted after each step when the session is updated
      SessionUpdated Session
    | -- | Emitted when the session completes successfully
      SessionCompleted Session
    | -- | Emitted when the session fails with an error message
      SessionFailed Session Text
    deriving (Show, Eq)

{- | Callback type for receiving session progress updates.
This decouples the session storage mechanism from the agent loop logic.
-}
type OnSessionProgress = SessionProgress -> IO ()

-- | A no-op session progress handler for when tracking is not needed.
ignoreSessionProgress :: OnSessionProgress
ignoreSessionProgress = const (pure ())

