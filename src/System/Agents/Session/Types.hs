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

    -- * Re-exports for convenience
    TokenUsage (..),
) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)

import System.Agents.LLMs.OpenAI (TokenUsage (..))
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

-- | Custom ToJSON for StepByteUsage that maintains backward compatibility.
-- Fields are only included if they have values (non-zero or Just).
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

-- | Custom FromJSON for StepByteUsage that handles backward compatibility.
-- Missing fields are filled with default values (0 for Int, Nothing for Maybe).
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

-- | User query.
newtype UserQuery = UserQuery Text
    deriving (Show, Ord, Eq, FromJSON, ToJSON)

-- | Tool response given.
newtype UserToolResponse = UserToolResponse Aeson.Value
    deriving (Show, Ord, Eq, FromJSON, ToJSON)

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

{- | Unification.

Each turn now optionally includes 'StepByteUsage' for tracking
data exchange sizes. The 'Maybe' allows backward compatibility
with sessions that were created before byte tracking was added.
-}
data Turn
    = UserTurn UserTurnContent (Maybe StepByteUsage)
    | LlmTurn LlmTurnContent (Maybe StepByteUsage)
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
                _ -> fail $ "Unknown Turn tag: " ++ show tag

        parseOld = Aeson.withObject "Turn" $ \obj -> do
            -- Try parsing as old format (v0)
            turnV0 <- Aeson.parseJSON (Aeson.Object obj)
            case turnV0 of
                UserTurn_v0 content -> pure $ UserTurn content Nothing
                LlmTurn_v0 content -> pure $ LlmTurn content Nothing

data Session
    = Session
    { turns :: [Turn]
    , sessionId :: SessionId
    , forkedFromSessionId :: Maybe SessionId
    , turnId :: TurnId
    }
    deriving (Show, Ord, Eq, Generic)
instance FromJSON Session
instance ToJSON Session

