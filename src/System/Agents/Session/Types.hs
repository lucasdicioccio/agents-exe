{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Core session types shared across the agent system.
--
-- This module contains the fundamental session types that need to be
-- accessible from multiple parts of the codebase, including tool execution
-- contexts. Keeping these types in a separate module helps avoid circular
-- dependencies between System.Agents.Session.Base and System.Agents.Tools.Context.
--
module System.Agents.Session.Types (
    -- * Identifiers
    SessionId(..),
    TurnId(..),
    newSessionId,
    newTurnId,
    
    -- * Core types
    Session(..),
    Turn(..),
    UserTurnContent(..),
    LlmTurnContent(..),
    
    -- * Byte usage tracking
    StepByteUsage(..),
    calculateStepByteUsage,
    sessionTotalBytes,
    
    -- * Content types
    SystemPrompt(..),
    LlmResponse(..),
    LlmToolCall(..),
    UserQuery(..),
    UserToolResponse(..),
    
    -- * Tool definitions
    SystemTool(..),
    SystemToolDefinition(..),
    SystemToolDefinitionV1(..),
) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)

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

-- | Byte usage breakdown for a single step.
--
-- Tracks the amount of data exchanged during a single turn of conversation,
-- broken down by category for cost transparency and debugging.
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
    } deriving (Show, Ord, Eq, Generic)

instance FromJSON StepByteUsage
instance ToJSON StepByteUsage

-- | Helper to calculate total bytes from components.
-- Use this when you have individual components but want to ensure
-- consistency between total and sum of parts.
calculateStepByteUsage :: Int -> Int -> Int -> Int -> StepByteUsage
calculateStepByteUsage input output reasoning tool =
    StepByteUsage
        { stepTotalBytes = input + output + reasoning + tool
        , stepInputBytes = input
        , stepOutputBytes = output
        , stepReasoningBytes = reasoning
        , stepToolBytes = tool
        }

-- | Calculate total bytes for an entire session.
-- Returns the sum of all step totals across all turns.
sessionTotalBytes :: Session -> Int
sessionTotalBytes session =
    sum [ stepTotalBytes usage
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
    , responseThinking :: Maybe Text  -- ^ Separate thinking/reasoning content from models like o1/o3 and Claude
    , rawResponse :: Aeson.Value
    }
    deriving (Show, Ord, Eq, Generic)
instance FromJSON LlmResponse
instance ToJSON LlmResponse

-- | LLM tool-call.
newtype LlmToolCall = LlmToolCall Aeson.Value
    deriving (Show, Ord, Eq, FromJSON, ToJSON)

-- Minimal tool structure loosely inspired by json-schema.
-- Caveat here is that we want at a same time to have a tool has a bag of function and as some json-serializable object.
data SystemToolDefinitionV1 = SystemToolDefinitionV1 {
    name :: Text
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
  
-- | Unification.
-- 
-- Each turn now optionally includes 'StepByteUsage' for tracking
-- data exchange sizes. The 'Maybe' allows backward compatibility
-- with sessions that were created before byte tracking was added.
data Turn
  = UserTurn UserTurnContent (Maybe StepByteUsage)
  | LlmTurn LlmTurnContent (Maybe StepByteUsage)
    deriving (Show, Ord, Eq, Generic)
instance FromJSON Turn
instance ToJSON Turn

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


