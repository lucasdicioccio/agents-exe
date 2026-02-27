{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Agents.Session.Base where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)

import System.Agents.ToolSchema


-------------------------------------------------------------------------------
newtype SessionId = SessionId UUID
    deriving (Show, Ord, Eq, FromJSON, ToJSON)

newSessionId :: IO SessionId
newSessionId =
    SessionId <$> UUID.nextRandom

-------------------------------------------------------------------------------
newtype TurnId = TurnId UUID
    deriving (Show, Ord, Eq, FromJSON, ToJSON)

newTurnId :: IO TurnId
newTurnId =
    TurnId <$> UUID.nextRandom

-------------------------------------------------------------------------------

-- TODO: iterate on Aeson.Value to decorate types with some extra structures
-- goal is to have some internal representation that will allow extensibility

-- | A text prompt given.
newtype SystemPrompt = SystemPrompt Text
    deriving (Show, Ord, Eq, FromJSON, ToJSON)

-- | LLM response.
data LlmResponse = LlmResponse 
    { responseText :: Maybe Text
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
  { llmResponse :: LlmResponse -- todo: flatten the raw here, but requires changing some definitions
  , llmToolCalls :: [LlmToolCall]
  }
    deriving (Show, Ord, Eq, Generic)
instance FromJSON LlmTurnContent
instance ToJSON LlmTurnContent
  
-- | Unification.
data Turn
  = UserTurn UserTurnContent
  | LlmTurn LlmTurnContent
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

-------------------------------------------------------------------------------
data MissingUserPrompt = MissingUserPrompt
  { missingQuery :: Bool
  , missingToolCalls :: [LlmToolCall]
  } deriving (Show, Eq, Ord)

-- todo: medias
data LlmCompletion = LlmCompletion
  { completePrompt :: SystemPrompt
  , completeTools :: [SystemTool]
  , completeQuery :: Maybe UserQuery
  , completeToolResponses :: [(LlmToolCall, UserToolResponse)]
  , completeConversationHistory :: [Turn]
  } deriving (Show, Eq, Ord)

data Action r
  = Stop r
  | AskUserPrompt MissingUserPrompt
  | AskLlmCompletion LlmCompletion
  -- comfort/note fully-motivated below, is to evolve the agent so that th runner logic has a primitive to do so
  -- * one advantage is it allows "pure" agents (i.e., dropping the need for an IO in usrQuery et al.)
  -- * could consider forking but that would require a joining function (r -> r -> r) to combine results, which prevents the functorial aspects
  -- * could consider extensiblility so that agents come with their set of decisions, but the runloop then has to account for these
  | Evolve (Agent r)
  deriving Functor

-- An agent is a decorated step function from a session step to an action that
-- may yield a result r or some delay.
-- Functions in its body.
data Agent r = Agent
  { step :: Session -> IO (Action r)
  --
  , sysPrompt :: IO SystemPrompt
  , sysTools :: IO [SystemTool]
  , usrQuery :: IO (Maybe UserQuery)
  , toolCall :: LlmToolCall -> IO UserToolResponse
  -- 
  , complete :: LlmCompletion -> IO (LlmResponse, [LlmToolCall])
  }
  deriving Functor

-------------------------------------------------------------------------------
-- Session Progress Tracking
-------------------------------------------------------------------------------

-- | Represents the progress of a session through its lifecycle.
-- This type is used with 'OnSessionProgress' callbacks to track
-- session state changes in a decoupled manner.
data SessionProgress
  = SessionStarted Session
    -- ^ Emitted when a new session is started
  | SessionUpdated Session
    -- ^ Emitted after each step when the session is updated
  | SessionCompleted Session
    -- ^ Emitted when the session completes successfully
  | SessionFailed Session Text
    -- ^ Emitted when the session fails with an error message
  deriving (Show, Eq)

-- | Callback type for receiving session progress updates.
-- This decouples the session storage mechanism from the agent loop logic.
type OnSessionProgress = SessionProgress -> IO ()

-- | A no-op session progress handler for when tracking is not needed.
ignoreSessionProgress :: OnSessionProgress
ignoreSessionProgress = const (pure ())

