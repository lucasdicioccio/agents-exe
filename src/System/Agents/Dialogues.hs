{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.Dialogues where

import Control.Concurrent.STM (STM)
import Data.Text (Text)

import System.Agents.Base (ConversationId)
import qualified System.Agents.Conversation as Conversation
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.Runtime as Runtime

data LoadedAgent
    = LoadedAgent
    { loadedAgentRuntime :: Runtime.Runtime
    , loadedAgentTools :: [Runtime.ToolRegistration]
    , loadedAgentInfo :: FileLoader.OpenAIAgent
    }

data StartedConversation
    = StartedConversation
    { conversingAgent :: FileLoader.OpenAIAgent
    , conversationState :: Conversation.ConversationState
    , headline :: Text
    }

data OngoingConversation
    = OngoingConversation
    { conversationId :: ConversationId
    , conversingAgent :: FileLoader.OpenAIAgent
    , conversationStatus :: Conversation.ConversationStatus
    , conversationHistory :: [Runtime.Trace]
    , prompt :: Maybe Text -> STM Bool
    , headline :: Text
    }
