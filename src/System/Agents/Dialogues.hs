{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.Dialogues where

import Control.Concurrent.STM (STM)
import Data.Text (Text)

import System.Agents.Base (Agent, ConversationId)
import qualified System.Agents.Conversation as Conversation
import qualified System.Agents.Runtime as Runtime

data LoadedAgent
    = LoadedAgent
    { loadedAgentRuntime :: Runtime.Runtime
    , loadedAgentInfo :: Agent
    }

data StartedConversation
    = StartedConversation
    { conversingAgent :: Agent
    , conversationState :: Conversation.ConversationState
    , headline :: Text
    }

data OngoingConversation
    = OngoingConversation
    { conversationId :: ConversationId
    , conversingAgent :: Agent
    , conversationStatus :: Conversation.ConversationStatus
    , conversationHistory :: [Runtime.Trace]
    , prompt :: Maybe Text -> STM Bool
    , headline :: Text
    }
