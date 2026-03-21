{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Entity-Component-System (ECS) core for the Operating System architecture.

This module provides the foundational data model for agents, toolboxes,
and resources using an ECS pattern.
-}
module System.Agents.OS.Core (
    -- * Re-exports from Types
    module System.Agents.OS.Core.Types,

    -- * Re-exports from World
    module System.Agents.OS.Core.World,

    -- * Agent Components
    AgentConfig (..),
    AgentState (..),
    AgentStatus (..),

    -- * Toolbox Components
    ToolboxConfig (..),
    ToolboxState (..),
    ToolboxBinding (..),
    ToolboxType (..),
    ResourceScope (..),
    ToolboxStatus (..),
    ModelConfig (..),
) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import System.Agents.OS.Core.Types
import System.Agents.OS.Core.World

-------------------------------------------------------------------------------
-- Agent Components
-------------------------------------------------------------------------------

-- | Configuration component for agents.
-- This is static configuration that doesn't change during agent execution.
data AgentConfig = AgentConfig
    { agentName :: Text
    , agentModel :: ModelConfig
    , agentSystemPrompt :: Text
    , agentToolboxBindings :: [ToolboxBindingSpec]
    }
    deriving (Show, Eq, Generic)

instance FromJSON AgentConfig
instance ToJSON AgentConfig

instance Component AgentConfig where
    componentId _ = ComponentTypeId 1

-- | Runtime state component for agents.
-- This changes during agent execution and tracks the current status.
data AgentState = AgentState
    { agentStatus :: AgentStatus
    , agentCurrentConversation :: Maybe ConversationId
    , agentCreatedAt :: UTCTime
    }
    deriving (Show, Eq, Generic)

instance FromJSON AgentState
instance ToJSON AgentState

instance Component AgentState where
    componentId _ = ComponentTypeId 2

-- | Status of an agent.
data AgentStatus
    = AgentIdle
    | AgentBusy TurnId
    | AgentError Text
    deriving (Show, Eq, Generic)

instance FromJSON AgentStatus
instance ToJSON AgentStatus

-------------------------------------------------------------------------------
-- Model Configuration
-------------------------------------------------------------------------------

-- | Configuration for an LLM model.
data ModelConfig = ModelConfig
    { modelFlavor :: Text
    , modelUrl :: Text
    , modelName :: Text
    , modelApiKeyId :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON ModelConfig
instance ToJSON ModelConfig

-------------------------------------------------------------------------------
-- Toolbox Components
-------------------------------------------------------------------------------

-- | Configuration component for toolboxes.
-- This is static configuration that doesn't change during toolbox lifetime.
data ToolboxConfig = ToolboxConfig
    { toolboxName :: Text
    , toolboxType :: ToolboxType
    , toolboxSettings :: Value
    }
    deriving (Show, Eq, Generic)

instance FromJSON ToolboxConfig
instance ToJSON ToolboxConfig

instance Component ToolboxConfig where
    componentId _ = ComponentTypeId 3

-- | Runtime state component for toolboxes.
-- This tracks the current state of a toolbox.
data ToolboxState = ToolboxState
    { toolboxScope :: ResourceScope
    , toolboxStatus :: ToolboxStatus
    , toolboxResourceRef :: Maybe ResourceId
    }
    deriving (Show, Eq, Generic)

instance FromJSON ToolboxState
instance ToJSON ToolboxState

instance Component ToolboxState where
    componentId _ = ComponentTypeId 4

-- | Binding between an agent and a toolbox.
data ToolboxBinding = ToolboxBinding
    { bindingAgentId :: AgentId
    , bindingToolboxId :: ToolboxId
    , bindingConfig :: Value
    }
    deriving (Show, Eq, Generic)

instance FromJSON ToolboxBinding
instance ToJSON ToolboxBinding

instance Component ToolboxBinding where
    componentId _ = ComponentTypeId 5

-------------------------------------------------------------------------------
-- Toolbox Types and Status
-------------------------------------------------------------------------------

-- | Types of toolboxes supported by the system.
data ToolboxType
    = ToolboxTypeBash
    | ToolboxTypeMCP
    | ToolboxTypeOpenAPI
    | ToolboxTypePostgREST
    | ToolboxTypeSqlite
    | ToolboxTypeSystem
    | ToolboxTypeDeveloper
    | ToolboxTypeLua
    | ToolboxTypeSkills
    deriving (Show, Eq, Generic)

instance FromJSON ToolboxType
instance ToJSON ToolboxType

-- | Scope of resources managed by a toolbox.
data ResourceScope
    = ScopeGlobal
    | ScopeAgent AgentId
    | ScopeConversation ConversationId
    deriving (Show, Eq, Generic)

instance FromJSON ResourceScope
instance ToJSON ResourceScope

-- | Status of a toolbox.
data ToolboxStatus
    = ToolboxInitializing
    | ToolboxReady
    | ToolboxError Text
    | ToolboxDisposed
    deriving (Show, Eq, Generic)

instance FromJSON ToolboxStatus
instance ToJSON ToolboxStatus

