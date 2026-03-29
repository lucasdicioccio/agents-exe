{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
OS-native agent initialization and management.

This module provides functions to create and manage agents using the
new OS Core ECS model, replacing the legacy Runtime-per-agent architecture.

This is part of Phase 2+ of the OS migration (New-Only mode).
-}
module System.Agents.OS.Agents (
    -- * Agent Creation
    createAgent,
    createAgentConfig,
    AgentCreationError (..),

    -- * Agent Lookup
    getAgentConfig,
    getAgentState,
    updateAgentState,

    -- * Tool Management
    AgentToolsStore,
    createAgentToolsStore,
    getAgentTools,
    setAgentTools,
    addAgentTool,
    removeAgentTool,

    -- * Utility Functions
    agentExists,
    listAllAgents,
) where

import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time (getCurrentTime)

import System.Agents.Base (AgentId (..), AgentSlug, newAgentId)
import qualified System.Agents.Base as Base
import System.Agents.OS.Core (
    AgentConfig (..),
    AgentState (..),
    AgentStatus (..),
    ModelConfig (..),
    World (..),
 )
import System.Agents.OS.Core.Types (Component (..), EntityId (..))
import qualified System.Agents.OS.Core.World as OSWorld
import System.Agents.ToolRegistration (ToolRegistration (..))
import System.Agents.ToolSchema (ToolName(..), ToolDescription(..))

-------------------------------------------------------------------------------
-- Agent Creation
-------------------------------------------------------------------------------

-- | Errors that can occur during agent creation.
data AgentCreationError
    = AgentConfigStoreNotRegistered
    | AgentStateStoreNotRegistered
    | InvalidModelConfig Text
    deriving (Show, Eq)

-- | Convert AgentId to EntityId
agentIdToEntityId :: AgentId -> EntityId
agentIdToEntityId (AgentId uuid) = EntityId uuid

-- | Convert EntityId to AgentId
entityIdToAgentId :: EntityId -> AgentId
entityIdToAgentId (EntityId uuid) = AgentId uuid

-- | Check if a component store is registered
hasComponentStore :: forall a. (Component a) => World -> Bool
hasComponentStore world =
    let cid = componentId (Proxy @a)
     in HashMap.member cid world.componentStores

{- | Create a new agent in the OS.

This creates an entity with AgentConfig and AgentState components,
returning the new AgentId.

Example:

@
result <- createAgent world $ AgentConfig
    { agentName = "my-agent"
    , agentModel = ModelConfig { ... }
    , agentSystemPrompt = "You are a helpful assistant..."
    , agentToolboxBindings = []
    }

case result of
    Left err -> putStrLn $ "Failed to create agent: " ++ show err
    Right agentId -> putStrLn $ "Created agent: " ++ show agentId
@
-}
createAgent :: World -> AgentConfig -> IO (Either AgentCreationError Base.AgentId)
createAgent world config = do
    -- Check that required component stores are registered
    let hasConfigStore = hasComponentStore @AgentConfig world
    let hasStateStore = hasComponentStore @AgentState world

    case (hasConfigStore, hasStateStore) of
        (False, _) -> pure $ Left AgentConfigStoreNotRegistered
        (_, False) -> pure $ Left AgentStateStoreNotRegistered
        (True, True) -> do
            -- Generate a new agent ID
            agentId <- newAgentId
            let entityId = agentIdToEntityId agentId

            -- Create initial agent state
            now <- getCurrentTime
            let initialState =
                    AgentState
                        { agentStatus = AgentIdle
                        , agentCurrentConversation = Nothing
                        , agentCreatedAt = now
                        }

            -- Store components in the world
            atomically $ do
                OSWorld.setComponent world entityId config
                OSWorld.setComponent world entityId initialState

            pure $ Right agentId

{- | Create an AgentConfig from legacy-style parameters.

This helper function converts the old Runtime-style parameters to
the new OS AgentConfig format.
-}
createAgentConfig ::
    -- | Agent name/slug
    Text ->
    -- | Model flavor (e.g., "openai")
    Text ->
    -- | Model URL
    Text ->
    -- | Model name
    Text ->
    -- | API key ID
    Text ->
    -- | System prompt
    Text ->
    -- | Toolbox binding specs
    [Text] ->
    AgentConfig
createAgentConfig name flavor url mname apiKeyId prompt toolboxBindings =
    AgentConfig
        { agentName = name
        , agentModel =
            ModelConfig
                { modelFlavor = flavor
                , modelUrl = url
                , modelName = mname
                , modelApiKeyId = apiKeyId
                }
        , agentSystemPrompt = prompt
        , agentToolboxBindings = toolboxBindings
        }

-------------------------------------------------------------------------------
-- Agent Lookup
-------------------------------------------------------------------------------

{- | Get an agent's configuration.

Returns Nothing if the agent doesn't exist or the config store isn't registered.
-}
getAgentConfig :: World -> Base.AgentId -> STM (Maybe AgentConfig)
getAgentConfig world agentId = do
    let entityId = agentIdToEntityId agentId
    OSWorld.getComponent world entityId

{- | Get an agent's current state.

Returns Nothing if the agent doesn't exist or the state store isn't registered.
-}
getAgentState :: World -> Base.AgentId -> STM (Maybe AgentState)
getAgentState world agentId = do
    let entityId = agentIdToEntityId agentId
    OSWorld.getComponent world entityId

{- | Update an agent's state.

Does nothing if the agent doesn't exist.
-}
updateAgentState :: World -> Base.AgentId -> (AgentState -> AgentState) -> STM ()
updateAgentState world agentId f =
    let entityId = agentIdToEntityId agentId
     in OSWorld.modifyComponent world entityId f

{- | Check if an agent exists in the world.

An agent exists if it has both AgentConfig and AgentState components.
-}
agentExists :: World -> Base.AgentId -> STM Bool
agentExists world agentId = do
    mConfig <- getAgentConfig world agentId
    mState <- getAgentState world agentId
    pure $ isJust mConfig && isJust mState

{- | List all agents in the world.

Returns the AgentIds of all entities that have an AgentConfig component.
-}
listAllAgents :: World -> STM [Base.AgentId]
listAllAgents world = do
    entityIds <- OSWorld.allEntitiesWithComponent @AgentConfig world
    pure $ map entityIdToAgentId entityIds

-------------------------------------------------------------------------------
-- Tool Management
-------------------------------------------------------------------------------

{- | In-memory store for agent tools during migration.

This is a temporary solution until tools are fully migrated to OS components.
In the final OS model, tools will be stored as ToolboxBinding components.

Note: Uses AgentSlug (Text) as the key for HashMap compatibility.
-}
newtype AgentToolsStore = AgentToolsStore
    { unAgentToolsStore :: TVar (HashMap AgentSlug [ToolRegistration])
    }

-- | Create a new empty agent tools store.
createAgentToolsStore :: IO AgentToolsStore
createAgentToolsStore = AgentToolsStore <$> newTVarIO HashMap.empty

-- | Get all tools for an agent by slug.
getAgentTools :: AgentToolsStore -> AgentSlug -> IO [ToolRegistration]
getAgentTools store slug = do
    toolsMap <- readTVarIO (unAgentToolsStore store)
    pure $ HashMap.lookupDefault [] slug toolsMap

-- | Set all tools for an agent (replaces existing).
setAgentTools :: AgentToolsStore -> AgentSlug -> [ToolRegistration] -> IO ()
setAgentTools store slug tools =
    atomically $ modifyTVar' (unAgentToolsStore store) (HashMap.insert slug tools)

-- | Add a single tool to an agent.
addAgentTool :: AgentToolsStore -> AgentSlug -> ToolRegistration -> IO ()
addAgentTool store slug tool =
    atomically $ modifyTVar' (unAgentToolsStore store) $ \m ->
        let existing = HashMap.lookupDefault [] slug m
         in HashMap.insert slug (existing ++ [tool]) m

-- | Remove a tool from an agent by name.
removeAgentTool :: AgentToolsStore -> AgentSlug -> Text -> IO ()
removeAgentTool store slug toolName =
    atomically $ modifyTVar' (unAgentToolsStore store) $ \m ->
        let existing = HashMap.lookupDefault [] slug m
            filtered = filter (\t -> toolName /= getToolName' t) existing
         in HashMap.insert slug filtered m

-- | Extract tool name from ToolRegistration
getToolName' :: ToolRegistration -> Text
getToolName' reg = reg.declareTool.toolDescriptionName.getToolName

-------------------------------------------------------------------------------
-- STM Helpers
-------------------------------------------------------------------------------

modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' tvar f = do
    val <- readTVar tvar
    writeTVar tvar $! f val
