{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
OS-native agent tree initialization (Phase 2+ of migration).

This module provides an alternative to 'System.Agents.AgentTree' that
creates OS Core entities instead of legacy Runtime structures.

This is the New-Only mode implementation where:
- Agents are created as OS entities with AgentConfig/AgentState components
- Tools are stored in OS-native data structures (AgentToolsStore)
- AgentRegistry is used instead of RuntimeRegistry
- No RuntimeBridge or legacy Runtime fallback is needed

For migration purposes, both AgentTree implementations can coexist,
allowing gradual migration of CLI modes.

== Usage

@
import System.Agents.OS.AgentTree

-- Initialize OS world
world <- atomically $ registerComponentStore <$> newWorld <*> pure (Proxy @AgentConfig)
world' <- atomically $ registerComponentStore world (Proxy @AgentState)

-- Create tools store
toolsStore <- createAgentToolsStore

-- Create agent
let config = createAgentConfig "my-agent" "openai" "..." "gpt-4" "key1" "You are..." []
result <- createAgent world' config
case result of
    Left err -> handleError err
    Right agentId -> do
        -- Set up tools
        setAgentTools toolsStore "my-agent" [tool1, tool2]
        ...
@
-}
module System.Agents.OS.AgentTree (
    -- * Types
    OSAgentTree (..),
    OSAgentNode (..),
    OSProps (..),

    -- * Initialization
    initOSAgentTree,

    -- * Agent creation
    createOSAgentFromTree,

    -- * Agent lookup
    getOSAgentConfig,
    getOSAgentTools,
    getOSAgentNode,
    getOSRootAgent,

    -- * Re-exports from OS.Agents
    module System.Agents.OS.Agents,
) where

import Control.Concurrent.STM (atomically)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import qualified System.FilePath as FilePath

import Prod.Tracer (Tracer)
import System.Agents.AgentTree (
    AgentConfigTree (..),
    AgentRegistry,
    LoadedApiKeys,
    TreeTrace,
    newAgentRegistry,
    registerAgent,
 )
import System.Agents.Base (Agent (..), AgentId, AgentSlug)
import System.Agents.OS.Agents
import System.Agents.OS.Core (AgentConfig (..), AgentState (..), World (..))
import qualified System.Agents.OS.Core as OS
import System.Agents.OS.Core.World (registerComponentStore)
import System.Agents.ToolRegistration (ToolRegistration (..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

{- | An agent node in the OS-native tree.

Unlike the legacy AgentTree which contains a Runtime, this contains
references to OS entities and their associated data.
-}
data OSAgentNode = OSAgentNode
    { osNodeFile :: FilePath
    , osNodeConfig :: Agent
    , osNodeAgentId :: AgentId
    , osNodeChildren :: [OSAgentNode]
    }
    deriving (Show)

{- | The OS-native agent tree root.

Contains the world (ECS context), agent registry, and tools store
for OS-native agent management.
-}
data OSAgentTree = OSAgentTree
    { osTreeWorld :: World
    , osTreeRegistry :: AgentRegistry
    , osTreeToolsStore :: AgentToolsStore
    , osTreeNodes :: Map AgentSlug OSAgentNode
    , osTreeRoot :: OSAgentNode
    , osTreeRootDir :: FilePath
    }

{- | Properties for OS-native agent tree loading.

Similar to legacy Props but for OS-native agent creation.
-}
data OSProps = OSProps
    { osApiKeys :: LoadedApiKeys
    , osRootAgentFile :: FilePath
    , osInteractiveTracer :: Tracer IO TreeTrace
    }

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

{- | Initialize a new OS agent tree infrastructure.

This creates:
1. An OS World with AgentConfig and AgentState component stores registered
2. An empty AgentRegistry
3. An empty AgentToolsStore

After initialization, use 'createOSAgentFromTree' to populate with agents.

Example:

@
tree <- initOSAgentTree
-- Now create agents and add them to the tree
@
-}
initOSAgentTree :: IO OSAgentTree
initOSAgentTree = do
    -- Create world with required component stores
    world <- atomically $ do
        w <- OS.newWorld
        w' <- registerComponentStore w (Proxy @AgentConfig)
        registerComponentStore w' (Proxy @AgentState)

    -- Create registry and tools store
    registry <- newAgentRegistry
    toolsStore <- createAgentToolsStore

    -- Create placeholder root (to be populated later)
    let placeholderRoot =
            OSAgentNode
                { osNodeFile = ""
                , osNodeConfig = error "placeholder"
                , osNodeAgentId = error "placeholder"
                , osNodeChildren = []
                }

    pure $
        OSAgentTree
            { osTreeWorld = world
            , osTreeRegistry = registry
            , osTreeToolsStore = toolsStore
            , osTreeNodes = Map.empty
            , osTreeRoot = placeholderRoot
            , osTreeRootDir = "."
            }

-------------------------------------------------------------------------------
-- Agent Creation
-------------------------------------------------------------------------------

{- | Create an OS agent from a legacy AgentConfigTree.

This converts a legacy configuration tree to OS-native entities.
The legacy tree is used for its configuration data, but no Runtime
is created.

This function:
1. Creates an OS AgentConfig from the legacy Agent config
2. Creates an OS agent entity
3. Registers the agent in the OS registry
4. (Optionally) initializes toolboxes and stores tools

Returns the updated OSAgentTree with the new agent added.
-}
createOSAgentFromTree ::
    -- | Current tree state
    OSAgentTree ->
    -- | Legacy config tree
    AgentConfigTree ->
    IO (Either AgentCreationError (OSAgentTree, AgentId))
createOSAgentFromTree tree configTree = do
    let agent = configTree.agentConfig
    let rootDir = FilePath.takeDirectory configTree.agentConfigFile

    -- Create OS AgentConfig
    let osConfig =
            createAgentConfig
                agent.slug
                agent.flavor
                agent.modelUrl
                agent.modelName
                agent.apiKeyId
                (Text.unlines agent.systemPrompt)
                [] -- Toolbox bindings to be set up separately

    -- Create OS agent entity
    result <- createAgent (osTreeWorld tree) osConfig
    case result of
        Left err -> pure $ Left err
        Right agentId -> do
            -- Register in AgentRegistry
            _ <- registerAgent (osTreeRegistry tree) agent.slug osConfig

            -- Create node
            let node =
                    OSAgentNode
                        { osNodeFile = configTree.agentConfigFile
                        , osNodeConfig = agent
                        , osNodeAgentId = agentId
                        , osNodeChildren = [] -- Populated separately
                        }

            -- Update tree
            let tree' =
                    tree
                        { osTreeNodes = Map.insert agent.slug node (osTreeNodes tree)
                        , osTreeRoot = if null (osTreeRootDir tree) then node else osTreeRoot tree
                        , osTreeRootDir = if null (osTreeRootDir tree) then rootDir else osTreeRootDir tree
                        }

            pure $ Right (tree', agentId)

-------------------------------------------------------------------------------
-- Agent Lookup
-------------------------------------------------------------------------------

-- | Get an agent's configuration from the OS tree.
getOSAgentConfig :: OSAgentTree -> AgentId -> IO (Maybe AgentConfig)
getOSAgentConfig tree agentId =
    atomically $ getAgentConfig (osTreeWorld tree) agentId

-- | Get tools for an OS agent.
getOSAgentTools :: OSAgentTree -> AgentSlug -> IO [ToolRegistration]
getOSAgentTools tree agentSlug =
    getAgentTools (osTreeToolsStore tree) agentSlug

-- | Get an agent node by slug.
getOSAgentNode :: OSAgentTree -> AgentSlug -> Maybe OSAgentNode
getOSAgentNode tree agentSlug =
    Map.lookup agentSlug (osTreeNodes tree)

-- | Get the root agent node.
getOSRootAgent :: OSAgentTree -> OSAgentNode
getOSRootAgent = osTreeRoot
