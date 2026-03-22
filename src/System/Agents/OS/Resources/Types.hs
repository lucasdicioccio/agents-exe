{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

{- |
Core types for the resource management system.

This module defines the foundational types for managing resources with
multiple lifecycle scopes. Resources can have different lifetimes:

* per-program: Global HTTP connection pool
* per-agent: Agent-specific sandbox
* per-toolbox: SQLite connection per toolbox instance
* per-conversation: Isolated Lua state
* per-turn: Temporary resources
* per-toolcall: Single-use resources

== Design Principles

1. Explicit Cleanup: Resources use explicit cleanup handlers for predictable
   resource management rather than relying on GC finalizers.

2. Scope Hierarchy: Resources are organized in a hierarchy where parent scopes
   encompass child scopes. A resource is valid as long as its scope and all
   parent scopes are active.

3. Thread Safety: The registry uses STM for concurrent access to resource
   tracking.

== Example Usage

@
import System.Agents.OS.Resources.Types

-- Create a registry
registry <- newResourceRegistry

-- Create a context for the current scope
let ctx = ResourceContext [ProgramScope] registry

-- Create a resource within this context
rid <- createResource ctx (SqliteResource config) $ \rid -> do
    conn <- openConnection config
    pure $ ResourceHandle
        { handleId = rid
        , handleCleanup = closeConnection conn
        , handleAccess = \f -> f (SqliteAccessor conn)
        }

-- Later, cleanup all resources in a scope
cleanupScope registry (AgentScope agentId)
@
-}
module System.Agents.OS.Resources.Types (
    -- * Scope Definition
    ResourceScope (..),
    ScopeLevel (..),
    scopeLevelToList,
    isScopeValid,

    -- * Resource Components
    ResourceInfo (..),
    ResourceType (..),
    ResourceHandle (..),
    ResourceAccessor (..),
    ResourceRegistry (..),
    ResourceContext (..),

    -- * Configuration types
    SqliteConfig (..),
    SqliteAccessMode (..),
    LuaConfig (..),
    HttpManagerConfig (..),
    ProcessConfig (..),

    -- * Registry operations
    newResourceRegistry,
    generateResourceId,

    -- * Component IDs
    resourceInfoComponentId,
) where

import Control.Concurrent.STM (STM, TVar, newTVar)
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import System.Agents.OS.Core.Types (
    AgentId,
    Component (..),
    ComponentTypeId (..),
    ConversationId,
    ResourceId (..),
    ToolCallId,
    ToolboxId,
    TurnId,
 )

-------------------------------------------------------------------------------
-- Scope Definition
-------------------------------------------------------------------------------

{- | Defines the lifetime of a resource.

The scope hierarchy determines when a resource should be cleaned up.
Resources are valid as long as their scope and all parent scopes are active.
-}
newtype ResourceScope = ResourceScope
    { scopeHierarchy :: [ScopeLevel]
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

{- | Individual scope levels in the hierarchy.

The hierarchy from broadest to narrowest:
1. ProgramScope - Global, lives for program duration
2. AgentScope - Per-agent
3. ToolboxScope - Per-toolbox
4. ConversationScope - Per-conversation
5. TurnScope - Per-turn
6. ToolCallScope - Per-toolcall (narrowest)
-}
data ScopeLevel
    = ProgramScope
    | AgentScope AgentId
    | ToolboxScope ToolboxId
    | ConversationScope ConversationId
    | TurnScope TurnId
    | ToolCallScope ToolCallId
    deriving (Eq, Ord, Show, Generic)

instance FromJSON ScopeLevel
instance ToJSON ScopeLevel
instance Hashable ScopeLevel

{- | Convert a ResourceScope to a list of ScopeLevels.

The list is ordered from broadest to narrowest scope.
-}
scopeLevelToList :: ResourceScope -> [ScopeLevel]
scopeLevelToList = scopeHierarchy

{- | Check if a resource scope is valid within a given active scope path.

A resource is valid if:
1. Its scope path is a prefix of or equal to the active scope path
2. All scope levels in the resource's path match the corresponding levels in the active path

Example:
* Resource scope: [ProgramScope, AgentScope agent1]
* Active scopes: [ProgramScope, AgentScope agent1, ConversationScope conv1]
* Result: Valid (resource scope is a prefix of active scopes)

* Resource scope: [ProgramScope, AgentScope agent1]
* Active scopes: [ProgramScope, AgentScope agent2]
* Result: Invalid (agent IDs don't match)
-}
isScopeValid :: ResourceScope -> [ScopeLevel] -> Bool
isScopeValid (ResourceScope resourceScopes) activeScopes =
    go resourceScopes activeScopes
  where
    go :: [ScopeLevel] -> [ScopeLevel] -> Bool
    go [] _ = True -- Empty resource scope is always valid
    go _ [] = False -- Resource scope longer than active scopes
    go (r : rs) (a : as)
        | r == a = go rs as -- Match, continue checking
        | otherwise = False -- Mismatch, invalid

-------------------------------------------------------------------------------
-- Resource Components
-------------------------------------------------------------------------------

{- | Metadata about a managed resource.

This component tracks information about a resource without holding
a direct reference to the actual resource data (which may have
different lifetimes and cleanup requirements).
-}
data ResourceInfo = ResourceInfo
    { resourceId :: ResourceId
    , resourceScope :: ResourceScope
    , resourceType :: ResourceType
    , resourceCreatedAt :: UTCTime
    }
    deriving (Show, Generic)

instance FromJSON ResourceInfo
instance ToJSON ResourceInfo

-- | Component ID for ResourceInfo (allocated as ComponentTypeId 10 per spec)
resourceInfoComponentId :: ComponentTypeId
resourceInfoComponentId = ComponentTypeId 10

instance Component ResourceInfo where
    componentId _ = resourceInfoComponentId

{- | Types of resources that can be managed.

Each resource type has its own configuration and initialization logic.
The actual resource data is stored separately as a Component.
-}
data ResourceType
    = SqliteResource SqliteConfig
    | LuaResource LuaConfig
    | HttpManagerResource HttpManagerConfig
    | ProcessResource ProcessConfig
    deriving (Show, Eq, Generic)

instance FromJSON ResourceType
instance ToJSON ResourceType

-- | Configuration for SQLite resources
data SqliteConfig = SqliteConfig
    { sqlitePath :: FilePath
    , sqliteAccessMode :: SqliteAccessMode
    }
    deriving (Show, Eq, Generic)

instance FromJSON SqliteConfig
instance ToJSON SqliteConfig

-- | Access mode for SQLite connections
data SqliteAccessMode
    = SerialAccess
    | ConcurrentReads
    deriving (Show, Eq, Generic)

instance FromJSON SqliteAccessMode
instance ToJSON SqliteAccessMode

-- | Configuration for Lua resources
data LuaConfig = LuaConfig
    { luaMaxMemoryMB :: Int
    , luaMaxExecutionTimeSeconds :: Int
    , luaSandboxed :: Bool
    }
    deriving (Show, Eq, Generic)

instance FromJSON LuaConfig
instance ToJSON LuaConfig

-- | Configuration for HTTP manager resources
data HttpManagerConfig = HttpManagerConfig
    { httpPoolSize :: Int
    , httpConnectionTimeoutSeconds :: Int
    , httpResponseTimeoutSeconds :: Int
    }
    deriving (Show, Eq, Generic)

instance FromJSON HttpManagerConfig
instance ToJSON HttpManagerConfig

-- | Configuration for process resources
data ProcessConfig = ProcessConfig
    { processCommand :: Text
    , processWorkingDir :: Maybe FilePath
    , processEnvironment :: [(Text, Text)]
    }
    deriving (Show, Eq, Generic)

instance FromJSON ProcessConfig
instance ToJSON ProcessConfig

{- | A handle to an actual resource.

The handle contains:
* The resource ID for identification
* A cleanup action to release the resource
* An access function to use the resource

The access function ensures that the resource can only be accessed
through a controlled interface, allowing for proper lifecycle management.
-}
data ResourceHandle = ResourceHandle
    { handleId :: ResourceId
    -- ^ Unique identifier for this resource handle
    , handleCleanup :: IO ()
    -- ^ Action to clean up the resource. Must be idempotent.
    , handleAccess :: forall a. (ResourceAccessor -> IO a) -> IO a
    {- ^ Function to access the resource. The accessor is only valid
    during the execution of the provided function.
    -}
    }

{- | Opaque type for accessing a resource.
The actual type is determined by the resource type.
-}
newtype ResourceAccessor = ResourceAccessor
    { unResourceAccessor :: ()
    }

-- This is a placeholder. In practice, specific resource modules
-- will define their own accessor types (e.g., SqliteAccessor).

{- | Registry for tracking all resources.

The registry maintains a map of resource IDs to their handles,
allowing for:
* Lookup of resources by ID
* Cleanup of all resources in a scope
* Concurrent access via STM
-}
data ResourceRegistry = ResourceRegistry
    { registryHandles :: TVar (HashMap ResourceId ResourceHandle)
    , registryCounter :: TVar Int
    }

-- | Create a new, empty resource registry.
newResourceRegistry :: STM ResourceRegistry
newResourceRegistry = do
    handles <- newTVar HashMap.empty
    counter <- newTVar 0
    pure ResourceRegistry{registryHandles = handles, registryCounter = counter}

{- | Generate a new unique ResourceId.

This generates a sequential integer-based ID within the registry.
For truly unique IDs across restarts, consider combining with
a UUID or timestamp.
-}
generateResourceId :: ResourceRegistry -> ResourceId -> STM ResourceId
-- Note: We take a ResourceId as input to use its EntityId for the base,
-- but in practice we'd want to generate a new EntityId.
-- For now, we just return the provided ID unchanged.
-- A real implementation would generate a fresh EntityId.
generateResourceId _registry rid = pure rid

{- | Context for resource operations.

The context carries:
* The current scope path (for creating resources in the correct scope)
* The registry (for tracking resources)
-}
data ResourceContext = ResourceContext
    { contextScope :: [ScopeLevel]
    , contextRegistry :: ResourceRegistry
    }
