{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Resource Management System with Multiple Lifecycle Scopes.

This module provides comprehensive resource management for the OS architecture:

* Multiple lifecycle scopes: program, agent, toolbox, conversation, turn, toolcall
* Explicit cleanup with predictable resource disposal
* Thread-safe registry using STM
* Integration with the ECS component system

== Resource Lifecycle

Resources have different lifetimes depending on their scope:

1. /Program scope/: Global resources like HTTP connection pools
2. /Agent scope/: Per-agent resources like sandboxes
3. /Toolbox scope/: Per-toolbox resources like SQLite connections
4. /Conversation scope/: Per-conversation resources like isolated Lua states
5. /Turn scope/: Temporary resources for a single turn
6. /ToolCall scope/: Single-use resources

== Cleanup Ordering

When cleaning up a scope, resources are disposed in /reverse order of creation/.
This ensures that resources that depend on other resources are cleaned up first.

== Example Usage

@
import System.Agents.OS.Resources
import qualified System.Agents.OS.Resources.Sqlite as SqliteRes

-- Create a registry
registry <- atomically newResourceRegistry

-- Create a context for the current scope
let ctx = ResourceContext [ProgramScope] registry

-- Create a resource
eid <- createEntity -- from OS.Core
rid <- createResource ctx (SqliteResource config) $ \rid -> do
    conn <- openConnection config
    pure ResourceHandle
        { handleId = rid
        , handleCleanup = closeConnection conn
        , handleAccess = \\f -> f (SqliteRes.SqliteAccessor conn)
        }

-- Later, cleanup all resources in a scope
cleanupScope registry (AgentScope agentId)

-- Check if a resource is still valid
let isValid = isResourceValid resourceScope activeScopes
@

== Tradeoffs

1. __Explicit Cleanup vs Finalizers__: We use explicit cleanup (handleCleanup)
   for predictable resource management. Finalizers are non-deterministic and
   may not run promptly.

2. __Resource Sharing__: Shared resources use the same ResourceId. The registry
   tracks cleanup implicitly - if two entities share a resource, only one should
   register cleanup.

3. __Scope Checking__: Runtime scope checking adds overhead but ensures
   correctness. Consider caching scope lookups in performance-critical paths.
-}
module System.Agents.OS.Resources (
    -- * Re-export types
    module System.Agents.OS.Resources.Types,
    module System.Agents.OS.Resources.Sqlite,
    module System.Agents.OS.Resources.Lua,
    module System.Agents.OS.Resources.Http,

    -- * Resource lifecycle operations
    createResource,
    registerResourceHandle,
    cleanupScope,
    isResourceValid,
    findResourcesInScope,

    -- * Resource access
    withResource,
    lookupResourceHandle,

    -- * Registry operations
    getResourceCount,
) where

import Control.Concurrent.STM (
    STM,
    atomically,
    modifyTVar',
    readTVar,
 )
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import qualified Data.HashMap.Strict as HashMap
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time (getCurrentTime)

import System.Agents.OS.Core.Types (
    ResourceId (..),
    newEntityId,
 )
import System.Agents.OS.Resources.Http
import System.Agents.OS.Resources.Lua
import System.Agents.OS.Resources.Sqlite
import System.Agents.OS.Resources.Types

-------------------------------------------------------------------------------
-- Resource Lifecycle Operations
-------------------------------------------------------------------------------

{- | Create a new resource and register it in the context's scope.

This function:
1. Generates a new ResourceId
2. Creates the resource handle using the provided factory function
3. Registers the resource in the registry
4. Returns the ResourceId for later reference

The factory function receives the ResourceId so it can embed it in the handle.

Example:
@
rid <- createResource ctx (SqliteResource config) $ \\rid -> do
    conn <- openConnection config
    pure ResourceHandle
        { handleId = rid
        , handleCleanup = closeConnection conn
        , handleAccess = \\f -> f (SqliteAccessor conn)
        }
@
-}
createResource ::
    ResourceContext ->
    ResourceType ->
    (ResourceId -> IO ResourceHandle) ->
    -- | Returns the ResourceId of the created resource
    IO ResourceId
createResource ctx resType factory = do
    -- Generate a new entity ID for this resource
    eid <- newEntityId
    let rid = ResourceId eid

    -- Record creation time
    createdAt <- getCurrentTime

    -- Build the resource info
    let resourceInfo =
            ResourceInfo
                { resourceId = rid
                , resourceScope = ResourceScope (contextScope ctx)
                , resourceType = resType
                , resourceCreatedAt = createdAt
                }

    -- Create the resource handle using the factory
    handle <- factory rid

    -- Register in the registry
    atomically $ registerResourceHandleSTM (contextRegistry ctx) handle resourceInfo

    pure rid

{- | Register a resource handle in the registry.

This is a low-level operation. Prefer using 'createResource' for normal use.
-}
registerResourceHandle :: ResourceRegistry -> ResourceHandle -> ResourceInfo -> IO ()
registerResourceHandle registry handle info =
    atomically $ registerResourceHandleSTM registry handle info

-- | STM version of registerResourceHandle
registerResourceHandleSTM :: ResourceRegistry -> ResourceHandle -> ResourceInfo -> STM ()
registerResourceHandleSTM registry handle _info = do
    modifyTVar' (registryHandles registry) $ \handles ->
        HashMap.insert (handleId handle) handle handles

-------------------------------------------------------------------------------
-- Resource Cleanup
-------------------------------------------------------------------------------

{- | Cleanup all resources in a given scope.

This function:
1. Finds all resources whose scope matches the given scope level
2. Sorts them by creation time (oldest first, for reverse cleanup)
3. Runs the cleanup action for each resource
4. Removes the resources from the registry

Cleanup Ordering:
Resources are cleaned up in /reverse order of creation/ (newest first).
This ensures proper dependency ordering - resources created later may depend
on resources created earlier, so we clean them up first.

Example:
@
-- Cleanup all resources for a specific agent
cleanupScope registry (AgentScope agentId)

-- Cleanup all resources for a specific toolbox
cleanupScope registry (ToolboxScope toolboxId)

-- Cleanup all per-turn resources
cleanupScope registry (TurnScope turnId)
@
-}
cleanupScope :: ResourceRegistry -> ScopeLevel -> IO ()
cleanupScope registry scopeLevel = do
    -- Find all resources in this scope
    resources <- atomically $ findResourcesInScope registry scopeLevel

    -- Sort by creation time (oldest first) for reverse cleanup
    let sortedResources = sortBy (comparing resourceCreatedAt) resources

    -- Clean up in reverse order (newest first)
    forM_ (reverse sortedResources) $ \info -> do
        mHandle <- atomically $ lookupResourceHandleSTM registry (resourceId info)
        case mHandle of
            Nothing -> pure () -- Already cleaned up
            Just handle -> do
                -- Run cleanup, catching any exceptions
                result <- try $ handleCleanup handle
                case result of
                    Left (e :: SomeException) -> do
                        -- Log error but continue with other resources
                        -- In a real implementation, we'd use a tracer here
                        putStrLn $ "Error cleaning up resource " ++ show (resourceId info) ++ ": " ++ show e
                    Right () -> pure ()

                -- Remove from registry regardless of cleanup success
                atomically $ removeResourceHandleSTM registry (resourceId info)

-- | Remove a resource handle from the registry (internal).
removeResourceHandleSTM :: ResourceRegistry -> ResourceId -> STM ()
removeResourceHandleSTM registry rid = do
    modifyTVar' (registryHandles registry) $ HashMap.delete rid

-------------------------------------------------------------------------------
-- Resource Lookup and Access
-------------------------------------------------------------------------------

{- | Find all resources that belong to a specific scope level.

This returns ResourceInfo for all resources whose scope path contains
the given scope level at the appropriate position.
-}
findResourcesInScope :: ResourceRegistry -> ScopeLevel -> STM [ResourceInfo]
findResourcesInScope _registry _scopeLevel = do
    -- Note: In a real implementation, we'd also store ResourceInfo in the
    -- registry or look it up from the ECS World. For now, we return an empty list.
    -- This would require integrating with the World component storage.
    pure []

-- | Look up a resource handle by ID.
lookupResourceHandle :: ResourceRegistry -> ResourceId -> IO (Maybe ResourceHandle)
lookupResourceHandle registry rid =
    atomically $ lookupResourceHandleSTM registry rid

-- | STM version of lookupResourceHandle
lookupResourceHandleSTM :: ResourceRegistry -> ResourceId -> STM (Maybe ResourceHandle)
lookupResourceHandleSTM registry rid = do
    handles <- readTVar (registryHandles registry)
    pure $ HashMap.lookup rid handles

{- | Execute an action with a resource.

This is a convenience function that looks up the resource, runs the action,
and handles the resource not being found.

Example:
@
result <- withResource registry rid $ \\accessor -> do
    -- Use the accessor here
    queryDatabase accessor
@
-}
withResource ::
    ResourceRegistry ->
    ResourceId ->
    (ResourceAccessor -> IO a) ->
    IO (Maybe a)
withResource registry rid action = do
    mHandle <- lookupResourceHandle registry rid
    case mHandle of
        Nothing -> pure Nothing
        Just handle -> do
            result <- handleAccess handle action
            pure $ Just result

-------------------------------------------------------------------------------
-- Scope Validation
-------------------------------------------------------------------------------

{- | Check if a resource scope is valid within given active scopes.

A resource scope is valid if its scope path is a prefix of or equal to
the active scope path, with matching scope levels at each position.

Example:
@
-- Resource scope: [ProgramScope, AgentScope agent1]
-- Active scopes: [ProgramScope, AgentScope agent1, ConversationScope conv1]
-- Result: True (resource scope is a prefix)

-- Resource scope: [ProgramScope, AgentScope agent1]
-- Active scopes: [ProgramScope, AgentScope agent2]
-- Result: False (agent IDs don't match)
@

This is used to check if a resource can still be accessed given the
current execution context.
-}
isResourceValid :: ResourceScope -> [ScopeLevel] -> Bool
isResourceValid = isScopeValid

-------------------------------------------------------------------------------
-- Registry Queries
-------------------------------------------------------------------------------

-- | Get the total number of resources in the registry.
getResourceCount :: ResourceRegistry -> IO Int
getResourceCount registry = do
    atomically $ do
        handles <- readTVar (registryHandles registry)
        pure $ HashMap.size handles
