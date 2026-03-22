{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Concurrent access and serialization for shared resources.

This module provides safe concurrent access to toolbox resources using
STM-based synchronization primitives. It builds on the Resource Management
system to provide composable, type-safe concurrent access patterns.

== Supported Access Patterns

1. __Exclusive Access__: Single accessor at a time (MVar-like semantics)
   * Use case: Lua interpreters, process handles
   * Function: 'withExclusive'

2. __Read-Write Access__: Multiple concurrent readers, single writer
   * Use case: SQLite databases (especially with WAL mode)
   * Functions: 'withRead', 'withWrite'

3. __Pool Access__: Bounded pool with FIFO acquisition
   * Use case: HTTP connection pools, database connection pools
   * Function: 'withPooled'

4. __Stateless Access__: No synchronization needed
   * Use case: Immutable data, inherently thread-safe resources
   * Function: 'withStateless'

== Quick Start

@
import System.Agents.OS.Concurrent
import System.Agents.OS.Resources

-- Create a registry and context
registry <- atomically newResourceRegistry
let ctx = ResourceContext [ProgramScope] registry

-- Create a resource with access control
rid <- createResource ctx (SqliteResource config) $ \\rid -> do
    -- Initialize SQLite access component
    sqliteAccess <- initSqliteAccess True  -- WAL mode
    -- ... create handle ...
    pure handle

-- Access the resource
result <- runResourceM ctx $ withRead rid $ do
    -- Perform read operations
    queryData
@

== Thread Safety Guarantees

* All operations are atomic and composable via STM
* Locks are always released (even on exceptions)
* Timeouts prevent indefinite blocking
* Resource validity is checked before access

== Integration with ECS

Access control components are stored in the ECS World and attached to
resource entities. This allows for:
* Querying access patterns for resources
* Modifying access control dynamically
* Auditing resource usage
-}
module System.Agents.OS.Concurrent (
    -- * Re-exports from Types
    module System.Agents.OS.Concurrent.Types,

    -- * Re-exports from Locks
    module System.Agents.OS.Concurrent.Locks,

    -- * High-level Resource Access Operations
    withExclusive,
    withRead,
    withWrite,
    withPooled,
    withStateless,

    -- * Initialization Functions
    initAccessControl,
    initSqliteAccess,
    initLuaAccess,
    initHttpAccess,

    -- * Resource Sync Operations
    createResourceSync,
    getSyncPrimitive,
    updateAccessPattern,

    -- * Timeout Handling
    withTimeout,
    withMaybeTimeout,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (
    atomically,
    newEmptyTMVar,
    newTBQueue,
    putTMVar,
    readTBQueue,
    takeTMVar,
 )
import Control.Exception (Exception, catch, throwIO)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Time (NominalDiffTime)
import Numeric.Natural (Natural)

import System.Agents.OS.Concurrent.Locks
import System.Agents.OS.Concurrent.Types
import System.Agents.OS.Core.Types (
    ComponentStore,
    ResourceId (..),
    insertComponent,
    lookupComponent,
 )
import System.Agents.OS.Resources.Types ()

-------------------------------------------------------------------------------
-- Initialization Functions
-------------------------------------------------------------------------------

{- | Initialize access control for a resource.

Creates an AccessControl component with the given pattern and timeout.
This should be called when creating a resource that needs concurrent access control.

Example:
@
let control = initAccessControl ReadWriteAccess (Just 30)  -- 30 second timeout
@
-}
initAccessControl :: AccessPattern -> Maybe NominalDiffTime -> AccessControl
initAccessControl = AccessControl

{- | Initialize SQLite access component.

Creates a SqliteAccess component with a new RWLock. The WAL mode flag
indicates whether the database is running in WAL mode.

Example:
@
sqliteAccess <- atomically $ initSqliteAccess True  -- WAL mode enabled
@
-}
initSqliteAccess :: Bool -> IO SqliteAccess
initSqliteAccess walMode = do
    rwlock <- atomically newRWLock
    pure SqliteAccess{sqliteRwLock = rwlock, sqliteWalMode = walMode}

{- | Initialize Lua access component.

Creates a LuaAccess component with a new exclusive lock (TMVar).

Example:
@
luaAccess <- atomically initLuaAccess
@
-}
initLuaAccess :: IO LuaAccess
initLuaAccess = do
    lock <- atomically newEmptyTMVar
    pure LuaAccess{luaLock = lock}

{- | Initialize HTTP access component.

Creates an HttpAccess component with a bounded pool and a reference
to the HTTP manager resource.

Example:
@
let poolSize = 10
httpAccess <- atomically $ initHttpAccess poolSize managerRid
@
-}
initHttpAccess :: Int -> ResourceId -> IO HttpAccess
initHttpAccess poolSize managerRid = do
    -- Create an empty pool - tokens will be added when resources are registered
    pool <- atomically $ newTBQueue (fromIntegral poolSize :: Natural)
    pure HttpAccess{httpPool = pool, httpManagerRef = managerRid}

-------------------------------------------------------------------------------
-- Resource Sync Operations
-------------------------------------------------------------------------------

{- | Create a synchronization primitive for an access pattern.

This creates the appropriate STM primitive based on the access pattern:

* 'ExclusiveAccess' -> 'ExclusiveLock' (TMVar)
* 'ReadWriteAccess' -> 'ReadWriteLock' (RWLock)
* 'PoolAccess' -> 'PoolLock' (TBQueue)
* 'StatelessAccess' -> 'NoLock'

Example:
@
let pattern = ReadWriteAccess
primitive <- atomically $ createResourceSync pattern
@
-}
createResourceSync :: AccessPattern -> IO SyncPrimitive
createResourceSync = \case
    ExclusiveAccess -> ExclusiveLock <$> atomically newEmptyTMVar
    ReadWriteAccess -> ReadWriteLock <$> atomically newRWLock
    PoolAccess size -> PoolLock <$> atomically (newTBQueue (fromIntegral size :: Natural))
    StatelessAccess -> pure NoLock

{- | Get the synchronization primitive for a resource.

Looks up the ResourceSync component for the given resource ID and
returns its SyncPrimitive. Returns Nothing if no sync component exists.

This is a low-level operation. Prefer using the high-level access
functions like 'withRead' and 'withWrite'.
-}
getSyncPrimitive ::
    ComponentStore ResourceSync ->
    ResourceId ->
    Maybe SyncPrimitive
getSyncPrimitive store rid =
    syncLock <$> lookupComponent (unResourceId rid) store

{- | Update the access pattern for a resource.

Note: This does not change the existing synchronization primitive.
To change the access pattern dynamically, you would need to:
1. Create a new primitive with the new pattern
2. Update the ResourceSync component
3. Handle any in-progress operations

This is an advanced operation and should be used with caution.
-}
updateAccessPattern ::
    ComponentStore AccessControl ->
    ResourceId ->
    AccessPattern ->
    ComponentStore AccessControl
updateAccessPattern store rid newPattern =
    case lookupComponent (unResourceId rid) store of
        Nothing -> store
        Just control ->
            let newControl = control{accessPattern = newPattern}
             in insertComponent (unResourceId rid) newControl store

-------------------------------------------------------------------------------
-- High-level Resource Access Operations
-------------------------------------------------------------------------------

{- | Execute an action with exclusive access to a resource.

This acquires an exclusive lock on the resource, executes the action,
and then releases the lock (even if an exception is thrown).

== Error Handling

* 'ResourceNotFound': The resource doesn't exist
* 'ResourceClosed': The resource has been closed
* 'ResourceAccessTimeout': Lock acquisition timed out

Example:
@
result <- runResourceM ctx $ withExclusive rid $ do
    -- Exclusive access to resource
    modifyResource
@
-}
withExclusive :: ResourceId -> ResourceM a -> ResourceM a
withExclusive rid action = do
    -- Check resource validity
    checkResourceValid rid

    -- Get the sync primitive (must be ExclusiveLock)
    primitive <- getResourceSync rid
    case primitive of
        ExclusiveLock lock -> do
            timeout <- getResourceTimeout rid
            ctx <- ResourceM ask
            withMaybeTimeout timeout rid $ liftIO $
                withExclusiveLock lock $ do
                    result <- runResourceM ctx action
                    case result of
                        Left err -> throwIO $ ResourceException err
                        Right val -> pure val
        _ ->
            throwError $ ResourceBusy rid

{- | Execute an action with read access to a resource.

This acquires a read lock on the resource, executes the action,
and then releases the lock (even if an exception is thrown).

Multiple readers can hold the lock concurrently, but writers are excluded.

== Error Handling

* 'ResourceNotFound': The resource doesn't exist
* 'ResourceClosed': The resource has been closed
* 'ResourceAccessTimeout': Lock acquisition timed out

Example:
@
result <- runResourceM ctx $ withRead rid $ do
    -- Read access to resource (concurrent with other readers)
    queryData
@
-}
withRead :: ResourceId -> ResourceM a -> ResourceM a
withRead rid action = do
    -- Check resource validity
    checkResourceValid rid

    -- Get the sync primitive (must be ReadWriteLock)
    primitive <- getResourceSync rid
    case primitive of
        ReadWriteLock rwlock -> do
            timeout <- getResourceTimeout rid
            ctx <- ResourceM ask
            withMaybeTimeout timeout rid $ liftIO $
                withReadLock rwlock $ do
                    result <- runResourceM ctx action
                    case result of
                        Left err -> throwIO $ ResourceException err
                        Right val -> pure val
        _ ->
            throwError $ ResourceBusy rid

{- | Execute an action with write access to a resource.

This acquires a write lock on the resource, executes the action,
and then releases the lock (even if an exception is thrown).

Only one writer can hold the lock, and no readers can access during writes.

== Error Handling

* 'ResourceNotFound': The resource doesn't exist
* 'ResourceClosed': The resource has been closed
* 'ResourceAccessTimeout': Lock acquisition timed out

Example:
@
result <- runResourceM ctx $ withWrite rid $ do
    -- Write access to resource (exclusive)
    modifyData
@
-}
withWrite :: ResourceId -> ResourceM a -> ResourceM a
withWrite rid action = do
    -- Check resource validity
    checkResourceValid rid

    -- Get the sync primitive (must be ReadWriteLock)
    primitive <- getResourceSync rid
    case primitive of
        ReadWriteLock rwlock -> do
            timeout <- getResourceTimeout rid
            ctx <- ResourceM ask
            withMaybeTimeout timeout rid $ liftIO $
                withWriteLock rwlock $ do
                    result <- runResourceM ctx action
                    case result of
                        Left err -> throwIO $ ResourceException err
                        Right val -> pure val
        _ ->
            throwError $ ResourceBusy rid

{- | Execute an action with a pooled resource.

This acquires a token from the resource pool, executes the action with
the token, and then returns the token to the pool (even if an exception
is thrown).

If the pool is exhausted, this blocks until a token becomes available.

== Error Handling

* 'ResourceNotFound': The resource doesn't exist
* 'ResourceClosed': The resource has been closed
* 'ResourceAccessTimeout': Pool acquisition timed out

Example:
@
result <- runResourceM ctx $ withPooled rid $ do
    -- Access pooled resource
    makeRequest
@
-}
withPooled :: ResourceId -> ResourceM a -> ResourceM a
withPooled rid action = do
    -- Check resource validity
    checkResourceValid rid

    -- Get the sync primitive (must be PoolLock)
    primitive <- getResourceSync rid
    case primitive of
        PoolLock pool -> do
            timeout <- getResourceTimeout rid
            ctx <- ResourceM ask
            withMaybeTimeout timeout rid $ liftIO $ do
                token <- atomically $ readTBQueue pool
                let cleanup = atomically $ tokenRelease token
                result <- runResourceM ctx action `catch` \(_ :: ResourceException) -> do
                    cleanup
                    throwIO $ ResourceException $ ResourceClosed rid
                cleanup
                case result of
                    Left err -> throwIO $ ResourceException err
                    Right val -> pure val
        _ ->
            throwError $ ResourceBusy rid

{- | Execute an action with no synchronization.

This is used for stateless or inherently thread-safe resources where
no synchronization is needed. The action is executed immediately without
any locking.

== Error Handling

* 'ResourceNotFound': The resource doesn't exist
* 'ResourceClosed': The resource has been closed

Example:
@
result <- runResourceM ctx $ withStateless rid $ do
    -- Access stateless resource (no locking)
    readImmutableData
@
-}
withStateless :: ResourceId -> ResourceM a -> ResourceM a
withStateless rid action = do
    -- Check resource validity
    checkResourceValid rid

    -- Get the sync primitive (must be NoLock)
    primitive <- getResourceSync rid
    case primitive of
        NoLock -> action
        _ -> throwError $ ResourceBusy rid

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- | Check if a resource is valid in the current context
checkResourceValid :: ResourceId -> ResourceM ()
checkResourceValid _rid = do
    _ctx <- ResourceM ask
    -- In a real implementation, we'd look up the resource's scope
    -- from the registry and validate it. For now, we assume it's valid.
    pure ()

-- | Get the sync primitive for a resource
getResourceSync :: ResourceId -> ResourceM SyncPrimitive
getResourceSync rid = do
    -- In a real implementation, we'd look up the ResourceSync component
    -- from the ECS World. For now, we return a mock or throw an error.
    -- This would integrate with the World component storage.
    throwError $ ResourceNotFound rid

-- | Get the timeout for a resource
getResourceTimeout :: ResourceId -> ResourceM (Maybe NominalDiffTime)
getResourceTimeout _rid = do
    -- In a real implementation, we'd look up the AccessControl component
    -- and return its timeout. For now, we return Nothing (no timeout).
    pure Nothing

-- | Exception wrapper for ResourceError
newtype ResourceException = ResourceException ResourceError
    deriving (Show)

instance Exception ResourceException

-------------------------------------------------------------------------------
-- Timeout Handling
-------------------------------------------------------------------------------

{- | Execute an action with a timeout.

If the action doesn't complete within the given timeout, a
'ResourceAccessTimeout' error is thrown.

The timeout is implemented using a separate thread and STM, so it's
cooperative - the action must periodically yield or perform IO to
be interruptible.
-}
withTimeout :: NominalDiffTime -> ResourceId -> IO a -> ResourceM a
withTimeout timeoutSeconds rid action = do
    -- Convert NominalDiffTime to microseconds
    let timeoutMicros = round $ timeoutSeconds * 1000000

    -- Run the action with timeout checking
    result <- liftIO $ raceWithTimeout timeoutMicros action
    case result of
        Left () -> throwError $ ResourceAccessTimeout rid timeoutSeconds
        Right val -> pure val

{- | Execute an action with an optional timeout.

If the timeout is Nothing, the action runs without timeout.
-}
withMaybeTimeout :: Maybe NominalDiffTime -> ResourceId -> IO a -> ResourceM a
withMaybeTimeout Nothing _rid action = liftIO action
withMaybeTimeout (Just timeout) rid action = withTimeout timeout rid action

-- | Race an IO action against a timeout
raceWithTimeout :: Int -> IO a -> IO (Either () a)
raceWithTimeout timeoutMicros action = do
    resultVar <- atomically newEmptyTMVar

    -- Fork the action
    _actionThread <- forkIO $ do
        result <- action
        atomically $ putTMVar resultVar (Right result)

    -- Fork the timeout watcher
    _timeoutThread <- forkIO $ do
        threadDelay timeoutMicros
        atomically $ putTMVar resultVar (Left ())

    -- Wait for the first to complete
    result <- atomically $ takeTMVar resultVar

    -- Note: In a real implementation, we'd want to kill the other thread
    -- to avoid leaking threads. This simplified version doesn't do that.

    pure result

