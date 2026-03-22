{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Core types for concurrent resource access and serialization.

This module defines:
* Access patterns for different resource types
* Access control configuration
* Resource synchronization primitives
* Resource access monad and error types
* Resource-specific access components

== Thread Safety Guarantees

1. __ExclusiveAccess__: Only one thread can access the resource at a time.
   Implemented using 'TMVar' for composable blocking.

2. __ReadWriteAccess__: Multiple readers can access concurrently, but writers
   have exclusive access. Writers may starve if readers continuously hold the lock.

3. __PoolAccess__: Bounded pool of resources with FIFO acquisition.
   Threads block when pool is exhausted until a resource is released.

4. __StatelessAccess__: No synchronization overhead. Safe for immutable or
   inherently thread-safe resources.

== Design Tradeoffs

1. __STM vs MVar__: STM is used throughout for composability and automatic
   retry on conflict. MVar is slightly faster but less composable.

2. __Fairness__: Current RWLock implementation can starve writers. A
   writer-priority variant is available if contention becomes an issue.

3. __Timeouts__: Optional timeouts allow callers to choose between waiting
   and failing fast. 'Nothing' means wait indefinitely.

4. __Resource Token Pattern__: Pool access returns a token with embedded
   release action, ensuring resources are always returned to the pool.
-}
module System.Agents.OS.Concurrent.Types (
    -- * Access Patterns
    AccessPattern (..),
    AccessControl (..),

    -- * Synchronization State
    RWLock (..),
    ResourceSync (..),
    SyncPrimitive (..),
    ResourceToken (..),

    -- * Resource Access Monad
    ResourceM (..),
    ResourceError (..),
    ResourceContext,

    -- * Resource-Specific Access Components
    SqliteAccess (..),
    LuaAccess (..),
    HttpAccess (..),

    -- * Component IDs
    accessControlComponentId,
    resourceSyncComponentId,
    sqliteAccessComponentId,
    luaAccessComponentId,
    httpAccessComponentId,

    -- * Helper Functions
    runResourceM,
    mapResourceM,
) where

import Control.Concurrent.STM (
    STM,
    TBQueue,
    TMVar,
    TVar,
 )
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (NominalDiffTime)
import GHC.Generics (Generic)

import System.Agents.OS.Core.Types (
    Component (..),
    ComponentTypeId (..),
    ResourceId,
 )
import System.Agents.OS.Resources.Types (
    ResourceContext,
    ScopeLevel,
 )

-------------------------------------------------------------------------------
-- Access Patterns
-------------------------------------------------------------------------------

{- | Access pattern for a resource.

Determines the concurrency semantics for accessing a resource:

* 'ExclusiveAccess': Only one accessor at a time (MVar-like semantics)
* 'ReadWriteAccess': Multiple concurrent readers, single exclusive writer
* 'PoolAccess': Bounded pool of resources with FIFO acquisition
* 'StatelessAccess': No synchronization needed (e.g., immutable data)
-}
data AccessPattern
    = -- | Only one accessor at a time
      ExclusiveAccess
    | -- | Multiple readers or single writer
      ReadWriteAccess
    | -- | Connection pool with max size
      PoolAccess Int
    | StatelessAccess
    deriving
        -- \^ No synchronization needed
        (Show, Eq, Generic)

instance FromJSON AccessPattern
instance ToJSON AccessPattern

{- | Resource access control configuration.

Specifies how a resource should be accessed, including the access pattern
and optional timeout for acquisition.
-}
data AccessControl = AccessControl
    { accessPattern :: AccessPattern
    -- ^ The access pattern for this resource
    , accessTimeout :: Maybe NominalDiffTime
    -- ^ Optional timeout for resource acquisition (Nothing = wait indefinitely)
    }
    deriving (Show, Eq, Generic)

instance FromJSON AccessControl
instance ToJSON AccessControl

-- | Component ID for AccessControl (allocated as ComponentTypeId 20 per spec)
accessControlComponentId :: ComponentTypeId
accessControlComponentId = ComponentTypeId 20

instance Component AccessControl where
    componentId _ = accessControlComponentId

-------------------------------------------------------------------------------
-- Synchronization Primitives
-------------------------------------------------------------------------------

{- | STM-based read-write lock.

Supports multiple concurrent readers or a single writer. The lock uses:
* A counter for active readers
* A boolean flag for active writer
* A wait queue for blocked threads

== Writer Starvation

Current implementation can starve writers when readers continuously acquire
and release the lock. Consider using a writer-priority variant if this
becomes a problem.

== Implementation Notes

* Readers check the writer flag before acquiring
* Writers wait for all readers to finish
* The wait queue allows for fair wakeup ordering
-}
data RWLock = RWLock
    { rwLockReaders :: TVar Int
    -- ^ Number of active readers
    , rwLockWriter :: TVar Bool
    -- ^ True if a writer holds the lock
    , rwLockWaitQueue :: TVar [TMVar ()]
    -- ^ Queue of waiting threads (for fairness)
    }

instance Show RWLock where
    show _ = "RWLock{..}"

{- | Resource synchronization state.

Holds the synchronization primitive used to control access to a resource.
This is stored as a component attached to the resource's entity.

Note: This component is runtime-only and cannot be serialized to JSON
because it contains STM primitives.
-}
newtype ResourceSync = ResourceSync
    { syncLock :: SyncPrimitive
    }

instance Show ResourceSync where
    show rs = "ResourceSync{syncLock=" ++ show (syncLock rs) ++ "}"

-- | Component ID for ResourceSync (allocated as ComponentTypeId 21 per spec)
resourceSyncComponentId :: ComponentTypeId
resourceSyncComponentId = ComponentTypeId 21

instance Component ResourceSync where
    componentId _ = resourceSyncComponentId

{- | Synchronization primitive for resource access.

Each variant provides different concurrency semantics:

* 'ExclusiveLock': Single exclusive access via TMVar
* 'ReadWriteLock': Multiple readers or single writer
* 'PoolLock': Bounded pool with FIFO semantics
* 'NoLock': No synchronization (for stateless/immutable resources)

Note: This is runtime-only and cannot be serialized.
-}
data SyncPrimitive
    = -- | Exclusive access via TMVar
      ExclusiveLock (TMVar ())
    | -- | Reader-writer lock
      ReadWriteLock RWLock
    | -- | Bounded resource pool
      PoolLock (TBQueue ResourceToken)
    | -- | No synchronization needed
      NoLock

instance Show SyncPrimitive where
    show (ExclusiveLock _) = "ExclusiveLock{..}"
    show (ReadWriteLock rw) = "ReadWriteLock{" ++ show rw ++ "}"
    show (PoolLock _) = "PoolLock{..}"
    show NoLock = "NoLock"

{- | Token representing a pooled resource.

The token embeds a release action that must be called when done with
the resource. This ensures resources are always returned to the pool
(even on exceptions when used with 'bracket').

Example:
@
token <- atomically $ readTBQueue pool
bracket (pure token) (atomically . tokenRelease) $ \t -> do
    -- Use tokenResource t here
@
-}
data ResourceToken = ResourceToken
    { tokenResource :: ResourceId
    -- ^ The resource this token represents
    , tokenRelease :: STM ()
    -- ^ Action to release the token back to the pool
    }

instance Show ResourceToken where
    show rt = "ResourceToken{tokenResource=" ++ show (tokenResource rt) ++ ",..}"

-- Equality based only on resource ID since STM actions cannot be compared
instance Eq ResourceToken where
    t1 == t2 = tokenResource t1 == tokenResource t2

-------------------------------------------------------------------------------
-- Resource Access Monad
-------------------------------------------------------------------------------

{- | Error type for resource operations.

These errors can occur when attempting to access or manipulate resources:

* 'ResourceNotFound': The requested resource doesn't exist
* 'ResourceAccessTimeout': Acquisition timed out
* 'ResourceScopeInvalid': Resource not valid in current scope
* 'ResourceClosed': Resource has been closed/disposed
* 'ResourceBusy': Resource is in use and unavailable
-}
data ResourceError
    = -- | Resource not found in registry
      ResourceNotFound ResourceId
    | -- | Acquisition timed out
      ResourceAccessTimeout ResourceId NominalDiffTime
    | -- | Resource scope not valid in current context
      ResourceScopeInvalid ResourceId [ScopeLevel]
    | -- | Resource has been closed
      ResourceClosed ResourceId
    | ResourceBusy ResourceId
    deriving
        -- \^ Resource is busy
        (Show, Eq, Generic)

instance FromJSON ResourceError
instance ToJSON ResourceError

{- | Monad for resource access operations.

Provides:
* Access to resource context (ReaderT)
* Error handling (ExceptT)
* IO operations (liftIO)

This monad is used for all resource access operations that require
synchronization. It ensures consistent error handling and context access.

Example:
@
result <- runResourceM ctx $ do
    withExclusive resourceId $ do
        -- Access resource exclusively
        performOperation
@
-}
newtype ResourceM a = ResourceM
    { unResourceM :: ReaderT ResourceContext (ExceptT ResourceError IO) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadError ResourceError
        )

{- | Run a ResourceM computation.

Executes the computation with the given context, returning either
a 'ResourceError' or the successful result.
-}
runResourceM :: ResourceContext -> ResourceM a -> IO (Either ResourceError a)
runResourceM ctx (ResourceM m) = runExceptT $ runReaderT m ctx

{- | Map a function over the result of a ResourceM computation.

This is useful for transforming results while preserving the error handling.
-}
mapResourceM :: (a -> b) -> ResourceM a -> ResourceM b
mapResourceM f = fmap f

-------------------------------------------------------------------------------
-- Resource-Specific Access Components
-------------------------------------------------------------------------------

{- | Access control component for SQLite resources.

SQLite supports WAL mode for concurrent reads. This component holds
the RWLock for coordinating access and tracks whether WAL mode is enabled.

Note: This is runtime-only and cannot be serialized because it contains
STM primitives.

== WAL Mode

When WAL mode is enabled:
* Readers don't block writers
* Writers don't block readers
* Only one writer at a time

Without WAL mode, the entire database is locked during writes.
-}
data SqliteAccess = SqliteAccess
    { sqliteRwLock :: RWLock
    -- ^ Read-write lock for database access
    , sqliteWalMode :: Bool
    -- ^ True if WAL mode is enabled
    }

instance Show SqliteAccess where
    show sa = "SqliteAccess{..,sqliteWalMode=" ++ show (sqliteWalMode sa) ++ "}"

instance Component SqliteAccess where
    componentId _ = sqliteAccessComponentId

-- | Component ID for SqliteAccess (allocated as ComponentTypeId 22 per spec)
sqliteAccessComponentId :: ComponentTypeId
sqliteAccessComponentId = ComponentTypeId 22

{- | Access control component for Lua resources.

Lua interpreters require exclusive access per execution because:
* Lua states are not thread-safe
* Globals can be modified during execution
* Coroutine state must be preserved

This component holds a TMVar for exclusive access.

Note: This is runtime-only and cannot be serialized because it contains
STM primitives.
-}
newtype LuaAccess = LuaAccess
    { luaLock :: TMVar ()
    -- ^ Exclusive access lock
    }

instance Show LuaAccess where
    show _ = "LuaAccess{..}"

instance Component LuaAccess where
    componentId _ = luaAccessComponentId

-- | Component ID for LuaAccess (allocated as ComponentTypeId 23 per spec)
luaAccessComponentId :: ComponentTypeId
luaAccessComponentId = ComponentTypeId 23

{- | Access control component for HTTP resources.

HTTP connection managers are inherently thread-safe and can be shared
across concurrent requests. This component holds a pool of request slots
for connection limiting and a reference to the underlying manager resource.

Note: This is runtime-only and cannot be serialized because it contains
STM primitives.

== Pool Semantics

The pool limits the number of concurrent requests to prevent overload:
* Each request acquires a token from the pool
* Tokens are returned when the response is consumed
* Requests block when the pool is exhausted
-}
data HttpAccess = HttpAccess
    { httpPool :: TBQueue ResourceToken
    -- ^ Connection pool with bounded capacity
    , httpManagerRef :: ResourceId
    -- ^ Reference to the HTTP manager resource
    }

instance Show HttpAccess where
    show ha = "HttpAccess{..,httpManagerRef=" ++ show (httpManagerRef ha) ++ "}"

instance Component HttpAccess where
    componentId _ = httpAccessComponentId

-- | Component ID for HttpAccess (allocated as ComponentTypeId 24 per spec)
httpAccessComponentId :: ComponentTypeId
httpAccessComponentId = ComponentTypeId 24
