{-# LANGUAGE LambdaCase #-}

{- |
STM-based lock implementations for concurrent resource access.

This module provides:
* Read-write lock (RWLock) with reader/writer semantics
* Exclusive lock using TMVar
* Pool-based lock using TBQueue
* Utility functions for acquiring/releasing locks

== Thread Safety

All operations are STM-based and composable. They can be combined with
other STM operations atomically.

== Fairness Considerations

* 'RWLock': Current implementation can starve writers. Readers that
  continuously acquire and release the lock can prevent writers from
  acquiring it. A writer-priority variant is possible if needed.

* 'PoolLock': TBQueue provides FIFO ordering for blocked threads.

* 'ExclusiveLock': TMVar provides FIFO ordering for blocked threads.

== Performance Notes

* 'RWLock': Best for read-heavy workloads. Each acquisition requires
  multiple TVar operations.

* 'ExclusiveLock': Simple and fast. Single TVar operation.

* 'PoolLock': Good for managing limited resources. Bounded queue
  prevents memory exhaustion.
-}
module System.Agents.OS.Concurrent.Locks (
    -- * RWLock (Read-Write Lock)
    RWLock,
    newRWLock,
    acquireRead,
    acquireReadSTM,
    releaseRead,
    releaseReadSTM,
    acquireWrite,
    acquireWriteSTM,
    releaseWrite,
    releaseWriteSTM,
    withReadLock,
    withWriteLock,

    -- * Exclusive Lock
    newExclusiveLock,
    withExclusiveLock,
    withExclusiveLockSTM,

    -- * Pool Lock
    newPoolLock,
    withPoolLock,
    withPoolLockSTM,

    -- * Lock Utilities
    tryAcquireRead,
    tryAcquireWrite,
    tryAcquireExclusive,
    tryAcquirePool,
) where

import Control.Concurrent.STM (
    STM,
    TBQueue,
    TMVar,
    TVar,
    atomically,
    isEmptyTBQueue,
    isEmptyTMVar,
    newEmptyTMVar,
    newTBQueue,
    newTVar,
    putTMVar,
    readTBQueue,
    readTVar,
    takeTMVar,
    writeTBQueue,
    writeTVar,
 )
import Control.Exception (bracket_, onException)
import Control.Monad (void, when)
import Numeric.Natural (Natural)

import System.Agents.OS.Concurrent.Types (
    RWLock (..),
    ResourceToken (..),
 )
import System.Agents.OS.Core.Types (ResourceId)

-------------------------------------------------------------------------------
-- RWLock Implementation
-------------------------------------------------------------------------------

{- | Create a new read-write lock.

The lock is initially in an unlocked state (no readers or writers).
-}
newRWLock :: STM RWLock
newRWLock = do
    readers <- newTVar 0
    writer <- newTVar False
    waitQueue <- newTVar []
    pure RWLock{rwLockReaders = readers, rwLockWriter = writer, rwLockWaitQueue = waitQueue}

{- | Acquire a read lock (STM version).

Blocks until:
* No writer holds the lock
* No writer is waiting (fairness preference)

After acquisition, the reader count is incremented.
-}
acquireReadSTM :: RWLock -> STM ()
acquireReadSTM lock = do
    hasWriter <- readTVar (rwLockWriter lock)
    if hasWriter
        then do
            -- Create a wait slot and block
            waitVar <- newEmptyTMVar
            modifyTVar (rwLockWaitQueue lock) (waitVar :)
            takeTMVar waitVar
            -- Recheck after wakeup
            acquireReadSTM lock
        else do
            -- Safe to acquire read lock
            modifyTVar (rwLockReaders lock) (+ 1)

{- | Acquire a read lock (IO version).

Convenience wrapper around 'acquireReadSTM'.
-}
acquireRead :: RWLock -> IO ()
acquireRead = atomically . acquireReadSTM

{- | Release a read lock (STM version).

Decrements the reader count. If the count reaches zero and there are
waiting writers, signals the next waiter.
-}
releaseReadSTM :: RWLock -> STM ()
releaseReadSTM lock = do
    readers <- readTVar (rwLockReaders lock)
    when (readers > 0) $ do
        let newReaders = readers - 1
        writeTVar (rwLockReaders lock) newReaders
        -- If no more readers, signal waiting writers
        when (newReaders == 0) $ do
            waiters <- readTVar (rwLockWaitQueue lock)
            case reverse waiters of
                [] -> pure ()
                (w : ws) -> do
                    writeTVar (rwLockWaitQueue lock) (reverse ws)
                    putTMVar w ()

{- | Release a read lock (IO version).

Convenience wrapper around 'releaseReadSTM'.
-}
releaseRead :: RWLock -> IO ()
releaseRead = atomically . releaseReadSTM

{- | Try to acquire a read lock without blocking.

Returns True if the lock was acquired, False otherwise.
-}
tryAcquireRead :: RWLock -> STM Bool
tryAcquireRead lock = do
    hasWriter <- readTVar (rwLockWriter lock)
    if hasWriter
        then pure False
        else do
            modifyTVar (rwLockReaders lock) (+ 1)
            pure True

{- | Acquire a write lock (STM version).

Blocks until:
* No writer holds the lock
* No readers hold the lock

After acquisition, the writer flag is set.
-}
acquireWriteSTM :: RWLock -> STM ()
acquireWriteSTM lock = do
    hasWriter <- readTVar (rwLockWriter lock)
    readers <- readTVar (rwLockReaders lock)
    if hasWriter || readers > 0
        then do
            -- Create a wait slot and block
            waitVar <- newEmptyTMVar
            modifyTVar (rwLockWaitQueue lock) (waitVar :)
            takeTMVar waitVar
            -- Recheck after wakeup
            acquireWriteSTM lock
        else do
            -- Safe to acquire write lock
            writeTVar (rwLockWriter lock) True

{- | Acquire a write lock (IO version).

Convenience wrapper around 'acquireWriteSTM'.
-}
acquireWrite :: RWLock -> IO ()
acquireWrite = atomically . acquireWriteSTM

{- | Release a write lock (STM version).

Clears the writer flag and signals all waiting threads (both readers
and writers). The first waiter to retry will acquire the lock.
-}
releaseWriteSTM :: RWLock -> STM ()
releaseWriteSTM lock = do
    writeTVar (rwLockWriter lock) False
    -- Signal all waiting threads
    waiters <- readTVar (rwLockWaitQueue lock)
    case reverse waiters of
        [] -> pure ()
        (w : ws) -> do
            writeTVar (rwLockWaitQueue lock) (reverse ws)
            putTMVar w ()

{- | Release a write lock (IO version).

Convenience wrapper around 'releaseWriteSTM'.
-}
releaseWrite :: RWLock -> IO ()
releaseWrite = atomically . releaseWriteSTM

{- | Try to acquire a write lock without blocking.

Returns True if the lock was acquired, False otherwise.
-}
tryAcquireWrite :: RWLock -> STM Bool
tryAcquireWrite lock = do
    hasWriter <- readTVar (rwLockWriter lock)
    readers <- readTVar (rwLockReaders lock)
    if hasWriter || readers > 0
        then pure False
        else do
            writeTVar (rwLockWriter lock) True
            pure True

{- | Execute an action with a read lock held.

The lock is acquired before the action and released after (even if
an exception is thrown).

Example:
@
withReadLock rwLock $ do
    -- Perform read operations
    readData
@
-}
withReadLock :: RWLock -> IO a -> IO a
withReadLock lock action =
    bracket_
        (acquireRead lock)
        (releaseRead lock)
        action

{- | Execute an action with a write lock held.

The lock is acquired before the action and released after (even if
an exception is thrown).

Example:
@
withWriteLock rwLock $ do
    -- Perform write operations
    writeData newValue
@
-}
withWriteLock :: RWLock -> IO a -> IO a
withWriteLock lock action =
    bracket_
        (acquireWrite lock)
        (releaseWrite lock)
        action

-------------------------------------------------------------------------------
-- Exclusive Lock Implementation
-------------------------------------------------------------------------------

{- | Create a new exclusive lock.

The lock is initially empty (unlocked).
-}
newExclusiveLock :: STM (TMVar ())
newExclusiveLock = newEmptyTMVar

{- | Execute an action with an exclusive lock held.

The lock is acquired before the action and released after (even if
an exception is thrown).

Example:
@
lock <- atomically newExclusiveLock
withExclusiveLock lock $ do
    -- Exclusive access here
    performOperation
@
-}
withExclusiveLock :: TMVar () -> IO a -> IO a
withExclusiveLock lock action =
    bracket_
        (atomically $ putTMVar lock ())
        (atomically $ void $ tryTakeTMVar lock)
        action

{- | Execute an STM action with an exclusive lock held.

This variant works entirely in STM and can be composed with other
STM operations. Note that STM transactions are atomic, so the lock
is held for the entire transaction.
-}
withExclusiveLockSTM :: TMVar () -> STM a -> STM a
withExclusiveLockSTM lock action = do
    putTMVar lock ()
    result <- action
    void $ tryTakeTMVar lock
    pure result

{- | Try to acquire an exclusive lock without blocking.

Returns True if the lock was acquired, False otherwise.
-}
tryAcquireExclusive :: TMVar () -> STM Bool
tryAcquireExclusive lock = do
    empty <- isEmptyTMVar lock
    if empty
        then do
            putTMVar lock ()
            pure True
        else pure False

-------------------------------------------------------------------------------
-- Pool Lock Implementation
-------------------------------------------------------------------------------

{- | Create a new pool lock.

The pool is populated with tokens representing available resources.
Each token contains a resource ID and a release action.

Example:
@
let maxSize = 10
let resourceIds = [rid1, rid2, ..., rid10]
pool <- newPoolLock maxSize resourceIds
@
-}
newPoolLock :: Int -> [ResourceId] -> STM (TBQueue ResourceToken)
newPoolLock maxSize resourceIds = do
    pool <- newTBQueue (fromIntegral maxSize :: Natural)
    -- Create tokens for each resource
    mapM_
        ( \rid -> do
            let releaseAction = writeTBQueue pool (ResourceToken rid releaseAction)
            writeTBQueue pool (ResourceToken rid releaseAction)
        )
        resourceIds
    pure pool

{- | Execute an action with a pool token.

A token is acquired from the pool before the action and released after
(even if an exception is thrown).

Example:
@
withPoolLock pool $ \token -> do
    -- Use the resource
    let rid = tokenResource token
    accessResource rid
@
-}
withPoolLock :: TBQueue ResourceToken -> (ResourceToken -> IO a) -> IO a
withPoolLock pool action = do
    token <- atomically $ readTBQueue pool
    action token `onException` atomically (tokenRelease token)

{- | Execute an STM action with a pool token.

This variant works entirely in STM and can be composed with other
STM operations.
-}
withPoolLockSTM :: TBQueue ResourceToken -> (ResourceToken -> STM a) -> STM a
withPoolLockSTM pool action = do
    token <- readTBQueue pool
    result <- action token
    tokenRelease token
    pure result

{- | Try to acquire a pool token without blocking.

Returns Just token if available, Nothing otherwise.
-}
tryAcquirePool :: TBQueue ResourceToken -> STM (Maybe ResourceToken)
tryAcquirePool pool = do
    empty <- isEmptyTBQueue pool
    if empty
        then pure Nothing
        else Just <$> readTBQueue pool

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Modify a TVar using a pure function
modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar var f = do
    val <- readTVar var
    writeTVar var (f val)

-- | Try to take a value from a TMVar, returning Nothing if empty
tryTakeTMVar :: TMVar a -> STM (Maybe a)
tryTakeTMVar var = do
    empty <- isEmptyTMVar var
    if empty
        then pure Nothing
        else Just <$> takeTMVar var
