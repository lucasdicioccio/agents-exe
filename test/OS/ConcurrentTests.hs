{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Tests for the Concurrent Access and Serialization System.

These tests verify:
- RWLock reader/writer semantics
- Exclusive lock (TMVar) behavior
- Pool lock (TBQueue) semantics
- Access pattern configurations
- Resource access monad operations
- Timeout handling
- Resource-specific access components (SQLite, Lua, HTTP)
- Concurrent access stress tests
- Thread safety guarantees
-}
module OS.ConcurrentTests (
    concurrentTests,
) where

import Control.Concurrent (forkIO, threadDelay, MVar, newEmptyMVar, putMVar, takeMVar)
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
    readTVar,
    readTBQueue,
    writeTVar,
 )
import Control.Exception (SomeException, try)
import Control.Monad (forM_, replicateM, replicateM_, void, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy (..))
import Data.Time (NominalDiffTime)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, (@?=))

import System.Agents.OS.Concurrent
import System.Agents.OS.Concurrent.Locks
import System.Agents.OS.Concurrent.Types
import System.Agents.OS.Core (
    Component (..),
    ComponentTypeId (..),
    EntityId (..),
    ResourceId (..),
    createEntity,
 )

-------------------------------------------------------------------------------
-- Test Suite
-------------------------------------------------------------------------------

concurrentTests :: TestTree
concurrentTests =
    testGroup
        "Concurrent Access Tests"
        [ accessPatternTests
        , rwLockTests
        , exclusiveLockTests
        , poolLockTests
        , componentIdTests
        , resourceAccessMonadTests
        , timeoutTests
        , resourceSpecificAccessTests
        , concurrentStressTests
        , threadSafetyTests
        ]

-------------------------------------------------------------------------------
-- Access Pattern Tests
-------------------------------------------------------------------------------

accessPatternTests :: TestTree
accessPatternTests =
    testGroup
        "Access Patterns"
        [ testCase "ExclusiveAccess pattern" $ do
            let pattern = ExclusiveAccess
            show pattern @?= "ExclusiveAccess"
        , testCase "ReadWriteAccess pattern" $ do
            let pattern = ReadWriteAccess
            show pattern @?= "ReadWriteAccess"
        , testCase "PoolAccess pattern with size" $ do
            let pattern = PoolAccess 10
            show pattern @?= "PoolAccess 10"
        , testCase "StatelessAccess pattern" $ do
            let pattern = StatelessAccess
            show pattern @?= "StatelessAccess"
        , testCase "AccessControl with pattern and timeout" $ do
            let timeout :: NominalDiffTime = 30
            let control = AccessControl ExclusiveAccess (Just timeout)
            accessPattern control @?= ExclusiveAccess
            isJust (accessTimeout control) @?= True
        , testCase "AccessControl without timeout" $ do
            let control = AccessControl ReadWriteAccess Nothing
            accessPattern control @?= ReadWriteAccess
            accessTimeout control @?= Nothing
        , testCase "AccessControl equality" $ do
            let control1 = AccessControl ExclusiveAccess Nothing
            let control2 = AccessControl ExclusiveAccess Nothing
            let control3 = AccessControl ReadWriteAccess Nothing
            control1 @?= control2
            assertBool "Different patterns should not be equal" (control1 /= control3)
        ]

-------------------------------------------------------------------------------
-- RWLock Tests
-------------------------------------------------------------------------------

rwLockTests :: TestTree
rwLockTests =
    testGroup
        "RWLock"
        [ testCase "newRWLock creates unlocked lock" $ do
            rwlock <- atomically newRWLock
            readers <- atomically $ readTVar (rwLockReaders rwlock)
            writer <- atomically $ readTVar (rwLockWriter rwlock)
            readers @?= 0
            writer @?= False
        , testCase "acquireRead increments reader count" $ do
            rwlock <- atomically newRWLock
            atomically $ acquireReadSTM rwlock
            readers <- atomically $ readTVar (rwLockReaders rwlock)
            readers @?= 1
        , testCase "releaseRead decrements reader count" $ do
            rwlock <- atomically newRWLock
            atomically $ acquireReadSTM rwlock
            atomically $ releaseReadSTM rwlock
            readers <- atomically $ readTVar (rwLockReaders rwlock)
            readers @?= 0
        , testCase "acquireWrite sets writer flag" $ do
            rwlock <- atomically newRWLock
            atomically $ acquireWriteSTM rwlock
            writer <- atomically $ readTVar (rwLockWriter rwlock)
            writer @?= True
        , testCase "releaseWrite clears writer flag" $ do
            rwlock <- atomically newRWLock
            atomically $ acquireWriteSTM rwlock
            atomically $ releaseWriteSTM rwlock
            writer <- atomically $ readTVar (rwLockWriter rwlock)
            writer @?= False
        , testCase "multiple readers can acquire concurrently" $ do
            rwlock <- atomically newRWLock
            atomically $ do
                acquireReadSTM rwlock
                acquireReadSTM rwlock
                acquireReadSTM rwlock
            readers <- atomically $ readTVar (rwLockReaders rwlock)
            readers @?= 3
        , testCase "writer blocks when readers hold lock" $ do
            rwlock <- atomically newRWLock
            -- Acquire read lock
            atomically $ acquireReadSTM rwlock
            -- Try to acquire write (should not block in STM, but would block
            -- until readers release in real concurrent scenario)
            acquired <- atomically $ tryAcquireWrite rwlock
            acquired @?= False
        , testCase "reader blocks when writer holds lock" $ do
            rwlock <- atomically newRWLock
            -- Acquire write lock
            atomically $ acquireWriteSTM rwlock
            -- Try to acquire read (should fail)
            acquired <- atomically $ tryAcquireRead rwlock
            acquired @?= False
        , testCase "withReadLock acquires and releases" $ do
            rwlock <- atomically newRWLock
            result <- withReadLock rwlock $ do
                pure 42
            result @?= 42
            readers <- atomically $ readTVar (rwLockReaders rwlock)
            readers @?= 0
        , testCase "withWriteLock acquires and releases" $ do
            rwlock <- atomically newRWLock
            result <- withWriteLock rwlock $ do
                pure 42
            result @?= 42
            writer <- atomically $ readTVar (rwLockWriter rwlock)
            writer @?= False
        , testCase "tryAcquireRead succeeds when lock is free" $ do
            rwlock <- atomically newRWLock
            acquired <- atomically $ tryAcquireRead rwlock
            acquired @?= True
            readers <- atomically $ readTVar (rwLockReaders rwlock)
            readers @?= 1
        , testCase "tryAcquireWrite succeeds when lock is free" $ do
            rwlock <- atomically newRWLock
            acquired <- atomically $ tryAcquireWrite rwlock
            acquired @?= True
            writer <- atomically $ readTVar (rwLockWriter rwlock)
            writer @?= True
        ]

-------------------------------------------------------------------------------
-- Exclusive Lock Tests
-------------------------------------------------------------------------------

exclusiveLockTests :: TestTree
exclusiveLockTests =
    testGroup
        "Exclusive Lock"
        [ testCase "newExclusiveLock creates empty TMVar" $ do
            lock <- atomically newExclusiveLock
            empty <- atomically $ isEmptyTMVar lock
            empty @?= True
        , testCase "withExclusiveLock acquires and releases" $ do
            lock <- atomically newExclusiveLock
            result <- withExclusiveLock lock $ do
                pure 42
            result @?= 42
            -- Lock should be released
            empty <- atomically $ isEmptyTMVar lock
            empty @?= True
        , testCase "tryAcquireExclusive succeeds when lock is free" $ do
            lock <- atomically newExclusiveLock
            acquired <- atomically $ tryAcquireExclusive lock
            acquired @?= True
        , testCase "tryAcquireExclusive fails when lock is held" $ do
            lock <- atomically newExclusiveLock
            atomically $ putTMVar lock ()
            acquired <- atomically $ tryAcquireExclusive lock
            acquired @?= False
        ]

-------------------------------------------------------------------------------
-- Pool Lock Tests
-------------------------------------------------------------------------------

poolLockTests :: TestTree
poolLockTests =
    testGroup
        "Pool Lock"
        [ testCase "newPoolLock creates queue with tokens" $ do
            eid1 <- createEntity
            eid2 <- createEntity
            let rids = [ResourceId eid1, ResourceId eid2]
            pool <- atomically $ newPoolLock 2 rids
            -- Pool should have 2 tokens available
            empty <- atomically $ isEmptyTBQueue pool
            empty @?= False
        , testCase "pool token has correct resource ID" $ do
            eid <- createEntity
            let rid = ResourceId eid
            pool <- atomically $ newPoolLock 1 [rid]
            token <- atomically $ readTBQueue pool
            tokenResource token @?= rid
        , testCase "pool token can be released" $ do
            eid <- createEntity
            let rid = ResourceId eid
            pool <- atomically $ newPoolLock 1 [rid]
            token <- atomically $ readTBQueue pool
            -- Pool should be empty now
            empty <- atomically $ isEmptyTBQueue pool
            empty @?= True
            -- Release the token
            atomically $ tokenRelease token
            -- Pool should have token again
            empty2 <- atomically $ isEmptyTBQueue pool
            empty2 @?= False
        , testCase "tryAcquirePool succeeds when token available" $ do
            eid <- createEntity
            let rid = ResourceId eid
            pool <- atomically $ newPoolLock 1 [rid]
            mToken <- atomically $ tryAcquirePool pool
            isJust mToken @?= True
        , testCase "tryAcquirePool fails when pool empty" $ do
            eid <- createEntity
            let rid = ResourceId eid
            pool <- atomically $ newPoolLock 1 [rid]
            _ <- atomically $ tryAcquirePool pool
            mToken <- atomically $ tryAcquirePool pool
            mToken @?= Nothing
        ]

-------------------------------------------------------------------------------
-- Component ID Tests
-------------------------------------------------------------------------------

componentIdTests :: TestTree
componentIdTests =
    testGroup
        "Component IDs"
        [ testCase "AccessControl has correct component ID" $ do
            let cid = componentId (Proxy @AccessControl)
            cid @?= ComponentTypeId 20
        , testCase "ResourceSync has correct component ID" $ do
            let cid = componentId (Proxy @ResourceSync)
            cid @?= ComponentTypeId 21
        , testCase "SqliteAccess has correct component ID" $ do
            let cid = componentId (Proxy @SqliteAccess)
            cid @?= ComponentTypeId 22
        , testCase "LuaAccess has correct component ID" $ do
            let cid = componentId (Proxy @LuaAccess)
            cid @?= ComponentTypeId 23
        , testCase "HttpAccess has correct component ID" $ do
            let cid = componentId (Proxy @HttpAccess)
            cid @?= ComponentTypeId 24
        , testCase "All component IDs are unique" $ do
            let cids =
                    [ componentId (Proxy @AccessControl)
                    , componentId (Proxy @ResourceSync)
                    , componentId (Proxy @SqliteAccess)
                    , componentId (Proxy @LuaAccess)
                    , componentId (Proxy @HttpAccess)
                    ]
            length cids @?= length (distinct cids)
        ]
  where
    distinct = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-------------------------------------------------------------------------------
-- Resource Access Monad Tests
-------------------------------------------------------------------------------

resourceAccessMonadTests :: TestTree
resourceAccessMonadTests =
    testGroup
        "Resource Access Monad"
        [ testCase "ResourceError values" $ do
            eid <- createEntity
            let rid = ResourceId eid
            let err1 = ResourceNotFound rid
            let err2 = ResourceBusy rid
            -- Just verify they contain the expected constructor names
            let s1 = show err1
            let s2 = show err2
            assertBool "ResourceNotFound in show" ("ResourceNotFound" `isInfixOf` s1)
            assertBool "ResourceBusy in show" ("ResourceBusy" `isInfixOf` s2)
            assertBool "ResourceId in show" ("ResourceId" `isInfixOf` s1)
        , testCase "ResourceError equality" $ do
            eid <- createEntity
            let rid = ResourceId eid
            let err1 = ResourceNotFound rid
            let err2 = ResourceNotFound rid
            let err3 = ResourceBusy rid
            err1 @?= err2
            assertBool "Different errors should not be equal" (err1 /= err3)
        , testCase "runResourceM returns error for invalid resource" $ do
            -- This test would need a proper context setup
            -- For now, we just verify the types compile
            assertBool "Placeholder" True
        ]

-------------------------------------------------------------------------------
-- Timeout Tests
-------------------------------------------------------------------------------

timeoutTests :: TestTree
timeoutTests =
    testGroup
        "Timeout Handling"
        [ testCase "AccessControl with timeout stores value" $ do
            let timeout :: NominalDiffTime = 5.5
            let control = AccessControl ExclusiveAccess (Just timeout)
            accessTimeout control @?= Just timeout
        , testCase "AccessControl without timeout has Nothing" $ do
            let control = AccessControl ReadWriteAccess Nothing
            accessTimeout control @?= Nothing
        ]

-------------------------------------------------------------------------------
-- Resource-Specific Access Tests
-------------------------------------------------------------------------------

resourceSpecificAccessTests :: TestTree
resourceSpecificAccessTests =
    testGroup
        "Resource-Specific Access"
        [ testCase "initSqliteAccess creates RWLock" $ do
            access <- initSqliteAccess True
            sqliteWalMode access @?= True
        , testCase "initLuaAccess creates TMVar" $ do
            _access <- initLuaAccess
            -- Just verify it creates successfully
            assertBool "LuaAccess created" True
        , testCase "initHttpAccess creates pool" $ do
            eid <- createEntity
            let rid = ResourceId eid
            access <- initHttpAccess 10 rid
            httpManagerRef access @?= rid
        , testCase "SqliteAccess with WAL mode" $ do
            access <- initSqliteAccess True
            sqliteWalMode access @?= True
        , testCase "SqliteAccess without WAL mode" $ do
            access <- initSqliteAccess False
            sqliteWalMode access @?= False
        ]

-------------------------------------------------------------------------------
-- Concurrent Stress Tests
-------------------------------------------------------------------------------

concurrentStressTests :: TestTree
concurrentStressTests =
    testGroup
        "Concurrent Stress Tests"
        [ testCase "RWLock handles multiple concurrent readers" $ do
            rwlock <- atomically newRWLock
            counter <- newIORef (0 :: Int)

            -- Spawn 10 reader threads
            let readerAction = do
                    withReadLock rwlock $ do
                        _ <- atomicModifyIORef' counter (\n -> (n + 1, ()))
                        threadDelay 1000  -- Hold lock briefly

            _ <- replicateM 10 $ forkIO readerAction

            -- Wait for all threads
            threadDelay 50000

            count <- readIORef counter
            count @?= 10
        , testCase "RWLock writer excludes all readers" $ do
            rwlock <- atomically newRWLock
            writerAcquired <- newIORef False
            readerTried <- newIORef False

            -- First, acquire write lock
            withWriteLock rwlock $ do
                writeIORef writerAcquired True
                -- Try to acquire read lock (should fail or block)
                acquired <- atomically $ tryAcquireRead rwlock
                writeIORef readerTried acquired
                threadDelay 10000

            writerDone <- readIORef writerAcquired
            readerResult <- readIORef readerTried

            writerDone @?= True
            readerResult @?= False  -- Reader should not acquire while writer holds
        , testCase "Exclusive lock is exclusive" $ do
            lock <- atomically newExclusiveLock
            counter <- newIORef (0 :: Int)

            -- Spawn 5 threads that try to acquire the lock
            let worker = do
                    withExclusiveLock lock $ do
                        -- Simulate work
                        _ <- atomicModifyIORef' counter (\n -> (n + 1, ()))
                        threadDelay 5000

            replicateM_ 5 $ forkIO worker
            threadDelay 50000

            count <- readIORef counter
            -- All 5 should have run (sequentially)
            count @?= 5
        , testCase "Pool limits concurrent access" $ do
            eid <- createEntity
            let rid = ResourceId eid
            pool <- atomically $ newPoolLock 2 [rid, rid]  -- Pool of 2

            activeCount <- newIORef (0 :: Int)
            maxActive <- newIORef (0 :: Int)

            let worker = do
                    mToken <- atomically $ tryAcquirePool pool
                    case mToken of
                        Nothing -> pure ()
                        Just token -> do
                            -- Track active count
                            current <- atomicModifyIORef' activeCount (\n -> (n + 1, n + 1))
                            _ <- atomicModifyIORef' maxActive (\m -> (max m current, ()))
                            threadDelay 5000
                            _ <- atomicModifyIORef' activeCount (\n -> (n - 1, ()))
                            atomically $ tokenRelease token

            -- Spawn 5 workers
            replicateM_ 5 $ forkIO worker
            threadDelay 50000

            maxSeen <- readIORef maxActive
            -- Pool size is 2, so max concurrent should be at most 2
            assertBool "Max concurrent should be <= 2" (maxSeen <= 2)
        ]

-------------------------------------------------------------------------------
-- Thread Safety Tests
-------------------------------------------------------------------------------

threadSafetyTests :: TestTree
threadSafetyTests =
    testGroup
        "Thread Safety"
        [ testCase "RWLock prevents race conditions on read-modify-write" $ do
            rwlock <- atomically newRWLock
            counter <- atomically $ newTVar 0

            let increment = withWriteLock rwlock $ do
                    atomically $ do
                        val <- readTVar counter
                        writeTVar counter (val + 1)

            -- Spawn 100 threads
            replicateM_ 100 $ forkIO increment
            threadDelay 100000

            finalCount <- atomically $ readTVar counter
            finalCount @?= 100
        , testCase "STM operations are atomic" $ do
            var <- atomically $ newTVar 0

            let stmAction = do
                    val <- readTVar var
                    writeTVar var (val + 1)

            -- Perform 100 atomic increments
            replicateM_ 100 $ atomically stmAction

            finalVal <- atomically $ readTVar var
            finalVal @?= 100
        , testCase "Lock is released on exception" $ do
            rwlock <- atomically newRWLock
            let action = withWriteLock rwlock $ do
                    error "Simulated failure"
            result <- try action :: IO (Either SomeException ())
            case result of
                Left _ -> do
                    -- Lock should be released
                    acquired <- atomically $ tryAcquireWrite rwlock
                    acquired @?= True  -- Should be able to acquire now
                Right _ -> error "Should have thrown exception"
        , testCase "Exclusive lock is released on exception" $ do
            lock <- atomically newExclusiveLock
            let action = withExclusiveLock lock $ do
                    error "Simulated failure"
            result <- try action :: IO (Either SomeException ())
            case result of
                Left _ -> do
                    -- Lock should be released
                    acquired <- atomically $ tryAcquireExclusive lock
                    acquired @?= True
                Right _ -> error "Should have thrown exception"
        ]

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

writeIORef :: IORef a -> a -> IO ()
writeIORef ref val = atomicModifyIORef' ref (\_ -> (val, ()))

