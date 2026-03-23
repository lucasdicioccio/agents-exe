{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Performance benchmarks for the OS Model.

These benchmarks measure:
- Agent creation performance
- Tool call latency
- Concurrent agent operations
- Memory usage under load
- ECS query performance
- Resource management overhead
- Persistence layer performance
-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (mapConcurrently, mapConcurrently_, replicateConcurrently_)
import Control.Concurrent.STM (
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVarIO,
 )
import Control.DeepSeq (NFData, force, rnf)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, replicateM, replicateM_, void)
import Criterion.Main (
    Benchmark,
    bench,
    bgroup,
    defaultMain,
    nf,
    nfIO,
    whnf,
    whnfIO,
 )
import Data.Aeson (Value (..), object, (.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (removePathForcibly)
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performGC)

import System.Agents.OS.Concurrent
import System.Agents.OS.Core
import System.Agents.OS.Core.World (
    World,
    allEntitiesWithComponent,
    createEntity,
    getComponent,
    modifyComponent,
    newWorld,
    registerComponentStore,
    setComponent,
 )
import System.Agents.OS.Conversation
import System.Agents.OS.Persistence
import System.Agents.OS.Resources

-------------------------------------------------------------------------------
-- Main Benchmark Suite
-------------------------------------------------------------------------------

main :: IO ()
main = do
    -- Setup: Create a world for benchmarks
    world <- atomically $ do
        w <- newWorld
        w' <- registerComponentStore w (Proxy @AgentConfig)
        w'' <- registerComponentStore w' (Proxy @AgentState)
        w''' <- registerComponentStore w'' (Proxy @ToolboxConfig)
        w'''' <- registerComponentStore w''' (Proxy @ConversationConfig)
        registerComponentStore w'''' (Proxy @TurnConfig)

    defaultMain
        [ bgroup "Entity Creation" (entityCreationBenchmarks world)
        , bgroup "Component Operations" (componentOperationBenchmarks world)
        , bgroup "Query Performance" (queryPerformanceBenchmarks world)
        , bgroup "Concurrent Access" concurrentAccessBenchmarks
        , bgroup "Resource Management" resourceManagementBenchmarks
        , bgroup "Persistence" persistenceBenchmarks
        ]

-------------------------------------------------------------------------------
-- Entity Creation Benchmarks
-------------------------------------------------------------------------------

entityCreationBenchmarks :: World -> [Benchmark]
entityCreationBenchmarks world =
    [ bench "create single entity" $ nfIO $ do
        _ <- createEntity
        pure ()

    , bench "create 100 entities" $ nfIO $ do
        replicateM_ 100 createEntity

    , bench "create 1000 entities" $ nfIO $ do
        replicateM_ 1000 createEntity

    , bench "create agent with components" $ nfIO $ do
        createTestAgent world "benchmark-agent"

    , bench "create 100 agents" $ nfIO $ do
        forM_ [(1 :: Int) .. 100] $ \i ->
            createTestAgent world ("agent-" <> Text.pack (show i))
    ]

-------------------------------------------------------------------------------
-- Component Operation Benchmarks
-------------------------------------------------------------------------------

componentOperationBenchmarks :: World -> [Benchmark]
componentOperationBenchmarks world =
    [ bench "set single component" $ whnfIO $ do
        eid <- createEntity
        atomically $ setComponent world eid (testAgentConfig "test")

    , bench "get component (hit)" $ whnfIO $ do
        eid <- createEntity
        atomically $ do
            setComponent world eid (testAgentConfig "test")
            getComponent @AgentConfig world eid

    , bench "get component (miss)" $ whnfIO $ do
        eid <- createEntity
        atomically $ getComponent @AgentConfig world eid

    , bench "modify component" $ whnfIO $ do
        eid <- createEntity
        atomically $ do
            setComponent world eid (testAgentConfig "test")
            modifyComponent @AgentConfig world eid $ \c ->
                c{agentName = "modified"}

    , bench "component round-trip" $ nfIO $ do
        eid <- createEntity
        atomically $ do
            setComponent world eid (testAgentConfig "test")
            mComp <- getComponent @AgentConfig world eid
            case mComp of
                Nothing -> pure ()
                Just c -> setComponent world eid (c :: AgentConfig)
    ]

-------------------------------------------------------------------------------
-- Query Performance Benchmarks
-------------------------------------------------------------------------------

queryPerformanceBenchmarks :: World -> [Benchmark]
queryPerformanceBenchmarks world =
    [ bench "query 10 entities with component" $ nfIO $ do
        -- Create entities
        forM_ [(1 :: Int) .. 10] $ \i -> do
            eid <- createEntity
            atomically $ setComponent world eid (testAgentConfig $ "agent-" <> Text.pack (show i))
        -- Query
        atomically $ allEntitiesWithComponent @AgentConfig world

    , bench "query 100 entities with component" $ nfIO $ do
        forM_ [(1 :: Int) .. 100] $ \i -> do
            eid <- createEntity
            atomically $ setComponent world eid (testAgentConfig $ "agent-" <> Text.pack (show i))
        atomically $ allEntitiesWithComponent @AgentConfig world

    , bench "query 1000 entities with component" $ nfIO $ do
        forM_ [(1 :: Int) .. 1000] $ \i -> do
            eid <- createEntity
            atomically $ setComponent world eid (testAgentConfig $ "agent-" <> Text.pack (show i))
        atomically $ allEntitiesWithComponent @AgentConfig world
    ]

-------------------------------------------------------------------------------
-- Concurrent Access Benchmarks
-------------------------------------------------------------------------------

concurrentAccessBenchmarks :: [Benchmark]
concurrentAccessBenchmarks =
    [ bench "concurrent entity creation (10 threads)" $ nfIO $ do
        mapConcurrently_ (\_ -> replicateM_ 100 createEntity) [(1 :: Int) .. 10]

    , bench "concurrent reads (RWLock)" $ nfIO $ do
        rwlock <- atomically newRWLock
        counter <- newTVarIO (0 :: Int)
        mapConcurrently_
            (\_ -> atomically $ withReadLock rwlock $ modifyTVar' counter (+ 1))
            [(1 :: Int) .. 100]

    , bench "exclusive writes (RWLock)" $ nfIO $ do
        rwlock <- atomically newRWLock
        counter <- newTVarIO (0 :: Int)
        mapConcurrently_
            (\_ -> atomically $ withWriteLock rwlock $ modifyTVar' counter (+ 1))
            [(1 :: Int) .. 100]

    , bench "mixed read/write (RWLock)" $ nfIO $ do
        rwlock <- atomically newRWLock
        counter <- newTVarIO (0 :: Int)
        -- 90% reads, 10% writes
        mapConcurrently_
            ( \i ->
                if i `mod` 10 == 0
                    then atomically $ withWriteLock rwlock $ modifyTVar' counter (+ 1)
                    else atomically $ withReadLock rwlock $ readTVarIO counter >> pure ()
            )
            [(1 :: Int) .. 100]
    ]

-------------------------------------------------------------------------------
-- Resource Management Benchmarks
-------------------------------------------------------------------------------

resourceManagementBenchmarks :: [Benchmark]
resourceManagementBenchmarks =
    [ bench "create 100 resources" $ nfIO $ do
        registry <- atomically newResourceRegistry
        let ctx = ResourceContext [ProgramScope] registry
        forM_ [(1 :: Int) .. 100] $ \i -> do
            _ <-
                createResource ctx (CustomResource ("res-" <> Text.pack (show i)) Null) $ \rid ->
                    pure
                        ResourceHandle
                            { handleId = rid
                            , handleCleanup = pure ()
                            , handleAccess = \f -> f (CustomAccessor Null)
                            }
            pure ()

    , bench "resource access" $ nfIO $ do
        registry <- atomically newResourceRegistry
        let ctx = ResourceContext [ProgramScope] registry
        rid <- createResource ctx (CustomResource "test" Null) $ \rid ->
            pure
                ResourceHandle
                    { handleId = rid
                    , handleCleanup = pure ()
                    , handleAccess = \f -> f (CustomAccessor Null)
                    }
        -- Access resource 100 times
        replicateM_ 100 $ do
            _ <- withResource registry rid $ \_ -> pure (42 :: Int)
            pure ()

    , bench "resource cleanup" $ nfIO $ do
        registry <- atomically newResourceRegistry
        let ctx = ResourceContext [ProgramScope] registry
        rids <- forM [(1 :: Int) .. 100] $ \i ->
            createResource ctx (CustomResource ("res-" <> Text.pack (show i)) Null) $ \rid ->
                pure
                    ResourceHandle
                        { handleId = rid
                        , handleCleanup = pure ()
                        , handleAccess = \f -> f (CustomAccessor Null)
                        }
        -- Cleanup all
        forM_ rids $ \rid -> do
            mHandle <- lookupResourceHandle registry rid
            case mHandle of
                Just h -> handleCleanup h
                Nothing -> pure ()
    ]

-------------------------------------------------------------------------------
-- Persistence Benchmarks
-------------------------------------------------------------------------------

persistenceBenchmarks :: [Benchmark]
persistenceBenchmarks =
    [ bench "persist single entity (SQLite)" $ nfIO $ do
        withSystemTempDirectory "bench" $ \tmpDir -> do
            let dbPath = tmpDir ++ "/bench.db"
            backend <- createPersistenceBackend (SqliteBackendType dbPath)
            eid <- createEntity
            let config = testAgentConfig "persist-test"
            persist backend eid config
            closePersistenceBackend backend

    , bench "persist 100 entities (SQLite)" $ nfIO $ do
        withSystemTempDirectory "bench" $ \tmpDir -> do
            let dbPath = tmpDir ++ "/bench.db"
            backend <- createPersistenceBackend (SqliteBackendType dbPath)
            forM_ [(1 :: Int) .. 100] $ \i -> do
                eid <- createEntity
                let config = testAgentConfig ("agent-" <> Text.pack (show i))
                persist backend eid config
            closePersistenceBackend backend

    , bench "load entity (SQLite)" $ nfIO $ do
        withSystemTempDirectory "bench" $ \tmpDir -> do
            let dbPath = tmpDir ++ "/bench.db"
            backend <- createPersistenceBackend (SqliteBackendType dbPath)
            eid <- createEntity
            let config = testAgentConfig "load-test"
            persist backend eid config
            -- Close and reopen to ensure disk persistence
            closePersistenceBackend backend
            backend' <- createPersistenceBackend (SqliteBackendType dbPath)
            _ <- load backend' eid :: IO (Maybe AgentConfig)
            closePersistenceBackend backend'

    , bench "in-memory persistence (no-op)" $ nfIO $ do
        backend <- createPersistenceBackend InMemory
        forM_ [(1 :: Int) .. 1000] $ \i -> do
            eid <- createEntity
            let config = testAgentConfig ("agent-" <> Text.pack (show i))
            persist backend eid config
        closePersistenceBackend backend
    ]

-------------------------------------------------------------------------------
-- Memory Usage Benchmarks
-------------------------------------------------------------------------------

memoryBenchmarks :: IO ()
memoryBenchmarks = do
    putStrLn "\n=== Memory Usage Benchmarks ==="

    -- Baseline
    performGC
    threadDelay 100000

    -- Measure entity creation
    putStrLn "\nEntity creation memory:"
    world <- atomically $ do
        w <- newWorld
        registerComponentStore w (Proxy @AgentConfig)

    forM_ [10, 100, 1000, 10000] $ \n -> do
        start <- getCurrentTime
        entities <- replicateM n $ do
            eid <- createEntity
            atomically $ setComponent world eid (testAgentConfig "mem-test")
            pure eid

        performGC
        threadDelay 100000

        end <- getCurrentTime
        let elapsed = diffUTCTime end start
        putStrLn $ "  " ++ show n ++ " entities: " ++ show elapsed

        -- Cleanup
        forM_ entities $ \eid ->
            atomically $ removeComponent @AgentConfig world eid

    -- Measure component stores
    putStrLn "\nComponent store memory:"
    forM_ [1, 5, 10] $ \nStores -> do
        world' <- atomically $ do
            w <- newWorld
            foldM (\w _ -> registerComponentStore w (Proxy @AgentConfig)) w [(1 :: Int) .. nStores]

        performGC
        putStrLn $ "  " ++ show nStores ++ " component stores created"

    putStrLn ""

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- | Create a test agent.
createTestAgent :: World -> Text -> IO AgentId
createTestAgent world name = do
    eid <- createEntity
    atomically $ setComponent world eid (testAgentConfig name)
    pure $ AgentId eid

-- | Create a test agent config.
testAgentConfig :: Text -> AgentConfig
testAgentConfig name =
    AgentConfig
        { agentName = name
        , agentModel = ModelConfig "openai" "https://api.openai.com/v1" "gpt-4" "key1"
        , agentSystemPrompt = "You are a benchmark agent"
        , agentToolboxBindings = []
        }

-- | Placeholder ResourceAccessor
data ResourceAccessor = CustomAccessor Value

-- | Placeholder ResourceType
data ResourceType = CustomResource Text Value

-- | FoldM helper
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM f acc [] = pure acc
foldM f acc (x:xs) = do
    acc' <- f acc x
    foldM f acc' xs

-- | Helper to remove component (needed for cleanup)
removeComponent :: forall a. (Component a) => World -> EntityId -> STM ()
removeComponent = System.Agents.OS.Core.World.removeComponent

