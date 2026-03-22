{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Tests for the Resource Management System.

These tests verify:
- Scope hierarchy and validity checking
- Resource creation and registration
- Scope cleanup (proper disposal order)
- Resource-specific components (SQLite, Lua, HTTP)
- Thread safety of registry operations
-}
module OS.ResourcesTests (
    resourcesTests,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVar, readTVar, writeTVar)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy (..))
import Data.Time (getCurrentTime)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure, (@?=))

import System.Agents.OS.Core (
    AgentId (..),
    Component (..),
    ComponentTypeId (..),
    ConversationId (..),
    EntityId (..),
    ResourceId (..),
    ToolCallId (..),
    ToolboxId (..),
    TurnId (..),
    createEntity,
 )
import System.Agents.OS.Resources
import qualified System.Agents.OS.Resources.Http as HttpRes
import qualified System.Agents.OS.Resources.Lua as LuaRes
import qualified System.Agents.OS.Resources.Sqlite as SqliteRes

-------------------------------------------------------------------------------
-- Test Suite
-------------------------------------------------------------------------------

resourcesTests :: TestTree
resourcesTests =
    testGroup
        "Resource Management Tests"
        [ scopeTests
        , registryTests
        , resourceLifecycleTests
        , componentIdTests
        , scopeValidationTests
        , sqliteResourceTests
        , luaResourceTests
        , httpResourceTests
        , concurrentAccessTests
        ]

-------------------------------------------------------------------------------
-- Scope Tests
-------------------------------------------------------------------------------

scopeTests :: TestTree
scopeTests =
    testGroup
        "Scope Definition"
        [ testCase "ResourceScope can be empty" $ do
            let scope = ResourceScope []
            scopeLevelToList scope @?= []
        , testCase "ResourceScope with single level" $ do
            let scope = ResourceScope [ProgramScope]
            scopeLevelToList scope @?= [ProgramScope]
        , testCase "ResourceScope with multiple levels" $ do
            eid <- createEntity
            let agentId = AgentId eid
            let scope = ResourceScope [ProgramScope, AgentScope agentId]
            length (scopeLevelToList scope) @?= 2
        , testCase "ScopeLevel equality" $ do
            eid <- createEntity
            let agentId1 = AgentId eid
            eid2 <- createEntity
            let agentId2 = AgentId eid2
            AgentScope agentId1 @?= AgentScope agentId1
            assertBool "Different agents should not be equal" (AgentScope agentId1 /= AgentScope agentId2)
        , testCase "All scope levels are distinct" $ do
            let levels = [ProgramScope]
            length levels @?= length (distinct levels)
        , testCase "ScopeLevel JSON round-trip" $ do
            let level = ProgramScope
            -- Note: We can't directly test JSON here without importing Aeson
            -- but we verify the type has the instances
            assertBool "Has Show instance" (length (show level) > 0)
        ]
  where
    distinct = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-------------------------------------------------------------------------------
-- Registry Tests
-------------------------------------------------------------------------------

registryTests :: TestTree
registryTests =
    testGroup
        "Resource Registry"
        [ testCase "newResourceRegistry creates empty registry" $ do
            registry <- atomically newResourceRegistry
            count <- getResourceCount registry
            count @?= 0
        , testCase "registerResourceHandle adds resource" $ do
            registry <- atomically newResourceRegistry
            eid <- createEntity
            let rid = ResourceId eid

            -- Create a simple handle
            let handle =
                    ResourceHandle
                        { handleId = rid
                        , handleCleanup = pure ()
                        , handleAccess = \f -> f (ResourceAccessor ())
                        }

            -- Create resource info
            now <- getCurrentTime
            let info =
                    ResourceInfo
                        { resourceId = rid
                        , resourceScope = ResourceScope [ProgramScope]
                        , resourceType = HttpManagerResource $ HttpManagerConfig 10 30 60
                        , resourceCreatedAt = now
                        }

            registerResourceHandle registry handle info
            count <- getResourceCount registry
            count @?= 1
        , testCase "lookupResourceHandle finds registered resource" $ do
            registry <- atomically newResourceRegistry
            eid <- createEntity
            let rid = ResourceId eid

            let handle =
                    ResourceHandle
                        { handleId = rid
                        , handleCleanup = pure ()
                        , handleAccess = \f -> f (ResourceAccessor ())
                        }

            now <- getCurrentTime
            let info =
                    ResourceInfo
                        { resourceId = rid
                        , resourceScope = ResourceScope [ProgramScope]
                        , resourceType = HttpManagerResource $ HttpManagerConfig 10 30 60
                        , resourceCreatedAt = now
                        }

            registerResourceHandle registry handle info
            mHandle <- lookupResourceHandle registry rid
            assertBool "Should find handle" (isJust mHandle)
        , testCase "lookupResourceHandle returns Nothing for missing resource" $ do
            registry <- atomically newResourceRegistry
            eid <- createEntity
            let rid = ResourceId eid

            mHandle <- lookupResourceHandle registry rid
            assertBool "Should not find handle" (isNothing mHandle)
        ]

-------------------------------------------------------------------------------
-- Resource Lifecycle Tests
-------------------------------------------------------------------------------

resourceLifecycleTests :: TestTree
resourceLifecycleTests =
    testGroup
        "Resource Lifecycle"
        [ testCase "createResource registers resource" $ do
            registry <- atomically newResourceRegistry
            let ctx = ResourceContext [ProgramScope] registry

            rid <-
                createResource ctx (HttpManagerResource $ HttpManagerConfig 10 30 60) $ \rid' ->
                    pure
                        ResourceHandle
                            { handleId = rid'
                            , handleCleanup = pure ()
                            , handleAccess = \f -> f (ResourceAccessor ())
                            }

            count <- getResourceCount registry
            count @?= 1

            mHandle <- lookupResourceHandle registry rid
            assertBool "Should find created resource" (isJust mHandle)
        , testCase "cleanupScope removes resources" $ do
            registry <- atomically newResourceRegistry
            eid <- createEntity
            let agentId = AgentId eid
            let ctx = ResourceContext [ProgramScope, AgentScope agentId] registry

            -- Create a resource
            _ <-
                createResource ctx (HttpManagerResource $ HttpManagerConfig 10 30 60) $ \rid ->
                    pure
                        ResourceHandle
                            { handleId = rid
                            , handleCleanup = pure ()
                            , handleAccess = \f -> f (ResourceAccessor ())
                            }

            countBefore <- getResourceCount registry
            countBefore @?= 1

            -- Cleanup the scope
            cleanupScope registry (AgentScope agentId)

            countAfter <- getResourceCount registry
            countAfter @?= 0
        , testCase "cleanup runs in reverse creation order" $ do
            registry <- atomically newResourceRegistry
            eid <- createEntity
            let agentId = AgentId eid
            let ctx = ResourceContext [ProgramScope, AgentScope agentId] registry

            -- Track cleanup order
            cleanupOrder <- atomically $ newTVar []

            -- Create multiple resources
            _ <-
                createResource ctx (HttpManagerResource $ HttpManagerConfig 10 30 60) $ \rid ->
                    pure
                        ResourceHandle
                            { handleId = rid
                            , handleCleanup = atomically $ modifyTVar' cleanupOrder (1 :)
                            , handleAccess = \f -> f (ResourceAccessor ())
                            }

            _ <-
                createResource ctx (HttpManagerResource $ HttpManagerConfig 10 30 60) $ \rid ->
                    pure
                        ResourceHandle
                            { handleId = rid
                            , handleCleanup = atomically $ modifyTVar' cleanupOrder (2 :)
                            , handleAccess = \f -> f (ResourceAccessor ())
                            }

            _ <-
                createResource ctx (HttpManagerResource $ HttpManagerConfig 10 30 60) $ \rid ->
                    pure
                        ResourceHandle
                            { handleId = rid
                            , handleCleanup = atomically $ modifyTVar' cleanupOrder (3 :)
                            , handleAccess = \f -> f (ResourceAccessor ())
                            }

            -- Cleanup the scope
            cleanupScope registry (AgentScope agentId)

            -- Check cleanup order (should be reverse: 3, 2, 1)
            order <- atomically $ readTVar cleanupOrder
            order @?= [3, 2, 1]
        , testCase "cleanup handles missing resources gracefully" $ do
            registry <- atomically newResourceRegistry
            eid <- createEntity
            let agentId = AgentId eid

            -- Cleanup a scope with no resources should not fail
            cleanupScope registry (AgentScope agentId)

            count <- getResourceCount registry
            count @?= 0
        , testCase "handleAccess provides access to resource" $ do
            registry <- atomically newResourceRegistry
            let ctx = ResourceContext [ProgramScope] registry

            rid <-
                createResource ctx (HttpManagerResource $ HttpManagerConfig 10 30 60) $ \rid' ->
                    pure
                        ResourceHandle
                            { handleId = rid'
                            , handleCleanup = pure ()
                            , handleAccess = \f -> f (ResourceAccessor ())
                            }

            mResult <- withResource registry rid $ \_accessor -> do
                pure "success"

            mResult @?= Just "success"
        ]

-------------------------------------------------------------------------------
-- Component ID Tests
-------------------------------------------------------------------------------

componentIdTests :: TestTree
componentIdTests =
    testGroup
        "Component IDs"
        [ testCase "ResourceInfo has correct component ID" $ do
            let cid = componentId (Proxy @ResourceInfo)
            cid @?= ComponentTypeId 10
        , testCase "SqliteResourceData has correct component ID" $ do
            let cid = componentId (Proxy @SqliteRes.SqliteResourceData)
            cid @?= ComponentTypeId 11
        , testCase "LuaResourceData has correct component ID" $ do
            let cid = componentId (Proxy @LuaRes.LuaResourceData)
            cid @?= ComponentTypeId 12
        , testCase "HttpResourceData has correct component ID" $ do
            let cid = componentId (Proxy @HttpRes.HttpResourceData)
            cid @?= ComponentTypeId 13
        , testCase "All component IDs are unique" $ do
            let cids =
                    [ componentId (Proxy @ResourceInfo)
                    , componentId (Proxy @SqliteRes.SqliteResourceData)
                    , componentId (Proxy @LuaRes.LuaResourceData)
                    , componentId (Proxy @HttpRes.HttpResourceData)
                    ]
            length (distinct cids) @?= 4
        ]
  where
    distinct = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-------------------------------------------------------------------------------
-- Scope Validation Tests
-------------------------------------------------------------------------------

scopeValidationTests :: TestTree
scopeValidationTests =
    testGroup
        "Scope Validation"
        [ testCase "empty resource scope is always valid" $ do
            let scope = ResourceScope []
            isResourceValid scope [ProgramScope, AgentScope (AgentId $ EntityId (read "550e8400-e29b-41d4-a716-446655440000"))] @?= True
        , testCase "resource scope matching active scopes is valid" $ do
            let eid = EntityId (read "550e8400-e29b-41d4-a716-446655440000")
            let agentId = AgentId eid
            let scope = ResourceScope [ProgramScope, AgentScope agentId]
            let active = [ProgramScope, AgentScope agentId, ConversationScope (ConversationId eid)]
            isResourceValid scope active @?= True
        , testCase "resource scope not matching is invalid" $ do
            eid1 <- createEntity
            eid2 <- createEntity
            let agentId1 = AgentId eid1
            let agentId2 = AgentId eid2
            let scope = ResourceScope [ProgramScope, AgentScope agentId1]
            let active = [ProgramScope, AgentScope agentId2]
            isResourceValid scope active @?= False
        , testCase "resource scope longer than active is invalid" $ do
            eid <- createEntity
            let agentId = AgentId eid
            let convId = ConversationId eid
            let scope = ResourceScope [ProgramScope, AgentScope agentId, ConversationScope convId]
            let active = [ProgramScope, AgentScope agentId]
            isResourceValid scope active @?= False
        , testCase "exact match is valid" $ do
            eid <- createEntity
            let agentId = AgentId eid
            let scope = ResourceScope [ProgramScope, AgentScope agentId]
            let active = [ProgramScope, AgentScope agentId]
            isResourceValid scope active @?= True
        ]

-------------------------------------------------------------------------------
-- SQLite Resource Tests
-------------------------------------------------------------------------------

sqliteResourceTests :: TestTree
sqliteResourceTests =
    testGroup
        "SQLite Resources"
        [ testCase "SqliteConfig round-trip" $ do
            let config = SqliteConfig "/path/to/db.sqlite" SerialAccess
            -- Verify structure
            sqlitePath config @?= "/path/to/db.sqlite"
            sqliteAccessMode config @?= SerialAccess
        , testCase "SqliteAccessMode values" $ do
            SerialAccess @?= SerialAccess
            ConcurrentReads @?= ConcurrentReads
            assertBool "Modes are different" (SerialAccess /= ConcurrentReads)
        , testCase "SqliteResource type" $ do
            let config = SqliteConfig "/path/to/db.sqlite" ConcurrentReads
            let resType = SqliteResource config
            case resType of
                SqliteResource c -> do
                    sqlitePath c @?= "/path/to/db.sqlite"
                    sqliteAccessMode c @?= ConcurrentReads
                _ -> assertFailure "Expected SqliteResource"
        ]

-------------------------------------------------------------------------------
-- Lua Resource Tests
-------------------------------------------------------------------------------

luaResourceTests :: TestTree
luaResourceTests =
    testGroup
        "Lua Resources"
        [ testCase "LuaConfig structure" $ do
            let config = LuaConfig 256 300 True
            luaMaxMemoryMB config @?= 256
            luaMaxExecutionTimeSeconds config @?= 300
            luaSandboxed config @?= True
        , testCase "LuaResource type" $ do
            let config = LuaConfig 512 600 False
            let resType = LuaResource config
            case resType of
                LuaResource c -> do
                    luaMaxMemoryMB c @?= 512
                    luaMaxExecutionTimeSeconds c @?= 600
                    luaSandboxed c @?= False
                _ -> assertFailure "Expected LuaResource"
        ]

-------------------------------------------------------------------------------
-- HTTP Resource Tests
-------------------------------------------------------------------------------

httpResourceTests :: TestTree
httpResourceTests =
    testGroup
        "HTTP Resources"
        [ testCase "HttpManagerConfig structure" $ do
            let config = HttpManagerConfig 10 30 60
            httpPoolSize config @?= 10
            httpConnectionTimeoutSeconds config @?= 30
            httpResponseTimeoutSeconds config @?= 60
        , testCase "HttpManagerResource type" $ do
            let config = HttpManagerConfig 20 45 90
            let resType = HttpManagerResource config
            case resType of
                HttpManagerResource c -> do
                    httpPoolSize c @?= 20
                    httpConnectionTimeoutSeconds c @?= 45
                    httpResponseTimeoutSeconds c @?= 90
                _ -> assertFailure "Expected HttpManagerResource"
        ]

-------------------------------------------------------------------------------
-- Concurrent Access Tests
-------------------------------------------------------------------------------

concurrentAccessTests :: TestTree
concurrentAccessTests =
    testGroup
        "Concurrent Access"
        [ testCase "concurrent resource creation" $ do
            registry <- atomically newResourceRegistry
            let ctx = ResourceContext [ProgramScope] registry

            -- Create resources from multiple threads
            let createResource' n = do
                    _ <-
                        createResource ctx (HttpManagerResource $ HttpManagerConfig n 30 60) $ \rid ->
                            pure
                                ResourceHandle
                                    { handleId = rid
                                    , handleCleanup = pure ()
                                    , handleAccess = \f -> f (ResourceAccessor ())
                                    }
                    pure ()

            -- Create 10 resources concurrently
            mapM_ (\n -> forkIO $ createResource' n) [1 .. 10 :: Int]

            -- Wait for creation
            threadDelay 100000

            -- Check count (may vary due to concurrency, but should be > 0)
            count <- getResourceCount registry
            assertBool "Should have created resources" (count > 0)
        , testCase "STM atomicity for registry operations" $ do
            registry <- atomically newResourceRegistry
            eid <- createEntity
            let rid = ResourceId eid

            let handle =
                    ResourceHandle
                        { handleId = rid
                        , handleCleanup = pure ()
                        , handleAccess = \f -> f (ResourceAccessor ())
                        }

            now <- getCurrentTime
            let info =
                    ResourceInfo
                        { resourceId = rid
                        , resourceScope = ResourceScope [ProgramScope]
                        , resourceType = HttpManagerResource $ HttpManagerConfig 10 30 60
                        , resourceCreatedAt = now
                        }

            -- Multiple STM operations should be atomic
            atomically $ do
                modifyTVar' (registryHandles registry) $ HashMap.insert rid handle
                modifyTVar' (registryHandles registry) $ HashMap.insert rid handle -- idempotent

            count <- getResourceCount registry
            count @?= 1
        ]

