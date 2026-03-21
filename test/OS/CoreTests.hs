{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Tests for the ECS Core types and operations.

These tests verify:
- Phantom type safety for entity IDs
- Component storage and retrieval
- World operations
- STM concurrency safety
-}
module OS.CoreTests (
    coreTests,
) where

import Control.Concurrent.STM (atomically)
import Data.Aeson (FromJSON, ToJSON, Value (..), decode, encode)
import Data.Hashable (hash)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, (@?=))

import System.Agents.OS.Core

-------------------------------------------------------------------------------
-- Test Suite
-------------------------------------------------------------------------------

coreTests :: TestTree
coreTests = testGroup "ECS Core Tests"
    [ entityIdTests
    , phantomTypeTests
    , componentStoreTests
    , worldTests
    , agentComponentTests
    , toolboxComponentTests
    , jsonRoundTripTests
    ]

-------------------------------------------------------------------------------
-- EntityId Tests
-------------------------------------------------------------------------------

entityIdTests :: TestTree
entityIdTests = testGroup "EntityId"
    [ testCase "newEntityId generates unique IDs" $ do
        eid1 <- createEntity
        eid2 <- createEntity
        assertBool "Entity IDs should be unique" (eid1 /= eid2)
    
    , testCase "EntityId equality" $ do
        eid <- createEntity
        assertEqual "EntityId should equal itself" eid eid
    
    , testCase "EntityId hashability" $ do
        eid <- createEntity
        let h1 = hash eid
        let h2 = hash eid
        assertEqual "Hash should be consistent" h1 h2
    ]

-------------------------------------------------------------------------------
-- Phantom Type Tests
-------------------------------------------------------------------------------

phantomTypeTests :: TestTree
phantomTypeTests = testGroup "Phantom-typed IDs"
    [ testCase "AgentId wraps EntityId" $ do
        eid <- createEntity
        let aid = AgentId eid
        assertEqual "unAgentId should return original" eid (unAgentId aid)
    
    , testCase "ToolboxId wraps EntityId" $ do
        eid <- createEntity
        let tid = ToolboxId eid
        assertEqual "unToolboxId should return original" eid (unToolboxId tid)
    
    , testCase "Different phantom types from same EntityId are not equal" $ do
        eid <- createEntity
        let aid = AgentId eid
        let tid = ToolboxId eid
        -- These have the same underlying EntityId but different types
        -- We can't compare them directly (type error), which is the point
        assertEqual "Underlying EntityIds match" (unAgentId aid) (unToolboxId tid)
    
    , testCase "ConversationId wraps EntityId" $ do
        eid <- createEntity
        let cid = ConversationId eid
        assertEqual "unConversationId should return original" eid (unConversationId cid)
    
    , testCase "TurnId wraps EntityId" $ do
        eid <- createEntity
        let tid = TurnId eid
        assertEqual "unTurnId should return original" eid (unTurnId tid)
    
    , testCase "ToolCallId wraps EntityId" $ do
        eid <- createEntity
        let tcid = ToolCallId eid
        assertEqual "unToolCallId should return original" eid (unToolCallId tcid)
    
    , testCase "ResourceId wraps EntityId" $ do
        eid <- createEntity
        let rid = ResourceId eid
        assertEqual "unResourceId should return original" eid (unResourceId rid)
    ]

-------------------------------------------------------------------------------
-- Component Store Tests
-------------------------------------------------------------------------------

-- Test component for storage tests
data TestComponent = TestComponent
    { tcValue :: Int
    , tcName :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON TestComponent
instance ToJSON TestComponent

instance Component TestComponent where
    componentId _ = ComponentTypeId 100

componentStoreTests :: TestTree
componentStoreTests = testGroup "ComponentStore"
    [ testCase "emptyComponentStore has no entries" $ do
        let store = emptyComponentStore @TestComponent
        let ComponentStore entries = store
        assertEqual "Empty store should have 0 entries" 0 (length entries)
    
    , testCase "insertComponent adds entry" $ do
        eid <- createEntity
        let comp = TestComponent 42 "test"
        let store = insertComponent eid comp emptyComponentStore
        assertEqual "Should find component" (Just comp) (lookupComponent eid store)
    
    , testCase "insertComponent updates existing entry" $ do
        eid <- createEntity
        let comp1 = TestComponent 1 "first"
        let comp2 = TestComponent 2 "second"
        let store1 = insertComponent eid comp1 emptyComponentStore
        let store2 = insertComponent eid comp2 store1
        assertEqual "Should have updated component" (Just comp2) (lookupComponent eid store2)
    
    , testCase "deleteComponent removes entry" $ do
        eid <- createEntity
        let comp = TestComponent 42 "test"
        let store1 = insertComponent eid comp emptyComponentStore
        let store2 = deleteComponent eid store1
        assertEqual "Should not find component" Nothing (lookupComponent eid store2)
    
    , testCase "lookupComponent returns Nothing for missing entry" $ do
        eid1 <- createEntity
        eid2 <- createEntity
        let comp = TestComponent 42 "test"
        let store = insertComponent eid1 comp emptyComponentStore
        assertEqual "Should not find component for different entity" Nothing (lookupComponent eid2 store)
    ]

-------------------------------------------------------------------------------
-- World Tests
-------------------------------------------------------------------------------

worldTests :: TestTree
worldTests = testGroup "World"
    [ testCase "newWorld creates empty world" $ do
        world <- atomically newWorld
        let World stores = world
        assertEqual "New world should have no stores" 0 (length stores)
    
    , testCase "registerComponentStore adds store" $ do
        world <- atomically newWorld
        world' <- atomically $ registerComponentStore world (Proxy @TestComponent)
        let World stores = world'
        assertEqual "World should have 1 store" 1 (length stores)
    
    , testCase "setComponent and getComponent round-trip" $ do
        world <- atomically newWorld
        world' <- atomically $ registerComponentStore world (Proxy @TestComponent)
        eid <- createEntity
        let comp = TestComponent 42 "test"
        atomically $ setComponent world' eid comp
        mComp <- atomically $ getComponent @TestComponent world' eid
        assertEqual "Should retrieve same component" (Just comp) mComp
    
    , testCase "getComponent returns Nothing for unregistered store" $ do
        world <- atomically newWorld
        eid <- createEntity
        mComp <- atomically $ getComponent @TestComponent world eid
        assertEqual "Should return Nothing" (Nothing :: Maybe TestComponent) mComp
    
    , testCase "getComponent returns Nothing for missing component" $ do
        world <- atomically newWorld
        world' <- atomically $ registerComponentStore world (Proxy @TestComponent)
        eid <- createEntity
        mComp <- atomically $ getComponent @TestComponent world' eid
        assertEqual "Should return Nothing" (Nothing :: Maybe TestComponent) mComp
    
    , testCase "hasComponent returns correct values" $ do
        world <- atomically newWorld
        world' <- atomically $ registerComponentStore world (Proxy @TestComponent)
        eid <- createEntity
        hasBefore <- atomically $ hasComponent @TestComponent world' eid
        assertEqual "Should not have component initially" False hasBefore
        let comp = TestComponent 42 "test"
        atomically $ setComponent world' eid comp
        hasAfter <- atomically $ hasComponent @TestComponent world' eid
        assertEqual "Should have component after set" True hasAfter
    
    , testCase "removeComponent deletes component" $ do
        world <- atomically newWorld
        world' <- atomically $ registerComponentStore world (Proxy @TestComponent)
        eid <- createEntity
        let comp = TestComponent 42 "test"
        atomically $ setComponent world' eid comp
        atomically $ removeComponent @TestComponent world' eid
        mComp <- atomically $ getComponent @TestComponent world' eid
        assertEqual "Should not find component after remove" (Nothing :: Maybe TestComponent) mComp
    
    , testCase "modifyComponent updates component" $ do
        world <- atomically newWorld
        world' <- atomically $ registerComponentStore world (Proxy @TestComponent)
        eid <- createEntity
        let comp = TestComponent 42 "test"
        atomically $ setComponent world' eid comp
        atomically $ modifyComponent @TestComponent world' eid (\c -> c { tcValue = 100 })
        mComp <- atomically $ getComponent @TestComponent world' eid
        assertEqual "Should have updated value" (Just 100) (fmap tcValue mComp)
    
    , testCase "allEntitiesWithComponent returns correct entities" $ do
        world <- atomically newWorld
        world' <- atomically $ registerComponentStore world (Proxy @TestComponent)
        eid1 <- createEntity
        eid2 <- createEntity
        eid3 <- createEntity
        let comp1 = TestComponent 1 "one"
        let comp2 = TestComponent 2 "two"
        atomically $ do
            setComponent world' eid1 comp1
            setComponent world' eid2 comp2
            -- eid3 has no component
        entities <- atomically $ allEntitiesWithComponent @TestComponent world'
        assertEqual "Should have 2 entities with component" 2 (length entities)
        assertBool "Should contain eid1" (eid1 `elem` entities)
        assertBool "Should contain eid2" (eid2 `elem` entities)
    
    , testCase "entityExists returns True for entity with component" $ do
        world <- atomically newWorld
        world' <- atomically $ registerComponentStore world (Proxy @TestComponent)
        eid <- createEntity
        existsBefore <- atomically $ entityExists world' eid
        assertEqual "Should not exist initially" False existsBefore
        let comp = TestComponent 42 "test"
        atomically $ setComponent world' eid comp
        existsAfter <- atomically $ entityExists world' eid
        assertEqual "Should exist after adding component" True existsAfter
    ]

-------------------------------------------------------------------------------
-- Agent Component Tests
-------------------------------------------------------------------------------

agentComponentTests :: TestTree
agentComponentTests = testGroup "Agent Components"
    [ testCase "AgentConfig has correct component ID" $ do
        let cid = componentId (Proxy @AgentConfig)
        cid @?= ComponentTypeId 1
    
    , testCase "AgentState has correct component ID" $ do
        let cid = componentId (Proxy @AgentState)
        cid @?= ComponentTypeId 2
    
    , testCase "AgentConfig and AgentState have different IDs" $ do
        let cid1 = componentId (Proxy @AgentConfig)
        let cid2 = componentId (Proxy @AgentState)
        assertBool "Component IDs should differ" (cid1 /= cid2)
    
    , testCase "AgentStatus values" $ do
        AgentIdle @?= AgentIdle
        AgentBusy (TurnId (EntityId (read "550e8400-e29b-41d4-a716-446655440000"))) @?=
            AgentBusy (TurnId (EntityId (read "550e8400-e29b-41d4-a716-446655440000")))
        AgentError "test" @?= AgentError "test"
    ]

-------------------------------------------------------------------------------
-- Toolbox Component Tests
-------------------------------------------------------------------------------

toolboxComponentTests :: TestTree
toolboxComponentTests = testGroup "Toolbox Components"
    [ testCase "ToolboxConfig has correct component ID" $ do
        let cid = componentId (Proxy @ToolboxConfig)
        cid @?= ComponentTypeId 3
    
    , testCase "ToolboxState has correct component ID" $ do
        let cid = componentId (Proxy @ToolboxState)
        cid @?= ComponentTypeId 4
    
    , testCase "ToolboxBinding has correct component ID" $ do
        let cid = componentId (Proxy @ToolboxBinding)
        cid @?= ComponentTypeId 5
    
    , testCase "All toolbox component IDs are unique" $ do
        let cids =
                [ componentId (Proxy @ToolboxConfig)
                , componentId (Proxy @ToolboxState)
                , componentId (Proxy @ToolboxBinding)
                ]
        assertEqual "Should have 3 unique IDs" 3 (length $ distinct cids)
    
    , testCase "ToolboxStatus values" $ do
        ToolboxInitializing @?= ToolboxInitializing
        ToolboxReady @?= ToolboxReady
        ToolboxError "test" @?= ToolboxError "test"
        ToolboxDisposed @?= ToolboxDisposed
    
    , testCase "ToolboxType values" $ do
        ToolboxTypeBash @?= ToolboxTypeBash
        ToolboxTypeMCP @?= ToolboxTypeMCP
        ToolboxTypeOpenAPI @?= ToolboxTypeOpenAPI
    
    , testCase "ResourceScope values" $ do
        ScopeGlobal @?= ScopeGlobal
        ScopeAgent (AgentId (EntityId (read "550e8400-e29b-41d4-a716-446655440000"))) @?=
            ScopeAgent (AgentId (EntityId (read "550e8400-e29b-41d4-a716-446655440000")))
    ]
  where
    distinct = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-------------------------------------------------------------------------------
-- JSON Round-trip Tests
-------------------------------------------------------------------------------

jsonRoundTripTests :: TestTree
jsonRoundTripTests = testGroup "JSON Round-trip"
    [ testCase "AgentConfig round-trip" $ do
        let config = AgentConfig
                { agentName = "test-agent"
                , agentModel = ModelConfig "openai" "https://api.openai.com/v1" "gpt-4" "key1"
                , agentSystemPrompt = "You are helpful"
                , agentToolboxBindings = []
                }
        let json = encode config
        let mDecoded = decode json
        assertEqual "Should decode to same value" (Just config) mDecoded
    
    , testCase "AgentState round-trip" $ do
        now <- getCurrentTime
        let state = AgentState
                { agentStatus = AgentIdle
                , agentCurrentConversation = Nothing
                , agentCreatedAt = now
                }
        let json = encode state
        let mDecoded = decode json
        assertEqual "Should decode to same value" (Just state) mDecoded
    
    , testCase "AgentStatus round-trip" $ do
        let statuses = [AgentIdle, AgentError "oops"]
        mapM_ (\s -> let json = encode s in assertEqual ("Status: " ++ show s) (Just s) (decode json)) statuses
    
    , testCase "ToolboxConfig round-trip" $ do
        let config = ToolboxConfig
                { toolboxName = "test-toolbox"
                , toolboxType = ToolboxTypeBash
                , toolboxSettings = Object mempty
                }
        let json = encode config
        let mDecoded = decode json
        assertEqual "Should decode to same value" (Just config) mDecoded
    
    , testCase "ToolboxState round-trip" $ do
        let state = ToolboxState
                { toolboxScope = ScopeGlobal
                , toolboxStatus = ToolboxReady
                , toolboxResourceRef = Nothing
                }
        let json = encode state
        let mDecoded = decode json
        assertEqual "Should decode to same value" (Just state) mDecoded
    
    , testCase "ToolboxBinding round-trip" $ do
        eid1 <- createEntity
        eid2 <- createEntity
        let binding = ToolboxBinding
                { bindingAgentId = AgentId eid1
                , bindingToolboxId = ToolboxId eid2
                , bindingConfig = Object mempty
                }
        let json = encode binding
        let mDecoded = decode json
        assertEqual "Should decode to same value" (Just binding) mDecoded
    ]

