{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Integration tests for OS Interface layer.

These tests verify that the TUI and OneShot interfaces work correctly
with the RuntimeBridge compatibility layer.
-}
module OS.InterfaceTests (
    tests,
) where

import Control.Concurrent.STM (atomically, newTVarIO, readTVarIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Base (AgentId (..), ConversationId (..), newAgentId)
import System.Agents.OS.Compat.Runtime (
    RuntimeBridge (..),
    initializeOS,
    newRuntimeBridge,
 )
import System.Agents.OS.Interfaces (
    InterfaceConfig (..),
    InterfaceMode (..),
    defaultInterfaceConfig,
    getConversationStatus,
 )
import System.Agents.OS.Interfaces.OneShot (
    OneShotInterfaceConfig (..),
    OneShotPersistence (..),
    defaultOneShotInterfaceConfig,
    executeOneShot,
    extractResultText,
    initOneShotInterface,
 )
import System.Agents.OS.Interfaces.TUI (
    MultiAgentConfig (..),
    TUIInterfaceConfig (..),
    defaultTUIInterfaceConfig,
    getLayoutMode,
    getRegisteredAgents,
    initTUIInterface,
    registerAgent,
    setLayoutMode,
 )
import System.Agents.TUI.Types (
    LayoutMode (..),
 )

-------------------------------------------------------------------------------
-- Test Suite
-------------------------------------------------------------------------------

tests :: TestTree
tests =
    testGroup
        "OS Interface Tests"
        [ runtimeBridgeTests
        , tuiInterfaceTests
        , oneShotInterfaceTests
        , multiAgentTests
        ]

-------------------------------------------------------------------------------
-- Runtime Bridge Tests
-------------------------------------------------------------------------------

runtimeBridgeTests :: TestTree
runtimeBridgeTests =
    testGroup
        "Runtime Bridge"
        [ testCase "can initialize OS" $ do
            os <- initializeOS
            -- OS should be created successfully
            pure ()
        , testCase "can create RuntimeBridge" $ do
            os <- initializeOS
            agentId <- newAgentId
            let bridge = newRuntimeBridge agentId os
            bridgeAgentId bridge @?= agentId
        , testCase "RuntimeBridge stores correct agent ID" $ do
            os <- initializeOS
            agentId <- newAgentId
            let bridge = newRuntimeBridge agentId os
            bridgeAgentId bridge @?= agentId
        ]

-------------------------------------------------------------------------------
-- TUI Interface Tests
-------------------------------------------------------------------------------

tuiInterfaceTests :: TestTree
tuiInterfaceTests =
    testGroup
        "TUI Interface"
        [ testCase "can initialize TUI interface" $ do
            let config = defaultTUIInterfaceConfig
            handle <- initTUIInterface config
            -- Interface should be initialized
            agents <- getRegisteredAgents handle
            agents @?= []
        , testCase "can register an agent" $ do
            let config = defaultTUIInterfaceConfig
            handle <- initTUIInterface config
            (agentId, _bridge) <- registerAgent handle "test-agent"
            agents <- getRegisteredAgents handle
            length agents @?= 1
            -- Use list index instead of head
            case agents of
                (aid, _) : _ -> aid @?= agentId
                [] -> assertFailure "Expected at least one agent"
        , testCase "can register multiple agents" $ do
            let config = defaultTUIInterfaceConfig
            handle <- initTUIInterface config
            (agentId1, _) <- registerAgent handle "test-agent-1"
            (agentId2, _) <- registerAgent handle "test-agent-2"
            agents <- getRegisteredAgents handle
            length agents @?= 2
            -- Check that both agent IDs are present (order doesn't matter)
            let agentIds = map fst agents
            assertBool "Should contain first agent ID" (agentId1 `elem` agentIds)
            assertBool "Should contain second agent ID" (agentId2 `elem` agentIds)
        , testCase "default layout is SingleAgent" $ do
            let config = defaultTUIInterfaceConfig
            handle <- initTUIInterface config
            layout <- getLayoutMode handle
            layout @?= SingleAgent
        , testCase "can change layout mode" $ do
            let config = defaultTUIInterfaceConfig
            handle <- initTUIInterface config
            setLayoutMode handle SplitVertical
            layout <- getLayoutMode handle
            layout @?= SplitVertical
        ]

-------------------------------------------------------------------------------
-- OneShot Interface Tests
-------------------------------------------------------------------------------

oneShotInterfaceTests :: TestTree
oneShotInterfaceTests =
    testGroup
        "OneShot Interface"
        [ testCase "can initialize OneShot interface" $ do
            let config = defaultOneShotInterfaceConfig
            handle <- initOneShotInterface config
            -- Interface should be initialized
            pure ()
        , testCase "default persistence is None" $ do
            let config = defaultOneShotInterfaceConfig
            config.oscPersistence @?= Persistence_None
        , testCase "default thinking is disabled" $ do
            let config = defaultOneShotInterfaceConfig
            config.oscEnableThinking @?= False
        , testCase "default mode is OneShot" $ do
            let config = defaultOneShotInterfaceConfig
            ifcMode (oscBaseConfig config) @?= ModeOneShot
        , testCase "executeOneShot returns placeholder" $ do
            let config = defaultOneShotInterfaceConfig
            handle <- initOneShotInterface config
            result <- executeOneShot handle Nothing "test query"
            extractResultText result @?= "OneShot execution not yet fully implemented - use legacy mainOneShotText"
        ]

-------------------------------------------------------------------------------
-- Multi-Agent Tests
-------------------------------------------------------------------------------

multiAgentTests :: TestTree
multiAgentTests =
    testGroup
        "Multi-Agent Coordination"
        [ testCase "can create multi-agent config" $ do
            let config =
                    (defaultTUIInterfaceConfig)
                        { tuiEnableMultiAgent = True
                        }
            config.tuiEnableMultiAgent @?= True
        , testCase "multi-agent config defaults to disabled" $ do
            let config = defaultTUIInterfaceConfig
            tuiEnableMultiAgent config @?= False
        ]

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- | Helper to check if an agent is in the registered list.
isAgentRegistered :: [(AgentId, a)] -> AgentId -> Bool
isAgentRegistered agents agentId =
    any (\(aid, _) -> aid == agentId) agents

