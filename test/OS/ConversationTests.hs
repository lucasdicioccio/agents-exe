{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Tests for conversation, turn, tool call, and lineage components.

These tests verify:
- Component type assignments
- Conversation lifecycle management
- Turn branching support
- Tool call nesting
- Lineage construction and context building
- JSON serialization
-}
module OS.ConversationTests (
    conversationTests,
) where

import Control.Concurrent.STM (atomically, newTVarIO)
import Data.Aeson (FromJSON, ToJSON, Value (..), decode, encode, toJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

import System.Agents.OS.Core
import System.Agents.OS.Conversation

-------------------------------------------------------------------------------
-- Test Suite
-------------------------------------------------------------------------------

conversationTests :: TestTree
conversationTests =
    testGroup
        "Conversation Tests"
        [ componentIdTests
        , conversationLifecycleTests
        , turnLifecycleTests
        , toolCallTests
        , lineageTests
        , branchingTests
        , jsonRoundTripTests
        , utilityFunctionTests
        ]

-------------------------------------------------------------------------------
-- Component ID Tests
-------------------------------------------------------------------------------

componentIdTests :: TestTree
componentIdTests =
    testGroup
        "Component IDs"
        [ testCase "ConversationConfig has ID 30" $ do
            let cid = componentId (Proxy @ConversationConfig)
            cid @?= ComponentTypeId 30
        , testCase "ConversationState has ID 31" $ do
            let cid = componentId (Proxy @ConversationState)
            cid @?= ComponentTypeId 31
        , testCase "AgentConversation has ID 32" $ do
            let cid = componentId (Proxy @AgentConversation)
            cid @?= ComponentTypeId 32
        , testCase "TurnConfig has ID 33" $ do
            let cid = componentId (Proxy @TurnConfig)
            cid @?= ComponentTypeId 33
        , testCase "TurnState has ID 34" $ do
            let cid = componentId (Proxy @TurnState)
            cid @?= ComponentTypeId 34
        , testCase "ToolCallConfig has ID 35" $ do
            let cid = componentId (Proxy @ToolCallConfig)
            cid @?= ComponentTypeId 35
        , testCase "ToolCallState has ID 36" $ do
            let cid = componentId (Proxy @ToolCallState)
            cid @?= ComponentTypeId 36
        , testCase "Lineage has ID 37" $ do
            let cid = componentId (Proxy @Lineage)
            cid @?= ComponentTypeId 37
        , testCase "Message has ID 38" $ do
            let cid = componentId (Proxy @Message)
            cid @?= ComponentTypeId 38
        , testCase "All conversation component IDs are unique" $ do
            let cids =
                    [ componentId (Proxy @ConversationConfig)
                    , componentId (Proxy @ConversationState)
                    , componentId (Proxy @AgentConversation)
                    , componentId (Proxy @TurnConfig)
                    , componentId (Proxy @TurnState)
                    , componentId (Proxy @ToolCallConfig)
                    , componentId (Proxy @ToolCallState)
                    , componentId (Proxy @Lineage)
                    , componentId (Proxy @Message)
                    ]
            assertEqual "Should have 9 unique IDs" 9 (length $ distinct cids)
        ]
  where
    distinct = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-------------------------------------------------------------------------------
-- Conversation Lifecycle Tests
-------------------------------------------------------------------------------

conversationLifecycleTests :: TestTree
conversationLifecycleTests =
    testGroup
        "Conversation Lifecycle"
        [ testCase "Create active conversation" $ do
            now <- getCurrentTime
            lastActivity <- newTVarIO now
            let state =
                    ConversationState
                        { conversationAgentId = AgentId (EntityId $ read "550e8400-e29b-41d4-a716-446655440000")
                        , conversationStatus = ConversationActive
                        , conversationStartedAt = now
                        , conversationLastActivity = lastActivity
                        }
            assertBool "Should be active" (isConversationActive state)
        , testCase "Create paused conversation" $ do
            now <- getCurrentTime
            lastActivity <- newTVarIO now
            let state =
                    ConversationState
                        { conversationAgentId = AgentId (EntityId $ read "550e8400-e29b-41d4-a716-446655440000")
                        , conversationStatus = ConversationPaused
                        , conversationStartedAt = now
                        , conversationLastActivity = lastActivity
                        }
            assertBool "Should not be active when paused" (not $ isConversationActive state)
        , testCase "ConversationStatus values" $ do
            ConversationActive @?= ConversationActive
            ConversationPaused @?= ConversationPaused
            ConversationArchived @?= ConversationArchived
            ConversationError "test" @?= ConversationError "test"
        , testCase "AgentRole values" $ do
            PrimaryAgent @?= PrimaryAgent
            AssistantAgent @?= AssistantAgent
            ObserverAgent @?= ObserverAgent
        , testCase "ConversationConfig with metadata" $ do
            let config =
                    ConversationConfig
                        { conversationTitle = Just "Test Chat"
                        , conversationMetadata = Map.fromList [("key", toJSON ("value" :: Text))]
                        }
            conversationTitle config @?= Just "Test Chat"
            assertEqual "Should have 1 metadata entry" 1 (Map.size $ conversationMetadata config)
        , testCase "AgentConversation linking" $ do
            let agentConv =
                    AgentConversation
                        { acAgentId = AgentId (EntityId $ read "550e8400-e29b-41d4-a716-446655440000")
                        , acConversationId = ConversationId (EntityId $ read "660e8400-e29b-41d4-a716-446655440001")
                        , acRole = PrimaryAgent
                        }
            acRole agentConv @?= PrimaryAgent
        ]

-------------------------------------------------------------------------------
-- Turn Lifecycle Tests
-------------------------------------------------------------------------------

turnLifecycleTests :: TestTree
turnLifecycleTests =
    testGroup
        "Turn Lifecycle"
        [ testCase "Create root turn" $ do
            now <- getCurrentTime
            let config =
                    TurnConfig
                        { turnConversationId = ConversationId (EntityId $ read "550e8400-e29b-41d4-a716-446655440000")
                        , turnParentTurnId = Nothing
                        }
            let state =
                    TurnState
                        { turnStatus = TurnStarting
                        , turnStartedAt = now
                        , turnCompletedAt = Nothing
                        }
            isNothing (turnParentTurnId config) @?= True
            turnStatus state @?= TurnStarting
        , testCase "Turn transitions to running" $ do
            now <- getCurrentTime
            let state =
                    TurnState
                        { turnStatus = TurnRunning
                        , turnStartedAt = now
                        , turnCompletedAt = Nothing
                        }
            assertBool "Should not be completed" (not $ isTurnCompleted state)
        , testCase "Turn completes successfully" $ do
            now <- getCurrentTime
            let state =
                    TurnState
                        { turnStatus = TurnCompleted "Hello!"
                        , turnStartedAt = now
                        , turnCompletedAt = Just now
                        }
            assertBool "Should be completed" (isTurnCompleted state)
        , testCase "Turn fails" $ do
            now <- getCurrentTime
            let state =
                    TurnState
                        { turnStatus = TurnFailed "Error occurred"
                        , turnStartedAt = now
                        , turnCompletedAt = Just now
                        }
            assertBool "Should be completed (with failure)" (isTurnCompleted state)
        , testCase "Turn with pending tool calls" $ do
            now <- getCurrentTime
            let toolCallIds = [ToolCallId (EntityId $ read "550e8400-e29b-41d4-a716-446655440000")]
            let state =
                    TurnState
                        { turnStatus = TurnToolCallsPending toolCallIds
                        , turnStartedAt = now
                        , turnCompletedAt = Nothing
                        }
            case turnStatus state of
                TurnToolCallsPending ids -> ids @?= toolCallIds
                _ -> assertBool "Expected TurnToolCallsPending" False
        , testCase "TurnStatus values" $ do
            TurnStarting @?= TurnStarting
            TurnRunning @?= TurnRunning
            TurnCompleted "test" @?= TurnCompleted "test"
            TurnFailed "error" @?= TurnFailed "error"
        ]

-------------------------------------------------------------------------------
-- Tool Call Tests
-------------------------------------------------------------------------------

toolCallTests :: TestTree
toolCallTests =
    testGroup
        "Tool Calls"
        [ testCase "Create top-level tool call" $ do
            let config =
                    ToolCallConfig
                        { tcTurnId = TurnId (EntityId $ read "550e8400-e29b-41d4-a716-446655440000")
                        , tcToolName = "bash"
                        , tcToolInput = toJSON ("ls" :: Text)
                        , tcParentCallId = Nothing
                        }
            tcToolName config @?= "bash"
            isNothing (tcParentCallId config) @?= True
        , testCase "Create nested tool call" $ do
            let parentId = ToolCallId (EntityId $ read "550e8400-e29b-41d4-a716-446655440000")
            let config =
                    ToolCallConfig
                        { tcTurnId = TurnId (EntityId $ read "550e8400-e29b-41d4-a716-446655440001")
                        , tcToolName = "agent"
                        , tcToolInput = toJSON ("query" :: Text)
                        , tcParentCallId = Just parentId
                        }
            tcParentCallId config @?= Just parentId
        , testCase "Tool call pending" $ do
            now <- getCurrentTime
            let state =
                    ToolCallState
                        { tcStatus = TcPending
                        , tcStartedAt = now
                        , tcCompletedAt = Nothing
                        , tcResult = Nothing
                        }
            assertBool "Should not be completed" (not $ isToolCallCompleted state)
        , testCase "Tool call executing" $ do
            now <- getCurrentTime
            let state =
                    ToolCallState
                        { tcStatus = TcExecuting
                        , tcStartedAt = now
                        , tcCompletedAt = Nothing
                        , tcResult = Nothing
                        }
            assertBool "Should not be completed while executing" (not $ isToolCallCompleted state)
        , testCase "Tool call completed" $ do
            now <- getCurrentTime
            let result = toJSON ("output" :: Text)
            let state =
                    ToolCallState
                        { tcStatus = TcCompleted result
                        , tcStartedAt = now
                        , tcCompletedAt = Just now
                        , tcResult = Just result
                        }
            assertBool "Should be completed" (isToolCallCompleted state)
            getToolCallResult state @?= Just result
        , testCase "Tool call failed" $ do
            now <- getCurrentTime
            let state =
                    ToolCallState
                        { tcStatus = TcFailed "Command not found"
                        , tcStartedAt = now
                        , tcCompletedAt = Just now
                        , tcResult = Nothing
                        }
            assertBool "Should be completed (with failure)" (isToolCallCompleted state)
        , testCase "Tool call cancelled" $ do
            now <- getCurrentTime
            let state =
                    ToolCallState
                        { tcStatus = TcCancelled
                        , tcStartedAt = now
                        , tcCompletedAt = Just now
                        , tcResult = Nothing
                        }
            assertBool "Should be completed (cancelled)" (isToolCallCompleted state)
        , testCase "ToolCallStatus values" $ do
            TcPending @?= TcPending
            TcExecuting @?= TcExecuting
            TcCancelled @?= TcCancelled
        , testCase "Default max tool call depth" $ do
            defaultMaxToolCallDepth @?= 10
        ]

-------------------------------------------------------------------------------
-- Lineage Tests
-------------------------------------------------------------------------------

lineageTests :: TestTree
lineageTests =
    testGroup
        "Lineage Tracking"
        [ testCase "Empty lineage" $ do
            let lineage = emptyLineage
            lineageDepth lineage @?= 0
            isNothing (lineageHead lineage) @?= True
            isNothing (lineageRoot lineage) @?= True
        , testCase "Push single frame" $ do
            now <- getCurrentTime
            let eid = EntityId $ read "550e8400-e29b-41d4-a716-446655440000"
            let lineage = pushLineage ConversationFrame eid now emptyLineage
            lineageDepth lineage @?= 1
            isJust (lineageHead lineage) @?= True
        , testCase "Push multiple frames" $ do
            now <- getCurrentTime
            let convId = EntityId $ read "550e8400-e29b-41d4-a716-446655440000"
            let turnId = EntityId $ read "550e8400-e29b-41d4-a716-446655440001"
            let toolId = EntityId $ read "550e8400-e29b-41d4-a716-446655440002"
            let lineage =
                    pushLineage ConversationFrame convId now $
                        pushLineage TurnFrame turnId now $
                            pushLineage ToolCallFrame toolId now emptyLineage
            lineageDepth lineage @?= 3
            currentFrameType lineage @?= Just ToolCallFrame
        , testCase "Lineage head and root" $ do
            now <- getCurrentTime
            let convId = EntityId $ read "550e8400-e29b-41d4-a716-446655440000"
            let turnId = EntityId $ read "550e8400-e29b-41d4-a716-446655440001"
            let lineage = pushLineage ConversationFrame convId now $ pushLineage TurnFrame turnId now emptyLineage
            case lineageHead lineage of
                Just frame -> frameType frame @?= TurnFrame
                Nothing -> assertBool "Expected head" False
            case lineageRoot lineage of
                Just frame -> frameType frame @?= ConversationFrame
                Nothing -> assertBool "Expected root" False
        , testCase "Build lineage context" $ do
            now <- getCurrentTime
            let convId = EntityId $ read "550e8400-e29b-41d4-a716-446655440000"
            let turnId = EntityId $ read "550e8400-e29b-41d4-a716-446655440001"
            let lineage = pushLineage ConversationFrame convId now $ pushLineage TurnFrame turnId now emptyLineage
            let context = buildLineageContext lineage
            assertBool "Should have depth" (Map.member "lineageDepth" context)
            assertBool "Should have path" (Map.member "lineagePath" context)
        , testCase "Build lineage path" $ do
            now <- getCurrentTime
            let convId = EntityId $ read "550e8400-e29b-41d4-a716-446655440000"
            let turnId = EntityId $ read "550e8400-e29b-41d4-a716-446655440001"
            let lineage = pushLineage ConversationFrame convId now $ pushLineage TurnFrame turnId now emptyLineage
            let path = buildLineagePath lineage
            assertBool "Path should contain conversation" (Text.isInfixOf "conversation" path)
            assertBool "Path should contain turn" (Text.isInfixOf "turn" path)
        , testCase "Find frames by type" $ do
            now <- getCurrentTime
            let convId = EntityId $ read "550e8400-e29b-41d4-a716-446655440000"
            let turnId = EntityId $ read "550e8400-e29b-41d4-a716-446655440001"
            let toolId = EntityId $ read "550e8400-e29b-41d4-a716-446655440002"
            let lineage =
                    pushLineage ConversationFrame convId now $
                        pushLineage TurnFrame turnId now $
                            pushLineage ToolCallFrame toolId now emptyLineage
            length (findConversationFrames lineage) @?= 1
            length (findTurnFrames lineage) @?= 1
            length (findToolCallFrames lineage) @?= 1
        , testCase "Check isInConversation" $ do
            now <- getCurrentTime
            let convId = ConversationId $ EntityId $ read "550e8400-e29b-41d4-a716-446655440000"
            let otherConvId = ConversationId $ EntityId $ read "660e8400-e29b-41d4-a716-446655440001"
            let lineage = pushLineage ConversationFrame (unConversationId convId) now emptyLineage
            assertBool "Should be in conversation" (isInConversation convId lineage)
            assertBool "Should not be in other conversation" (not $ isInConversation otherConvId lineage)
        , testCase "FrameType values" $ do
            ConversationFrame @?= ConversationFrame
            TurnFrame @?= TurnFrame
            ToolCallFrame @?= ToolCallFrame
            SystemFrame @?= SystemFrame
        ]

-------------------------------------------------------------------------------
-- Branching Tests
-------------------------------------------------------------------------------

branchingTests :: TestTree
branchingTests =
    testGroup
        "Conversation Branching"
        [ testCase "Create branch from turn" $ do
            now <- getCurrentTime
            let parentTurnId = TurnId (EntityId $ read "550e8400-e29b-41d4-a716-446655440000")
            let branchConfig =
                    TurnConfig
                        { turnConversationId = ConversationId (EntityId $ read "550e8400-e29b-41d4-a716-446655440001")
                        , turnParentTurnId = Just parentTurnId
                        }
            turnParentTurnId branchConfig @?= Just parentTurnId
        , testCase "Multiple branches from same parent" $ do
            let parentTurnId = TurnId (EntityId $ read "550e8400-e29b-41d4-a716-446655440000")
            let branch1 =
                    TurnConfig
                        { turnConversationId = ConversationId (EntityId $ read "550e8400-e29b-41d4-a716-446655440001")
                        , turnParentTurnId = Just parentTurnId
                        }
            let branch2 =
                    TurnConfig
                        { turnConversationId = ConversationId (EntityId $ read "550e8400-e29b-41d4-a716-446655440001")
                        , turnParentTurnId = Just parentTurnId
                        }
            turnParentTurnId branch1 @?= turnParentTurnId branch2
        ]

-------------------------------------------------------------------------------
-- JSON Round-trip Tests
-------------------------------------------------------------------------------

jsonRoundTripTests :: TestTree
jsonRoundTripTests =
    testGroup
        "JSON Round-trip"
        [ testCase "ConversationConfig round-trip" $ do
            let config =
                    ConversationConfig
                        { conversationTitle = Just "Test"
                        , conversationMetadata = Map.empty
                        }
            let json = encode config
            let mDecoded = decode json
            assertEqual "Should decode to same value" (Just config) mDecoded
        , testCase "ConversationStatus round-trip" $ do
            let statuses = [ConversationActive, ConversationPaused, ConversationArchived, ConversationError "test"]
            mapM_ (\s -> let json = encode s in assertEqual ("Status: " ++ show s) (Just s) (decode json)) statuses
        , testCase "AgentRole round-trip" $ do
            let roles = [PrimaryAgent, AssistantAgent, ObserverAgent]
            mapM_ (\r -> let json = encode r in assertEqual ("Role: " ++ show r) (Just r) (decode json)) roles
        , testCase "TurnConfig round-trip" $ do
            let config =
                    TurnConfig
                        { turnConversationId = ConversationId (EntityId $ read "550e8400-e29b-41d4-a716-446655440000")
                        , turnParentTurnId = Nothing
                        }
            let json = encode config
            let mDecoded = decode json
            assertEqual "Should decode to same value" (Just config) mDecoded
        , testCase "TurnStatus round-trip" $ do
            let statuses = [TurnStarting, TurnRunning, TurnCompleted "result", TurnFailed "error"]
            mapM_ (\s -> let json = encode s in assertEqual ("Status: " ++ show s) (Just s) (decode json)) statuses
        , testCase "ToolCallConfig round-trip" $ do
            let config =
                    ToolCallConfig
                        { tcTurnId = TurnId (EntityId $ read "550e8400-e29b-41d4-a716-446655440000")
                        , tcToolName = "bash"
                        , tcToolInput = toJSON ("ls" :: Text)
                        , tcParentCallId = Nothing
                        }
            let json = encode config
            let mDecoded = decode json
            assertEqual "Should decode to same value" (Just config) mDecoded
        , testCase "ToolCallStatus round-trip" $ do
            let result = toJSON ("output" :: Text)
            let statuses = [TcPending, TcExecuting, TcCompleted result, TcFailed "error", TcCancelled]
            mapM_ (\s -> let json = encode s in assertEqual ("Status: " ++ show s) (Just s) (decode json)) statuses
        , testCase "Lineage round-trip" $ do
            now <- getCurrentTime
            let convId = EntityId $ read "550e8400-e29b-41d4-a716-446655440000"
            let lineage = pushLineage ConversationFrame convId now emptyLineage
            let json = encode lineage
            let mDecoded = decode json
            assertEqual "Should decode to same value" (Just lineage) mDecoded
        , testCase "FrameType round-trip" $ do
            let types = [ConversationFrame, TurnFrame, ToolCallFrame, SystemFrame]
            mapM_ (\t -> let json = encode t in assertEqual ("Type: " ++ show t) (Just t) (decode json)) types
        , testCase "LineageFrame round-trip" $ do
            now <- getCurrentTime
            let frame = LineageFrame ConversationFrame (EntityId $ read "550e8400-e29b-41d4-a716-446655440000") now
            let json = encode frame
            let mDecoded = decode json
            assertEqual "Should decode to same value" (Just frame) mDecoded
        , testCase "Message round-trip" $ do
            now <- getCurrentTime
            let msg =
                    Message
                        { msgConversationId = ConversationId (EntityId $ read "550e8400-e29b-41d4-a716-446655440000")
                        , msgTurnId = TurnId (EntityId $ read "550e8400-e29b-41d4-a716-446655440001")
                        , msgRole = AssistantRole
                        , msgContent = "Hello!"
                        , msgToolCalls = []
                        , msgTimestamp = now
                        }
            let json = encode msg
            let mDecoded = decode json
            assertEqual "Should decode to same value" (Just msg) mDecoded
        , testCase "MessageRole round-trip" $ do
            let roles = [UserRole, AssistantRole, SystemRole, ToolRole]
            mapM_ (\r -> let json = encode r in assertEqual ("Role: " ++ show r) (Just r) (decode json)) roles
        ]

-------------------------------------------------------------------------------
-- Utility Function Tests
-------------------------------------------------------------------------------

utilityFunctionTests :: TestTree
utilityFunctionTests =
    testGroup
        "Utility Functions"
        [ testCase "isConversationActive for all statuses" $ do
            now <- getCurrentTime
            lastActivity <- newTVarIO now
            let baseState =
                    ConversationState
                        { conversationAgentId = AgentId (EntityId $ read "550e8400-e29b-41d4-a716-446655440000")
                        , conversationStartedAt = now
                        , conversationLastActivity = lastActivity
                        }
            let activeState = baseState{conversationStatus = ConversationActive}
            let pausedState = baseState{conversationStatus = ConversationPaused}
            let archivedState = baseState{conversationStatus = ConversationArchived}
            let errorState = baseState{conversationStatus = ConversationError "oops"}
            assertBool "Active should be active" (isConversationActive activeState)
            assertBool "Paused should not be active" (not $ isConversationActive pausedState)
            assertBool "Archived should not be active" (not $ isConversationActive archivedState)
            assertBool "Error should not be active" (not $ isConversationActive errorState)
        , testCase "isTurnCompleted for all statuses" $ do
            now <- getCurrentTime
            let starting = TurnState{turnStatus = TurnStarting, turnStartedAt = now, turnCompletedAt = Nothing}
            let running = TurnState{turnStatus = TurnRunning, turnStartedAt = now, turnCompletedAt = Nothing}
            let pending = TurnState{turnStatus = TurnToolCallsPending [], turnStartedAt = now, turnCompletedAt = Nothing}
            let completed = TurnState{turnStatus = TurnCompleted "done", turnStartedAt = now, turnCompletedAt = Just now}
            let failed = TurnState{turnStatus = TurnFailed "error", turnStartedAt = now, turnCompletedAt = Just now}
            assertBool "Starting should not be completed" (not $ isTurnCompleted starting)
            assertBool "Running should not be completed" (not $ isTurnCompleted running)
            assertBool "Pending should not be completed" (not $ isTurnCompleted pending)
            assertBool "Completed should be completed" (isTurnCompleted completed)
            assertBool "Failed should be completed" (isTurnCompleted failed)
        , testCase "isToolCallCompleted for all statuses" $ do
            now <- getCurrentTime
            let result = toJSON ("output" :: Text)
            let pending = ToolCallState{tcStatus = TcPending, tcStartedAt = now, tcCompletedAt = Nothing, tcResult = Nothing}
            let executing = ToolCallState{tcStatus = TcExecuting, tcStartedAt = now, tcCompletedAt = Nothing, tcResult = Nothing}
            let completed = ToolCallState{tcStatus = TcCompleted result, tcStartedAt = now, tcCompletedAt = Just now, tcResult = Just result}
            let failed = ToolCallState{tcStatus = TcFailed "error", tcStartedAt = now, tcCompletedAt = Just now, tcResult = Nothing}
            let cancelled = ToolCallState{tcStatus = TcCancelled, tcStartedAt = now, tcCompletedAt = Just now, tcResult = Nothing}
            assertBool "Pending should not be completed" (not $ isToolCallCompleted pending)
            assertBool "Executing should not be completed" (not $ isToolCallCompleted executing)
            assertBool "Completed should be completed" (isToolCallCompleted completed)
            assertBool "Failed should be completed" (isToolCallCompleted failed)
            assertBool "Cancelled should be completed" (isToolCallCompleted cancelled)
        , testCase "getToolCallResult" $ do
            now <- getCurrentTime
            let result = toJSON ("output" :: Text)
            let completed = ToolCallState{tcStatus = TcCompleted result, tcStartedAt = now, tcCompletedAt = Just now, tcResult = Just result}
            let failed = ToolCallState{tcStatus = TcFailed "error", tcStartedAt = now, tcCompletedAt = Just now, tcResult = Nothing}
            let pending = ToolCallState{tcStatus = TcPending, tcStartedAt = now, tcCompletedAt = Nothing, tcResult = Nothing}
            getToolCallResult completed @?= Just result
            getToolCallResult failed @?= Nothing
            getToolCallResult pending @?= Nothing
        ]

