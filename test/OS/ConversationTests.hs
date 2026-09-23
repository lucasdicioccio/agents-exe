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
- Tool-call ECS entity lifecycle
- Async engine concurrency, progress, and cancellation
-}
module OS.ConversationTests (
    conversationTests,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, newTVarIO)
import Data.Aeson (FromJSON, ToJSON, Value (..), decode, encode, object, parseJSON, toJSON, (.=))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.UUID (nil)
import GHC.Generics (Generic)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase, (@?=))

import qualified System.Agents.Base as Base
import System.Agents.OS.Conversation
import System.Agents.OS.Core
import System.Agents.OS.Core.World (newWorld)
import qualified System.Agents.Session.Async.Engine as AsyncEngine
import System.Agents.Session.Base (Action (..), Agent (..), AsyncYieldStrategy (..), LlmCompletion (..), MissingUserPrompt (..), Session (..), defaultContextConfig)
import System.Agents.Session.Step (naiveTilNoToolCallStep, runStepMAsync, runStepMSync)
import qualified System.Agents.Session.Types as Session
import System.Agents.Tools.Context (ToolCall (..), ToolExecutionContext, ToolPortal, ToolResult (..), ctxFullSession, ctxProgressCallback, mkMinimalContext)
import qualified System.Agents.Tools.Context as Ctx
import System.Agents.Tools.SystemToolbox.ToolCallStatus (cancelToolCallById, getToolCallStatus, listRunningToolCalls)
import System.Agents.Tools.SystemToolbox.Types (
    CancelToolCallParams (..),
    CancelToolCallResult (..),
    GetToolCallStatusParams (..),
    ListRunningToolCallsResult (..),
    RunningToolCallInfo (..),
    ToolCallStatusResult (..),
 )
import qualified Data.UUID as UUID
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
        , toolCallEntityTests
        , lineageTests
        , branchingTests
        , jsonRoundTripTests
        , utilityFunctionTests
        , sessionStepEntityTests
        , asyncEngineTests
        , toolCallStatusTests
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
                        , tcSessionId = Session.SessionId (read "550e8400-e29b-41d4-a716-446655440002")
                        , tcConversationId = Base.ConversationId (read "550e8400-e29b-41d4-a716-446655440003")
                        , tcProviderCallId = Nothing
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
                        , tcSessionId = Session.SessionId (read "550e8400-e29b-41d4-a716-446655440004")
                        , tcConversationId = Base.ConversationId (read "550e8400-e29b-41d4-a716-446655440005")
                        , tcProviderCallId = Nothing
                        }
            tcParentCallId config @?= Just parentId
        , testCase "Tool call pending" $ do
            now <- getCurrentTime
            let state =
                    ToolCallState
                        { tcStatus = TcPending
                        , tcStartedAt = Nothing
                        , tcCompletedAt = Nothing
                        , tcResult = Nothing
                        , tcProgress = []
                        }
            assertBool "Should not be completed" (not $ isToolCallCompleted state)
        , testCase "Tool call executing" $ do
            now <- getCurrentTime
            let state =
                    ToolCallState
                        { tcStatus = TcExecuting
                        , tcStartedAt = Just now
                        , tcCompletedAt = Nothing
                        , tcResult = Nothing
                        , tcProgress = []
                        }
            assertBool "Should not be completed while executing" (not $ isToolCallCompleted state)
        , testCase "Tool call completed" $ do
            now <- getCurrentTime
            let result = toJSON ("output" :: Text)
            let state =
                    ToolCallState
                        { tcStatus = TcCompleted result
                        , tcStartedAt = Just now
                        , tcCompletedAt = Just now
                        , tcResult = Just result
                        , tcProgress = []
                        }
            assertBool "Should be completed" (isToolCallCompleted state)
            getToolCallResult state @?= Just result
        , testCase "Tool call failed" $ do
            now <- getCurrentTime
            let state =
                    ToolCallState
                        { tcStatus = TcFailed "Command not found"
                        , tcStartedAt = Just now
                        , tcCompletedAt = Just now
                        , tcResult = Nothing
                        , tcProgress = []
                        }
            assertBool "Should be completed (with failure)" (isToolCallCompleted state)
        , testCase "Tool call cancelled" $ do
            now <- getCurrentTime
            let state =
                    ToolCallState
                        { tcStatus = TcCancelled
                        , tcStartedAt = Just now
                        , tcCompletedAt = Just now
                        , tcResult = Nothing
                        , tcProgress = []
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
-- Tool Call Entity Tests
-------------------------------------------------------------------------------

toolCallEntityTests :: TestTree
toolCallEntityTests =
    testGroup
        "Tool Call Entities"
        [ testCase "Create and read back a tool-call entity" $ do
            world <- atomically newWorld
            world' <- atomically $ registerToolCallComponents world
            sessId <- Session.newSessionId
            convId <- Base.newConversationId
            turnId <- Session.newTurnId
            callId <- Session.newToolCallId
            let toolInput = toJSON ("ls" :: Text)
            eid <- createToolCallEntity world' sessId convId turnId Nothing "bash" toolInput callId
            (mCfg, mSt) <- atomically $ do
                cfg <- getComponent @ToolCallConfig world' eid
                st <- getComponent @ToolCallState world' eid
                pure (cfg, st)
            case (mCfg, mSt) of
                (Just cfg, Just st) -> do
                    cfg.tcToolName @?= "bash"
                    cfg.tcToolInput @?= toolInput
                    cfg.tcSessionId @?= sessId
                    cfg.tcConversationId @?= convId
                    st.tcStatus @?= TcPending
                    st.tcStartedAt @?= Nothing
                    st.tcProgress @?= []
                _ -> assertBool "Expected config and state" False
        , testCase "Status transitions" $ do
            world <- atomically newWorld
            world' <- atomically $ registerToolCallComponents world
            sessId <- Session.newSessionId
            convId <- Base.newConversationId
            turnId <- Session.newTurnId
            callId <- Session.newToolCallId
            eid <- createToolCallEntity world' sessId convId turnId Nothing "bash" (toJSON ("ls" :: Text)) callId
            startToolCall world' eid
            Just st1 <- atomically $ getComponent @ToolCallState world' eid
            st1.tcStatus @?= TcExecuting
            assertBool "Should have start time" (isJust $ st1.tcStartedAt)
            let result = toJSON ("output" :: Text)
            completeToolCall world' eid result
            Just st2 <- atomically $ getComponent @ToolCallState world' eid
            st2.tcStatus @?= TcCompleted result
            assertBool "Should have completion time" (isJust $ st2.tcCompletedAt)
        , testCase "Fail and cancel transitions" $ do
            world <- atomically newWorld
            world' <- atomically $ registerToolCallComponents world
            sessId <- Session.newSessionId
            convId <- Base.newConversationId
            turnId <- Session.newTurnId
            callId <- Session.newToolCallId
            eid <- createToolCallEntity world' sessId convId turnId Nothing "bash" (toJSON ("ls" :: Text)) callId
            startToolCall world' eid
            failToolCall world' eid "boom"
            Just st1 <- atomically $ getComponent @ToolCallState world' eid
            st1.tcStatus @?= TcFailed "boom"
            callId2 <- Session.newToolCallId
            eid2 <- createToolCallEntity world' sessId convId turnId Nothing "bash" (toJSON ("ls" :: Text)) callId2
            startToolCall world' eid2
            cancelToolCall world' eid2
            Just st2 <- atomically $ getComponent @ToolCallState world' eid2
            st2.tcStatus @?= TcCancelled
        , testCase "Progress updates" $ do
            world <- atomically newWorld
            world' <- atomically $ registerToolCallComponents world
            sessId <- Session.newSessionId
            convId <- Base.newConversationId
            turnId <- Session.newTurnId
            callId <- Session.newToolCallId
            eid <- createToolCallEntity world' sessId convId turnId Nothing "bash" (toJSON ("ls" :: Text)) callId
            now <- getCurrentTime
            let progress = ToolCallProgress now (ProgressLog "working") (toJSON ("x" :: Text))
            addToolCallProgress world' eid progress
            Just st <- atomically $ getComponent @ToolCallState world' eid
            st.tcProgress @?= [progress]
        , testCase "Find entity by session tool-call id" $ do
            world <- atomically newWorld
            world' <- atomically $ registerToolCallComponents world
            sessId <- Session.newSessionId
            convId <- Base.newConversationId
            turnId <- Session.newTurnId
            callId <- Session.newToolCallId
            eid <- createToolCallEntity world' sessId convId turnId Nothing "bash" (toJSON ("ls" :: Text)) callId
            mFound <- atomically $ findToolCallEntityBySessionId world' sessId callId
            mFound @?= Just eid
            otherCallId <- Session.newToolCallId
            mNotFound <- atomically $ findToolCallEntityBySessionId world' sessId otherCallId
            mNotFound @?= Nothing
        , testCase "List tool calls by session and conversation" $ do
            world <- atomically newWorld
            world' <- atomically $ registerToolCallComponents world
            sessId <- Session.newSessionId
            convId <- Base.newConversationId
            otherConvId <- Base.newConversationId
            turnId <- Session.newTurnId
            callId1 <- Session.newToolCallId
            callId2 <- Session.newToolCallId
            _ <- createToolCallEntity world' sessId convId turnId Nothing "a" (toJSON ("1" :: Text)) callId1
            _ <- createToolCallEntity world' sessId otherConvId turnId Nothing "b" (toJSON ("2" :: Text)) callId2
            results <- atomically $ listToolCallsBySessionAndConversation world' sessId convId
            length results @?= 1
            case results of
                [(eid, cfg, _st)] -> do
                    cfg.tcToolName @?= "a"
                    eid @?= sessionToolCallIdToEntityId callId1
                _ -> assertBool "Expected exactly one result" False
        ]
  where
    -- Local helper mirroring the conversion in System.Agents.OS.Conversation.ToolCalls.
    sessionToolCallIdToEntityId :: Session.ToolCallId -> EntityId
    sessionToolCallIdToEntityId (Session.ToolCallId uuid) = EntityId uuid

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
            currentFrameType lineage @?= Just ConversationFrame
        , testCase "Lineage head and root" $ do
            now <- getCurrentTime
            let convId = EntityId $ read "550e8400-e29b-41d4-a716-446655440000"
            let turnId = EntityId $ read "550e8400-e29b-41d4-a716-446655440001"
            let lineage = pushLineage ConversationFrame convId now $ pushLineage TurnFrame turnId now emptyLineage
            case lineageHead lineage of
                Just frame -> frameType frame @?= ConversationFrame
                Nothing -> assertBool "Expected head" False
            case lineageRoot lineage of
                Just frame -> frameType frame @?= TurnFrame
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
                        , tcSessionId = Session.SessionId (read "550e8400-e29b-41d4-a716-446655440002")
                        , tcConversationId = Base.ConversationId (read "550e8400-e29b-41d4-a716-446655440003")
                        , tcProviderCallId = Nothing
                        , tcChildSessionId = Nothing
                        }
            let json = encode config
            let mDecoded = decode json
            assertEqual "Should decode to same value" (Just config) mDecoded
        , testCase "ToolCallStatus round-trip" $ do
            let result = toJSON ("output" :: Text)
            let statuses = [TcPending, TcExecuting, TcCompleted result, TcFailed "error", TcCancelled]
            mapM_ (\s -> let json = encode s in assertEqual ("Status: " ++ show s) (Just s) (decode json)) statuses
        , testCase "ToolCallState round-trip" $ do
            now <- getCurrentTime
            let result = toJSON ("output" :: Text)
            let progress = ToolCallProgress now (ProgressPartial result) result
            let state =
                    ToolCallState
                        { tcStatus = TcCompleted result
                        , tcStartedAt = Just now
                        , tcCompletedAt = Just now
                        , tcResult = Just result
                        , tcProgress = [progress]
                        }
            let json = encode state
            let mDecoded = decode json
            assertEqual "Should decode to same value" (Just state) mDecoded
        , testCase "ToolCallProgress round-trip" $ do
            now <- getCurrentTime
            let result = toJSON ("partial" :: Text)
            let progress = ToolCallProgress now (ProgressPartial result) result
            let json = encode progress
            let mDecoded = decode json
            assertEqual "Should decode to same value" (Just progress) mDecoded
        , testCase "ProgressKind round-trip" $ do
            let kinds =
                    [ ProgressStarted
                    , ProgressLog "hello"
                    , ProgressPartial (toJSON (42 :: Int))
                    , ProgressHeartbeat
                    ]
            mapM_ (\k -> let json = encode k in assertEqual ("Kind: " ++ show k) (Just k) (decode json)) kinds
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
            let pending = ToolCallState{tcStatus = TcPending, tcStartedAt = Nothing, tcCompletedAt = Nothing, tcResult = Nothing, tcProgress = []}
            let executing = ToolCallState{tcStatus = TcExecuting, tcStartedAt = Just now, tcCompletedAt = Nothing, tcResult = Nothing, tcProgress = []}
            let completed = ToolCallState{tcStatus = TcCompleted result, tcStartedAt = Just now, tcCompletedAt = Just now, tcResult = Just result, tcProgress = []}
            let failed = ToolCallState{tcStatus = TcFailed "error", tcStartedAt = Just now, tcCompletedAt = Just now, tcResult = Nothing, tcProgress = []}
            let cancelled = ToolCallState{tcStatus = TcCancelled, tcStartedAt = Just now, tcCompletedAt = Just now, tcResult = Nothing, tcProgress = []}
            assertBool "Pending should not be completed" (not $ isToolCallCompleted pending)
            assertBool "Executing should not be completed" (not $ isToolCallCompleted executing)
            assertBool "Completed should be completed" (isToolCallCompleted completed)
            assertBool "Failed should be completed" (isToolCallCompleted failed)
            assertBool "Cancelled should be completed" (isToolCallCompleted cancelled)
        , testCase "getToolCallResult" $ do
            now <- getCurrentTime
            let result = toJSON ("output" :: Text)
            let completed = ToolCallState{tcStatus = TcCompleted result, tcStartedAt = Just now, tcCompletedAt = Just now, tcResult = Just result, tcProgress = []}
            let failed = ToolCallState{tcStatus = TcFailed "error", tcStartedAt = Just now, tcCompletedAt = Just now, tcResult = Nothing, tcProgress = []}
            let pending = ToolCallState{tcStatus = TcPending, tcStartedAt = Nothing, tcCompletedAt = Nothing, tcResult = Nothing, tcProgress = []}
            getToolCallResult completed @?= Just result
            getToolCallResult failed @?= Nothing
            getToolCallResult pending @?= Nothing
        ]


-------------------------------------------------------------------------------
-- Session Step Entity Tests
-------------------------------------------------------------------------------

{- | Tests verifying that 'runStepMSync' and 'runStepMAsync' promote every
tool call to an OS ECS entity and keep its lifecycle state in sync.
-}
sessionStepEntityTests :: TestTree
sessionStepEntityTests =
    testGroup
        "Session Step creates OS entities"
        [ testCase "sync step creates completed tool-call entities" $ do
            world <- atomically newWorld
            world' <- atomically $ registerToolCallComponents world
            let agent = mkAgentWithWorld world' Session.Synchronous (\_ _ -> Session.RunSync)
            let calls = [mkStepCall "tool_a", mkStepCall "tool_b"]
            let session = mkStepSession calls
            (_agent, result) <- runStepMSync stepConvId agent session
            case result of
                Left _ -> assertFailure "expected a session, not a final result"
                Right _ -> do
                    entities <- atomically $ listToolCallsBySessionAndConversation world' stepSessionId stepConvId
                    length entities @?= 2
                    assertBool "all entities should be completed" $
                        all (\(_, _, st) -> isToolCallCompleted st) entities
                    assertBool "all entities should have a result" $
                        all (\(_, _, st) -> isJust $ getToolCallResult st) entities
        , testCase "async step creates entities for sync and deferred calls" $ do
            world <- atomically newWorld
            world' <- atomically $ registerToolCallComponents world
            let policy _ctx call
                    | stepCallName call == "sync_tool" = Session.RunSync
                    | otherwise = Session.Defer (Session.Reason "test deferral")
            let agent = mkAgentWithWorld world' Session.Asynchronous policy
            let calls = [mkStepCall "sync_tool", mkStepCall "defer_a", mkStepCall "defer_b"]
            let session = mkStepSession calls
            (_agent, result) <- runStepMAsync stepConvId agent session
            case result of
                Left _ -> assertFailure "expected a yielded session, not a final result"
                Right session' -> do
                    -- Three entities should exist in the world.
                    entities <- atomically $ listToolCallsBySessionAndConversation world' stepSessionId stepConvId
                    length entities @?= 3
                    -- The sync call should be completed.
                    let syncEntities =
                            [ (eid, st)
                            | (eid, cfg, st) <- entities
                            , cfg.tcToolName == "sync_tool"
                            ]
                    length syncEntities @?= 1
                    case syncEntities of
                        [(_, st)] -> st.tcStatus @?= TcCompleted (toJSON $ Session.TextResponse ("done:sync_tool" :: Text))
                        _ -> assertFailure "expected exactly one sync entity"
                    -- The deferred calls should be pending.
                    let deferredEntities =
                            [ st
                            | (_, cfg, st) <- entities
                            , cfg.tcToolName `elem` ["defer_a", "defer_b"]
                            ]
                    length deferredEntities @?= 2
                    assertBool "deferred entities should be pending" $
                        all (\st -> st.tcStatus == TcPending) deferredEntities
                    -- The session partial turn should record entity ids.
                    case session'.turns of
                        (Session.PartialUserTurn partial _ : _) -> do
                            assertBool "all tracked calls should have entity ids" $
                                all (isJust . Session.tcEntityId) partial.pTrackedToolCalls
                        _ -> assertFailure "expected a partial user turn"
        , testCase "async step completes all calls when policy is sync" $ do
            world <- atomically newWorld
            world' <- atomically $ registerToolCallComponents world
            let agent = mkAgentWithWorld world' Session.Asynchronous (\_ _ -> Session.RunSync)
            let calls = [mkStepCall "tool_a", mkStepCall "tool_b"]
            let session = mkStepSession calls
            (_agent, result) <- runStepMAsync stepConvId agent session
            case result of
                Left _ -> assertFailure "expected a session, not a final result"
                Right session' -> do
                    case session'.turns of
                        (Session.UserTurn _ _ : _) -> pure ()
                        _ -> assertFailure "expected a full user turn"
                    entities <- atomically $ listToolCallsBySessionAndConversation world' stepSessionId stepConvId
                    length entities @?= 2
                    assertBool "all entities should be completed" $
                        all (\(_, _, st) -> isToolCallCompleted st) entities
        ]

-- | Stable conversation id for session-step entity tests.
stepConvId :: Base.ConversationId
stepConvId = Base.ConversationId nil

-- | Stable session id for session-step entity tests.
stepSessionId :: Session.SessionId
stepSessionId = Session.SessionId nil

-- | Build a minimal LLM-issued tool call with a function name.
mkStepCall :: Text -> Session.LlmToolCall
mkStepCall name =
    Session.LlmToolCall $ toJSON $ ToolCall name (object [])

-- | Extract the function name from a tool call.
stepCallName :: Session.LlmToolCall -> Text
stepCallName (Session.LlmToolCall val) =
    case parseMaybe parseJSON val of
        Just (ToolCall name _) -> name
        Nothing ->
            case val of
                Object obj ->
                    case KeyMap.lookup "function" obj of
                        Just (Object func) ->
                            case KeyMap.lookup "name" func of
                                Just (String n) -> n
                                _ -> "unknown"
                        _ -> "unknown"
                _ -> "unknown"

-- | Build a session whose latest turn is an LLM turn with the given calls.
mkStepSession :: [Session.LlmToolCall] -> Session.Session
mkStepSession calls =
    Session.Session
        { Session.turns =
            [ Session.LlmTurn
                ( Session.LlmTurnContent
                    { Session.llmResponse = Session.LlmResponse Nothing Nothing Null Nothing
                    , Session.llmToolCalls = calls
                    }
                )
                Nothing
            ]
        , Session.sessionId = stepSessionId
        , Session.forkedFromSessionId = Nothing
        , Session.turnId = Session.TurnId nil
        , Session.sessionVersion = Just 2
        , Session.sessionExecutionMode = Just Session.Asynchronous
        }

-- | Dummy tool portal for session-step entity tests.
stepDummyPortal :: ToolPortal
stepDummyPortal _ _ =
    pure $
        ToolResult
            { resultData = object []
            , resultDuration = 0
            , resultTraceId = "dummy"
            }

-- | Build an agent backed by an OS world for session-step entity tests.
mkAgentWithWorld ::
    World ->
    Session.ExecutionMode ->
    (ToolExecutionContext -> Session.LlmToolCall -> Session.ToolCallDisposition) ->
    Agent (Session.LlmTurnContent, Session)
mkAgentWithWorld world mode policy =
    Agent
        { step = naiveTilNoToolCallStep
        , sysPrompt = pure $ Session.SystemPrompt "test prompt"
        , sysTools = pure []
        , usrQuery = pure Nothing
        , toolCall = \_ call -> pure $ Session.TextResponse ("done:" <> stepCallName call)
        , toolPortal = stepDummyPortal
        , complete = \_ -> pure (Session.LlmResponse Nothing Nothing Null Nothing, [])
        , contextConfig = defaultContextConfig
        , ctxWorld = Just world
        , ctxEmit = Nothing
        , ctxCallStack = []
        , ctxParentConversation = Nothing
        , ctxExecutionMode = mode
        , ctxAsyncYieldStrategy = YieldWhenAllDone
        , ctxMaxConcurrency = Nothing
        , ctxAsyncCallTimeout = Nothing
        , ctxToolCache = Nothing
        , ctxToolCallPolicy = policy
        , ctxToolExecutor = Nothing
        , ctxContinuationStore = Nothing
        , ctxDeploymentRunner = Nothing
        , ctxSessionBackend = Nothing
        , ctxAsyncEngine = Nothing
        , ctxParams = mempty
        , ctxInheritedBindings = []
        , ctxMailbox = Nothing
        , ctxMailRouter = Nothing
        , ctxSpawnSession = Nothing
        , ctxInterruptCompletions = False
        , ctxMailInToolResult = False
        }

-------------------------------------------------------------------------------
-- Async Engine Tests
-------------------------------------------------------------------------------

{- | Tests for the concurrent async execution engine.

These tests verify that 'RunAsync' calls execute concurrently, that
progress callbacks are persisted on the OS entity, and that cancellation
is race-safe against late engine completions.
-}
asyncEngineTests :: TestTree
asyncEngineTests =
    testGroup
        "Async Engine"
        [ testCase "two RunAsync calls execute concurrently" $ do
            world <- atomically newWorld
            world' <- atomically $ registerToolCallComponents world
            let delayMicros = 200000 -- 200 ms per call
            engine <- AsyncEngine.mkAsyncEngine world' (sleepExecutor delayMicros) 2 Nothing Nothing
            (tc1, eid1) <- mkAsyncTrackedCall world' "sleep_a"
            (tc2, eid2) <- mkAsyncTrackedCall world' "sleep_b"
            let baseCtx = mkMinimalContext asyncSessId asyncConvId asyncTurnId stepDummyPortal
            start <- getCurrentTime
            batch <- AsyncEngine.startAsyncBatch engine baseCtx [tc1, tc2]
            completed <- waitAllCompleted batch
            elapsed <- (`diffUTCTime` start) <$> getCurrentTime
            length completed @?= 2
            -- Sequential execution would take ~400ms; concurrent should be < 300ms.
            assertBool ("expected concurrent execution (< 0.3s), got " ++ show elapsed) $
                elapsed < 0.3
            -- Both entities should be completed.
            Just st1 <- atomically $ getComponent @ToolCallState world' eid1
            Just st2 <- atomically $ getComponent @ToolCallState world' eid2
            assertBool "entity 1 should be completed" (isToolCallCompleted st1)
            assertBool "entity 2 should be completed" (isToolCallCompleted st2)
        , testCase "progress callback emits ToolCallProgress entries" $ do
            world <- atomically newWorld
            world' <- atomically $ registerToolCallComponents world
            engine <- AsyncEngine.mkAsyncEngine world' progressExecutor 2 Nothing Nothing
            (tc, eid) <- mkAsyncTrackedCall world' "progress_tool"
            let baseCtx = mkMinimalContext asyncSessId asyncConvId asyncTurnId stepDummyPortal
            batch <- AsyncEngine.startAsyncBatch engine baseCtx [tc]
            _ <- waitAllCompleted batch
            Just st <- atomically $ getComponent @ToolCallState world' eid
            assertBool "should have progress entries" (not $ null $ tcProgress st)
            assertBool "should include a ProgressStarted entry" $
                any (\p -> progressKind p == ProgressStarted) (tcProgress st)
            assertBool "should include the custom partial progress entry" $
                any (\p -> progressKind p == ProgressPartial (object [("percent", toJSON (50 :: Int))])) (tcProgress st)
        , testCase "cancelToolCall marks entity cancelled and engine completion does not overwrite" $ do
            world <- atomically newWorld
            world' <- atomically $ registerToolCallComponents world
            engine <- AsyncEngine.mkAsyncEngine world' (sleepExecutor 500000) 2 Nothing Nothing
            (tcSlow, eidSlow) <- mkAsyncTrackedCall world' "slow_tool"
            (tcFast, eidFast) <- mkAsyncTrackedCall world' "fast_tool"
            let baseCtx = mkMinimalContext asyncSessId asyncConvId asyncTurnId stepDummyPortal
            batch <- AsyncEngine.startAsyncBatch engine baseCtx [tcSlow, tcFast]
            -- Cancel the slow call immediately.
            cancelled <- AsyncEngine.cancelToolCall engine (Session.tcId tcSlow)
            cancelled @?= True
            -- Wait for the remaining call to finish.
            _ <- waitAllCompleted batch
            -- The cancelled entity must remain cancelled.
            Just stSlow <- atomically $ getComponent @ToolCallState world' eidSlow
            stSlow.tcStatus @?= TcCancelled
            -- The other entity should be completed.
            Just stFast <- atomically $ getComponent @ToolCallState world' eidFast
            assertBool "fast entity should be completed" (isToolCallCompleted stFast)
            -- A second cancellation attempt on the already-final call returns False.
            cancelledAgain <- AsyncEngine.cancelToolCall engine (Session.tcId tcSlow)
            cancelledAgain @?= False
        ]

-- | Stable session id for async engine tests.
asyncSessId :: Session.SessionId
asyncSessId = Session.SessionId nil

-- | Stable conversation id for async engine tests.
asyncConvId :: Base.ConversationId
asyncConvId = Base.ConversationId nil

-- | Stable turn id for async engine tests.
asyncTurnId :: Session.TurnId
asyncTurnId = Session.TurnId nil

-- | Create a tracked async call with an associated OS entity.
mkAsyncTrackedCall :: World -> Text -> IO (Session.TrackedToolCall, EntityId)
mkAsyncTrackedCall world toolName = do
    callId <- Session.newToolCallId
    let call = Session.LlmToolCall $ toJSON $ ToolCall toolName (object [])
    eid <- createToolCallEntity world asyncSessId asyncConvId asyncTurnId Nothing toolName (object []) callId
    let tc =
            Session.TrackedToolCall
                { Session.tcId = callId
                , Session.tcCall = call
                , Session.tcState = Session.Running
                , Session.tcResult = Nothing
                , Session.tcContinuation = Nothing
                , Session.tcPolicy = Session.AppliedPolicy (Session.RunAsync Nothing) Nothing
                , Session.tcEntityId = Just eid
                , Session.tcDeliveredLate = False
                }
    pure (tc, eid)

-- | Executor that sleeps for the given number of microseconds then returns.
sleepExecutor :: Int -> ToolExecutionContext -> Session.LlmToolCall -> IO Session.UserToolResponse
sleepExecutor delayMicros _ctx _call = do
    threadDelay delayMicros
    pure $ Session.TextResponse "slept"

-- | Executor that emits a partial progress payload then returns.
progressExecutor :: ToolExecutionContext -> Session.LlmToolCall -> IO Session.UserToolResponse
progressExecutor ctx _call = do
    case ctxProgressCallback ctx of
        Just cb -> cb (object [("percent", toJSON (50 :: Int))])
        Nothing -> pure ()
    pure $ Session.TextResponse "progressed"

-- | Block until every call in the batch has reached a final state.
waitAllCompleted :: AsyncEngine.AsyncBatch -> IO [(Session.ToolCallId, Session.UserToolResponse)]
waitAllCompleted batch = go
  where
    go = do
        update <- AsyncEngine.waitForProgress batch
        if null (AsyncEngine.abuStillRunning update)
            then pure $ AsyncEngine.abuCompleted update
            else do
                more <- go
                pure $ AsyncEngine.abuCompleted update ++ more


-------------------------------------------------------------------------------
-- Tool-Call Status Introspection Tests
-------------------------------------------------------------------------------

{- | Tests for the system-toolbox async tool-call introspection capabilities.

These tests verify that agents can query, list, and cancel running async
 tool calls through the OS ECS world.
-}
toolCallStatusTests :: TestTree
toolCallStatusTests =
    testGroup
        "Tool-Call Status Introspection"
        [ testCase "get-tool-call-status returns pending then running" $ do
            world <- atomically newWorld
            world' <- atomically $ registerToolCallComponents world
            sessId <- Session.newSessionId
            convId <- Base.newConversationId
            turnId <- Session.newTurnId
            callId <- Session.newToolCallId
            let callIdText = toolCallIdToText callId
            eid <- createToolCallEntity world' sessId convId turnId Nothing "bash" (toJSON ("ls" :: Text)) callId
            let ctx = makeStatusCtx world' sessId convId turnId Nothing
            -- Pending state
            Right pending <- getToolCallStatus ctx $ defaultGetStatusParams callIdText
            tcsrStatus pending @?= "pending"
            tcsrIsFinal pending @?= False
            -- Running state
            startToolCall world' eid
            Right running <- getToolCallStatus ctx $ defaultGetStatusParams callIdText
            tcsrStatus running @?= "running"
            tcsrIsFinal running @?= False
            tcsrToolName running @?= Just "bash"
        , testCase "get-tool-call-status returns completed after finish" $ do
            world <- atomically newWorld
            world' <- atomically $ registerToolCallComponents world
            sessId <- Session.newSessionId
            convId <- Base.newConversationId
            turnId <- Session.newTurnId
            callId <- Session.newToolCallId
            let callIdText = toolCallIdToText callId
            eid <- createToolCallEntity world' sessId convId turnId Nothing "bash" (toJSON ("ls" :: Text)) callId
            let ctx = makeStatusCtx world' sessId convId turnId Nothing
            let result = object [("output", toJSON ("done" :: Text))]
            completeToolCall world' eid result
            Right completed <- getToolCallStatus ctx $ defaultGetStatusParams callIdText
            tcsrStatus completed @?= "completed"
            tcsrIsFinal completed @?= True
            tcsrResult completed @?= Just result
            tcsrToolName completed @?= Just "bash"
        , testCase "get-tool-call-status returns orphaned for tracked call without entity" $ do
            world <- atomically newWorld
            world' <- atomically $ registerToolCallComponents world
            sessId <- Session.newSessionId
            convId <- Base.newConversationId
            turnId <- Session.newTurnId
            callId <- Session.newToolCallId
            let callIdText = toolCallIdToText callId
            let tracked =
                    Session.TrackedToolCall
                        { Session.tcId = callId
                        , Session.tcCall = Session.LlmToolCall $ toJSON $ ToolCall "orphan_tool" (object [])
                        , Session.tcState = Session.Running
                        , Session.tcResult = Nothing
                        , Session.tcContinuation = Nothing
                        , Session.tcPolicy = Session.AppliedPolicy (Session.RunAsync Nothing) Nothing
                        , Session.tcEntityId = Nothing
                        , Session.tcDeliveredLate = False
                        }
            let session =
                    Session.Session
                        { Session.turns = [Session.PartialUserTurn (Session.PartialUserTurnContent (Session.SystemPrompt "test") [] Nothing [tracked] []) Nothing]
                        , Session.sessionId = sessId
                        , Session.forkedFromSessionId = Nothing
                        , Session.turnId = turnId
                        , Session.sessionVersion = Just 2
                        , Session.sessionExecutionMode = Just Session.Asynchronous
                        }
            let ctx = makeStatusCtx world' sessId convId turnId (Just session)
            Right orphaned <- getToolCallStatus ctx $ defaultGetStatusParams callIdText
            tcsrStatus orphaned @?= "orphaned"
            tcsrToolName orphaned @?= Just "orphan_tool"
            -- An orphaned call can never finish.
            tcsrIsFinal orphaned @?= True
        , testCase "list-running-tool-calls returns only non-final calls" $ do
            world <- atomically newWorld
            world' <- atomically $ registerToolCallComponents world
            sessId <- Session.newSessionId
            convId <- Base.newConversationId
            turnId <- Session.newTurnId
            callIdPending <- Session.newToolCallId
            callIdCompleted <- Session.newToolCallId
            _ <- createToolCallEntity world' sessId convId turnId Nothing "pending_tool" (object []) callIdPending
            eidCompleted <- createToolCallEntity world' sessId convId turnId Nothing "completed_tool" (object []) callIdCompleted
            completeToolCall world' eidCompleted (toJSON ("done" :: Text))
            let ctx = makeStatusCtx world' sessId convId turnId Nothing
            Right (ListRunningToolCallsResult running) <- listRunningToolCalls ctx
            length running @?= 1
            case running of
                [info] -> do
                    rtciToolCallId info @?= toolCallIdToText callIdPending
                    rtciStatus info @?= "pending"
                    rtciToolName info @?= "pending_tool"
                _ -> assertFailure "expected exactly one running call"
        , testCase "cancel-tool-call marks a running entity as cancelled" $ do
            world <- atomically newWorld
            world' <- atomically $ registerToolCallComponents world
            sessId <- Session.newSessionId
            convId <- Base.newConversationId
            turnId <- Session.newTurnId
            callId <- Session.newToolCallId
            let callIdText = toolCallIdToText callId
            eid <- createToolCallEntity world' sessId convId turnId Nothing "bash" (toJSON ("ls" :: Text)) callId
            startToolCall world' eid
            let ctx = makeStatusCtx world' sessId convId turnId Nothing
            Right cancelled <- cancelToolCallById ctx $ CancelToolCallParams callIdText Nothing
            ccrCancelled cancelled @?= True
            ccrPreviousStatus cancelled @?= "running"
            Right status <- getToolCallStatus ctx $ defaultGetStatusParams callIdText
            tcsrStatus status @?= "cancelled"
            tcsrIsFinal status @?= True
            -- A second cancellation attempt reports already final.
            Right notAgain <- cancelToolCallById ctx $ CancelToolCallParams callIdText Nothing
            ccrCancelled notAgain @?= False
            ccrPreviousStatus notAgain @?= "cancelled"
        ]

-- | Build a minimal execution context with an optional full session.
makeStatusCtx ::
    World ->
    Session.SessionId ->
    Base.ConversationId ->
    Session.TurnId ->
    Maybe Session.Session ->
    ToolExecutionContext
makeStatusCtx world sessId convId turnId mSession =
    (mkMinimalContext sessId convId turnId stepDummyPortal)
        { Ctx.ctxWorld = Just world
        , ctxFullSession = mSession
        }

-- | Convert a session-layer tool-call id to text.
toolCallIdToText :: Session.ToolCallId -> Text
toolCallIdToText (Session.ToolCallId uuid) = UUID.toText uuid

-- | Default parameters for a get-tool-call-status query.
defaultGetStatusParams :: Text -> GetToolCallStatusParams
defaultGetStatusParams callIdText =
    GetToolCallStatusParams
        { toolCallId = callIdText
        , includeProgress = True
        , waitForCompletion = False
        , timeoutSeconds = 5
        }
