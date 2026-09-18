{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Provides a version of turnAgentRuntimeIntoIOTool based on OneShot.hs
implementation of LLM session calls.

This module has been updated to work with OS-native structures and includes
TUI visibility support for subcall conversations.

Type conversion strategy:
- Base.ConversationId is used throughout most logic (from System.Agents.Base)
- OS.Core.Types.ConversationId/EntityId are only used when interacting with OS World
- Conversion: Base.ConversationId (UUID) -> EntityId (UUID) -> OS ConversationId
-}
module System.Agents.AgentTree.OneShotTool (
    Trace (..),
    turnAgentRuntimeIntoIOTool,
) where

import Control.Concurrent.STM (TQueue, atomically, newTVarIO, writeTQueue)
import Control.Exception (SomeAsyncException, SomeException, catch, displayException, fromException, throwIO)
import Control.Monad (forM_)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as CByteString
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (getCurrentTime)
import Prod.Tracer (Tracer (..), contramap)

import System.Agents.AgentFactory (AgentDeps, AgentRole (..), buildAgent)
import qualified System.Agents.AgentFactory as AgentFactory
import System.Agents.AgentTree (OSAgentNode (..))
import qualified System.Agents.Base as Base
import System.Agents.OS.Conversation (
    ConversationConfig (..),
    ConversationState (..),
    ConversationStatus (..),
    Lineage (..),
    emptyLineage,
    pushLineage,
 )
import qualified System.Agents.OS.Conversation as OSConv
import System.Agents.OS.Core.Types (
    AgentId (..),
    ConversationId (..),
    EntityId (..),
 )
import System.Agents.OS.Core.World (World, setComponent)
import qualified System.Agents.OS.Core.World as OSWorld
import System.Agents.OS.Events (OSEvent (..))
import System.Agents.Session.Base (
    Agent (..),
    LlmResponse (..),
    LlmTurnContent (..),
    PartialUserTurnContent (..),
    Session (..),
    Turn (..),
    UserQuery (..),
    newSessionId,
    newTurnId,
 )
import qualified System.Agents.Session.Base as SessionBase
import System.Agents.Session.Loop (run)
import System.Agents.ToolRegistration (
    ToolRegistration,
    registerIOScriptInLLM,
 )
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..))

-- Import ToolExecutionContext with qualified access to avoid ambiguity with Agent fields.
-- DuplicateRecordFields allows both Agent and ToolExecutionContext to have the same field names.
import System.Agents.Tools.Context (CallStackEntry (..), ToolExecutionContext (..))
import qualified System.Agents.Tools.Context as Ctx
import qualified System.Agents.Tools.IO as IOTools

-------------------------------------------------------------------------------
-- Trace Types
-------------------------------------------------------------------------------

newtype Trace
    = OneShotTrace AgentFactory.Trace
    deriving (Show)

-------------------------------------------------------------------------------

-- | Data type for the prompt argument to the sub-agent.
newtype PromptOtherAgent = PromptOtherAgent
    { what :: Text
    }
    deriving (Show)

instance Aeson.FromJSON PromptOtherAgent where
    parseJSON = Aeson.withObject "PromptOtherAgent" $ \v ->
        PromptOtherAgent <$> v Aeson..: "what"

-------------------------------------------------------------------------------
-- Type Conversions
-------------------------------------------------------------------------------

{- | Convert a Base.AgentId to an OS.Core.Types.AgentId
Base.AgentId wraps UUID, OS.AgentId wraps EntityId which wraps UUID
-}
baseAgentIdToOS :: Base.AgentId -> AgentId
baseAgentIdToOS (Base.AgentId uuid) = AgentId (EntityId uuid)

{- | Convert a Base.ConversationId to an OS.Core.Types.ConversationId
Base.ConversationId wraps UUID, OS.ConversationId wraps EntityId which wraps UUID
-}
baseConversationIdToOS :: Base.ConversationId -> ConversationId
baseConversationIdToOS (Base.ConversationId uuid) = ConversationId (EntityId uuid)

-- | Convert an OS.Core.Types.ConversationId to an EntityId (for World operations)
osConversationIdToEntityId :: ConversationId -> EntityId
osConversationIdToEntityId = unConversationId

-- | Convert a Base.ConversationId directly to an EntityId
baseConversationIdToEntityId :: Base.ConversationId -> EntityId
baseConversationIdToEntityId (Base.ConversationId uuid) = EntityId uuid

-- | Create a new Base.ConversationId
newBaseConversationId :: IO Base.ConversationId
newBaseConversationId = Base.newConversationId

-------------------------------------------------------------------------------

{- | Converts an OSAgentNode into an IO Tool using the OneShot session-based approach.

This version uses the LLM session calls from OneShot.hs. It creates an Agent from
the OSAgentNode, runs it with a session, and returns the result.

When the ToolExecutionContext includes a World and EventQueue, this function:
1. Inserts the subcall conversation into the OS World as a first-class entity
2. Emits OSEvent_SubcallStarted at the beginning
3. Emits OSEvent_SubcallProgress during execution (after each step)
4. Emits OSEvent_SubcallCompleted or OSEvent_SubcallFailed at the end

This enables TUI visibility for subcall conversations, showing parent/child
relationships and tracking subcall lifecycle.

Type handling:
- ctx.ctxConversationId is Base.ConversationId (from Tools.Context)
- OSEvent types use Base.ConversationId (from OS.Events)
- OS World operations use OS.Core.Types.ConversationId/EntityId
- Session.run uses Base.ConversationId
-}
turnAgentRuntimeIntoIOTool ::
    Tracer IO Trace ->
    -- | Dependencies shared with the calling agent (API keys, session storage)
    AgentDeps ->
    -- | The OS agent node to convert into a tool
    OSAgentNode ->
    -- | The slug of the calling agent (for tracing)
    Base.AgentSlug ->
    -- | The ID of the calling agent (for tracing)
    Base.AgentId ->
    -- | The resulting tool registration
    ToolRegistration
turnAgentRuntimeIntoIOTool tracer deps node callerSlug _callerId =
    registerIOScriptInLLM io props
  where
    agent = node.osNodeConfig

    -- Define the parameter properties for the LLM tool schema
    props =
        [ ParamProperty
            { propertyKey = "what"
            , propertyType = StringParamType
            , propertyDescription = "the prompt to call the specialized-agent with"
            , propertyRequired = True
            }
        ]

    -- Create the IO script that wraps the agent
    io =
        IOTools.IOScript
            ( IOTools.IOScriptDescription
                ("prompt_agent_" <> Base.slug agent)
                ("asks a prompt to the expert agent: " <> Base.slug agent)
            )
            runSubAgent

    -- Run the sub-agent with the given prompt and execution context
    runSubAgent :: ToolExecutionContext -> PromptOtherAgent -> IO CByteString.ByteString
    runSubAgent ctx (PromptOtherAgent query) = do
        -- Extract the conversation ID from the execution context for tracing
        -- ctx.ctxConversationId is Base.ConversationId
        let parentBaseConvId = ctx.ctxConversationId

        -- Calculate new call stack from parent context for arbitrarily deep nesting
        -- Use qualified access to avoid ambiguity with Agent fields
        let parentCallStack = Ctx.ctxCallStack ctx
        subcallBaseConvId <- newBaseConversationId
        let newEntry = CallStackEntry (Base.slug agent) subcallBaseConvId (length parentCallStack)
        let subcallCallStack = newEntry : parentCallStack

        -- Extract OS integration fields from context
        let mWorld = Ctx.ctxWorld ctx
        let mEventQueue = Ctx.ctxEventQueue ctx
        let mParentBaseConv = Ctx.ctxParentConversation ctx

        -- Build the sub-agent with the caller's dependencies, then attach the
        -- OS integration fields: the World and EventQueue are essential for
        -- nested subcalls to be visible in the TUI
        sessionAgent0 <-
            buildAgent
                (contramap OneShotTrace tracer)
                deps
                (SubAgent parentBaseConvId subcallCallStack)
                subcallBaseConvId
                node
        let sessionAgent =
                sessionAgent0
                    { SessionBase.ctxWorld = mWorld
                    , SessionBase.ctxEventQueue = mEventQueue
                    }

        -- Set the query on the agent
        let agentWithQuery = agentSetQuery (UserQuery query []) sessionAgent

        -- Create a fresh session with media support (version 1)
        session0 <- Session [] <$> newSessionId <*> pure Nothing <*> newTurnId <*> pure (Just 1) <*> pure Nothing

        -- Get current time for timestamps
        now <- getCurrentTime

        let depth = length parentCallStack

        -- Insert into OS World and emit start event if OS integration is available
        -- Convert Base.ConversationId to OS types for World operations
        let subcallOSConvId = baseConversationIdToOS subcallBaseConvId
        let subcallEntityId = osConversationIdToEntityId subcallOSConvId

        -- Insert conversation into OS World if world is available
        case mWorld of
            Just world -> do
                -- Insert ConversationConfig using OS ConversationId
                let convConfig =
                        ConversationConfig
                            { conversationTitle = Just $ "↳ " <> Base.slug agent
                            , conversationMetadata =
                                Map.fromList
                                    [ ("agentSlug", Aeson.String $ Base.slug agent)
                                    , ("callerSlug", Aeson.String callerSlug)
                                    , ("isSubcall", Aeson.Bool True)
                                    ]
                            }

                -- Create TVar for last activity
                lastActivityVar <- newTVarIO now

                -- Convert Base.AgentId to OS.AgentId for conversation state
                let osAgentId = baseAgentIdToOS node.osNodeAgentId

                -- Insert ConversationState using OS AgentId
                let convState =
                        ConversationState
                            { conversationAgentId = osAgentId
                            , conversationStatus = ConversationActive
                            , conversationStartedAt = now
                            , conversationLastActivity = lastActivityVar
                            }

                -- Build lineage with parent link if available
                -- Convert parent Base.ConversationId to EntityId for lineage
                lineage <- case mParentBaseConv of
                    Just parentConvId -> do
                        let parentEntityId = baseConversationIdToEntityId parentConvId
                        let baseLineage = pushLineage OSConv.ConversationFrame parentEntityId now emptyLineage
                        pure $ pushLineage OSConv.ConversationFrame subcallEntityId now baseLineage
                    Nothing -> pure $ pushLineage OSConv.ConversationFrame subcallEntityId now emptyLineage

                -- Perform all insertions atomically
                atomically $ do
                    setComponent world subcallEntityId convConfig
                    setComponent world subcallEntityId convState
                    setComponent world subcallEntityId (Lineage $ OSConv.lineageStack lineage)
            Nothing -> pure ()

        -- Emit SubcallStarted event if event queue is available
        -- OSEvent uses Base.ConversationId directly
        case mEventQueue of
            Just eventQueue -> do
                let event =
                        OSEvent_SubcallStarted
                            { subcallParentConversationId = parentBaseConvId
                            , subcallConversationId = subcallBaseConvId
                            , subcallAgentSlug = Base.slug agent
                            , subcallDepth = depth
                            }
                atomically $ writeTQueue eventQueue event
            Nothing -> pure ()

        -- Run the agent and handle result
        -- Session.run uses Base.ConversationId
        result <-
            runSubAgentWithEventEmission
                subcallBaseConvId
                session0
                agentWithQuery
                mWorld
                mEventQueue
                (Ctx.ctxProgressCallback ctx)

        -- Return the result
        pure $ Text.encodeUtf8 result

{- | Run the sub-agent with event emission for TUI visibility.

This function wraps the agent's step function to emit progress events after each
step, enabling the TUI to show the conversation as it loads. Without this, the TUI
would only see the start and end events, not the intermediate progress.

Uses Base.ConversationId throughout, converting to OS types only for World operations.
-}
runSubAgentWithEventEmission ::
    Base.ConversationId ->
    Session ->
    Agent (LlmTurnContent, Session) ->
    Maybe World ->
    Maybe (TQueue OSEvent) ->
    -- | Progress callback of the calling tool call, if it runs in the background
    Maybe (Aeson.Value -> IO ()) ->
    IO Text
runSubAgentWithEventEmission baseConvId session0 agent mWorld mEventQueue mReportProgress = do
    -- Create a progress emitter function that sends SubcallProgress events
    let emitProgress sess = do
            forM_ mReportProgress ($ subAgentProgress sess)
            case mEventQueue of
                Just eventQueue -> do
                    let event =
                            OSEvent_SubcallProgress
                                { subcallProgressConversationId = baseConvId
                                , subcallProgressSession = sess
                                }
                    atomically $ writeTQueue eventQueue event
                Nothing -> pure ()

    -- Wrap the agent's step function to emit progress after each step
    let agentWithProgress = wrapAgentWithProgress emitProgress agent

    result <-
        catch
            ( do
                (finalTurnContent, finalSession) <- run baseConvId agentWithProgress session0
                -- Emit final progress with the completed session
                emitProgress finalSession
                -- Extract and return the response text
                let resultText = extractResponseText finalTurnContent.llmResponse
                pure $ Right resultText
            )
            ( \e -> do
                -- Cancellation (e.g. cancel-tool-call) must stop the sub-agent,
                -- not be reported as a failure.
                case fromException e of
                    Just (_ :: SomeAsyncException) -> throwIO e
                    Nothing -> pure ()
                let errMsg = Text.pack $ displayException (e :: SomeException)
                pure $ Left errMsg
            )

    -- Emit completion or failure event
    -- OSEvent types use Base.ConversationId
    case mEventQueue of
        Just eventQueue -> do
            case result of
                Right resultText -> do
                    let event =
                            OSEvent_SubcallCompleted
                                { subcallCompletedConversationId = baseConvId
                                , subcallCompletedResult = resultText
                                }
                    atomically $ writeTQueue eventQueue event
                    -- Update OS World status if available
                    -- Convert Base.ConversationId to OS types for World update
                    case mWorld of
                        Just world -> do
                            let osConvId = baseConversationIdToOS baseConvId
                            updateConversationStatus world osConvId ConversationArchived
                        Nothing -> pure ()
                Left errMsg -> do
                    let event =
                            OSEvent_SubcallFailed
                                { subcallFailedConversationId = baseConvId
                                , subcallFailedError = errMsg
                                }
                    atomically $ writeTQueue eventQueue event
                    -- Update OS World status if available
                    -- Convert Base.ConversationId to OS types for World update
                    case mWorld of
                        Just world -> do
                            let osConvId = baseConversationIdToOS baseConvId
                            updateConversationStatus world osConvId (ConversationError errMsg)
                        Nothing -> pure ()
        Nothing -> pure ()

    -- Return result or re-throw error
    case result of
        Right resultText -> pure resultText
        Left errMsg -> error $ Text.unpack errMsg

{- | Progress payload describing where a sub-agent is, reported to the
calling tool call.
-}
subAgentProgress :: Session -> Aeson.Value
subAgentProgress sess =
    Aeson.object
        [ "message" .= describe sess.turns
        , "turns" .= length sess.turns
        ]
  where
    describe :: [Turn] -> Text
    describe = \case
        [] -> "sub-agent starting"
        (LlmTurn llm _ : _)
            | null llm.llmToolCalls -> "sub-agent answered"
            | otherwise -> "sub-agent calling " <> Text.intercalate ", " (map SessionBase.llmToolCallName llm.llmToolCalls)
        (UserTurn _ _ : _) -> "sub-agent waiting for its LLM"
        (PartialUserTurn partial _ : _) ->
            "sub-agent has " <> Text.pack (show (length partial.pTrackedToolCalls)) <> " tool calls in progress"

{- | Wrap an agent's step function to emit progress events after each step.

This allows the TUI to track the conversation as it progresses, showing
intermediate states rather than just the final result.
-}
wrapAgentWithProgress ::
    (Session -> IO ()) ->
    Agent (LlmTurnContent, Session) ->
    Agent (LlmTurnContent, Session)
wrapAgentWithProgress emitProgress agent =
    agent
        { step = \sess -> do
            -- Emit progress before executing the step
            emitProgress sess
            -- Execute the original step
            result <- agent.step sess
            pure result
        }

{- | Update conversation status in OS World.
Takes OS.Core.Types.ConversationId since this is a World operation.
-}
updateConversationStatus :: World -> ConversationId -> ConversationStatus -> IO ()
updateConversationStatus world osConvId newStatus = do
    now <- getCurrentTime
    let entityId = unConversationId osConvId
    mState <- atomically $ OSWorld.getComponent @ConversationState world entityId
    case mState of
        Just state -> do
            -- Update last activity
            newLastActivity <- newTVarIO now
            atomically $ do
                let updatedState =
                        state
                            { conversationStatus = newStatus
                            , conversationLastActivity = newLastActivity
                            }
                setComponent world entityId updatedState
        Nothing -> pure ()

-- | Set the user query on an agent.
agentSetQuery :: UserQuery -> Agent r -> Agent r
agentSetQuery query agent =
    agent{usrQuery = pure (Just query)}

extractResponseText :: LlmResponse -> Text
extractResponseText (LlmResponse txt _thinking _ _) =
    fromMaybe "" txt
