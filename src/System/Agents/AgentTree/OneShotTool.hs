{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Control.Concurrent.STM (TQueue, atomically, newTVarIO, readTVarIO, writeTQueue)
import Control.Exception (SomeException, catch, displayException)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as CByteString
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (getCurrentTime)
import Prod.Tracer (Tracer (..), contramap)

import System.Agents.AgentTree (LoadedApiKeys, OSAgentNode (..))
import qualified System.Agents.Base as Base
import System.Agents.Combinators.ProgressiveDisclosure (agentEvaluateActiveTools)
import qualified System.Agents.HttpClient as HttpClient
import qualified System.Agents.LLMs.OpenAI as OpenAI
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
import System.Agents.OneShot (agentStoreSession, mapProgressiveDisclosureTrace, parseModelFlavor)
import qualified System.Agents.OneShot as OneShot
import System.Agents.Session.Base (
    Agent (..),
    LlmResponse (..),
    LlmTurnContent (..),
    Session (..),
    SystemPrompt (..),
    SystemTool (..),
    SystemToolDefinition (..),
    SystemToolDefinitionV1 (..),
    UserQuery (..),
    defaultContextConfig,
    newSessionId,
    newTurnId,
 )
import qualified System.Agents.Session.Compat as SessionCompat
import System.Agents.Session.Loop (run)
import System.Agents.Session.OpenAI (OpenAICompletionConfig (..), mkOpenAICompletion)
import System.Agents.Session.Step (naiveTilNoToolCallStep)
import System.Agents.SessionStore (SessionStore)
import qualified System.Agents.ToolPortal as ToolPortal
import System.Agents.ToolRegistration (
    ToolRegistration (..),
    registerIOScriptInLLM,
 )
import qualified System.Agents.ToolRegistration as ToolRegistration
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..), ToolDescription (..), ToolName (..))
import System.Agents.Tools.Context (
    ToolExecutionContext,
    ctxCallStack,
    ctxConversationId,
    ctxEventQueue,
    ctxParentConversation,
    ctxWorld,
 )
import System.Agents.Tools.ExecuteToolCall (executeLlmToolCall)
import qualified System.Agents.Tools.IO as IOTools

-------------------------------------------------------------------------------
-- Trace Types
-------------------------------------------------------------------------------

data Trace
    = OneShotTrace !OneShot.Trace
    | OpenAITrace !OpenAI.Trace
    | ToolPortalTrace !ToolPortal.Trace
    | ToolRegistrationTrace !ToolRegistration.Trace
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
3. Emits OSEvent_SubcallProgress during execution (if progress tracking enabled)
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
    -- | Optional session store for persisting sessions
    SessionStore ->
    -- | API keys for creating HTTP runtime
    LoadedApiKeys ->
    -- | The OS agent node to convert into a tool
    OSAgentNode ->
    -- | The slug of the calling agent (for tracing)
    Base.AgentSlug ->
    -- | The ID of the calling agent (for tracing)
    Base.AgentId ->
    -- | The resulting tool registration
    ToolRegistration
turnAgentRuntimeIntoIOTool tracer store apiKeys node callerSlug callerId =
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

        -- Get the API key for this agent
        let apiKeyId = Base.apiKeyId agent
        let mApiKey = lookupApiKey apiKeyId apiKeys

        -- Create HTTP runtime with the API key
        httpRuntime <- case mApiKey of
            Just apiKey -> HttpClient.newRuntime (HttpClient.BearerToken $ Text.decodeUtf8 $ OpenAI.revealApiKey apiKey)
            Nothing -> HttpClient.newRuntime HttpClient.NoToken

        -- Create the agent from the OS node
        -- nodeToAgent expects Base.AgentId and returns Agent using Base.ConversationId
        sessionAgent0 <- nodeToAgent store httpRuntime node tracer callerSlug callerId

        -- Apply dynamic tool filtering based on session activation state
        -- This allows tools to be enabled/disabled via meta_activate_tool/meta_deactivate_tool
        sessionAgent <- agentEvaluateActiveTools (contramap (OneShotTrace . mapProgressiveDisclosureTrace) tracer) (osNodeTools node) sessionAgent0

        -- Set the query on the agent
        let agentWithQuery = agentSetQuery (UserQuery query []) sessionAgent

        -- Create a fresh session with media support (version 1)
        session0 <- Session [] <$> newSessionId <*> pure Nothing <*> newTurnId <*> pure (Just 1)

        -- Generate a conversation ID for this execution (Base.ConversationId)
        subcallBaseConvId <- newBaseConversationId

        -- Get current time for timestamps
        now <- getCurrentTime

        -- Extract OS integration fields from context
        let mWorld = ctx.ctxWorld
        let mEventQueue = ctx.ctxEventQueue
        let mParentBaseConv = ctx.ctxParentConversation

        -- Calculate subcall depth for lineage
        let depth = calculateSubcallDepth ctx

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

        -- Return the result
        pure $ Text.encodeUtf8 result

-- | Calculate the subcall depth from the context.
calculateSubcallDepth :: ToolExecutionContext -> Int
calculateSubcallDepth ctx = case ctx.ctxParentConversation of
    Nothing -> 0
    Just _ -> max 0 (length (ctxCallStack ctx) - 1)

{- | Run the sub-agent with event emission for TUI visibility.
Uses Base.ConversationId throughout, converting to OS types only for World operations.
-}
runSubAgentWithEventEmission ::
    Base.ConversationId ->
    Session ->
    Agent (LlmTurnContent, Session) ->
    Maybe World ->
    Maybe (TQueue OSEvent) ->
    IO Text
runSubAgentWithEventEmission baseConvId session0 agent mWorld mEventQueue = do
    -- Run the agent and handle exceptions
    -- Session.run uses Base.ConversationId directly
    result <-
        catch
            ( do
                (finalTurnContent, _finalSession) <- run baseConvId agent session0
                -- Extract and return the response text
                let resultText = extractResponseText finalTurnContent.llmResponse
                pure $ Right resultText
            )
            ( \e -> do
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

-- | Look up an API key by its ID from the loaded API keys.
lookupApiKey :: Text -> LoadedApiKeys -> Maybe OpenAI.ApiKey
lookupApiKey keyId keys = fmap snd $ listToMaybe $ filter ((== keyId) . fst) keys

-------------------------------------------------------------------------------

{- | Creates an Agent from an OSAgentNode configured for use as a tool.
Uses Base types throughout since Session subsystem uses Base.ConversationId.
-}
nodeToAgent ::
    SessionStore ->
    -- | HTTP runtime for making LLM requests
    HttpClient.Runtime ->
    OSAgentNode ->
    Tracer IO Trace ->
    Base.AgentSlug ->
    Base.AgentId ->
    IO (Agent (LlmTurnContent, Session))
nodeToAgent store httpRuntime node tracer _callerSlug _callerId = do
    let agentCfg = node.osNodeConfig
    let sPrompt = SystemPrompt $ Text.unlines $ Base.systemPrompt agentCfg

    -- Read tools from the OS-native TVar
    toolRegs <- readTVarIO (osNodeTools node)
    let sTools = map toolRegistrationToSystemTool toolRegs

    -- Create completion config and function
    let completionConfig =
            OpenAICompletionConfig
                { cfgTracer = contramap OpenAITrace tracer
                , cfgRuntime = httpRuntime
                , cfgBaseUrl = OpenAI.ApiBaseUrl $ Base.modelUrl agentCfg
                , cfgModelName = Base.modelName agentCfg
                , cfgModelFlavor = parseModelFlavor $ Base.flavor agentCfg
                }
    let completeF = mkOpenAICompletion completionConfig

    let tp = ToolPortal.makeToolPortal (contramap ToolPortalTrace tracer) (osNodeTools node)

    -- Generate a new Base.ConversationId for this agent instance
    -- Session subsystem uses Base.ConversationId
    convId <- newBaseConversationId

    pure $
        agentStoreSession store Nothing convId $
            Agent
                { step = naiveTilNoToolCallStep
                , sysPrompt = pure sPrompt
                , sysTools = pure sTools
                , usrQuery = pure Nothing
                , toolCall = executeLlmToolCall (contramap ToolRegistrationTrace tracer) (readTVarIO $ osNodeTools node) (SessionCompat.parseToolCallFromLlmToolCall, SessionCompat.callResultToUserToolResponse)
                , toolPortal = tp
                , complete = completeF
                , contextConfig = defaultContextConfig
                }

-------------------------------------------------------------------------------

-- | Convert a ToolRegistration to a SystemTool for the Session agent.
toolRegistrationToSystemTool :: ToolRegistration -> SystemTool
toolRegistrationToSystemTool reg =
    let llmTool = reg.declareTool
        toolDefv1 =
            SystemToolDefinitionV1
                { name = llmTool.toolDescriptionName.getToolName
                , llmName = llmTool.toolDescriptionName.getToolName
                , description = llmTool.toolDescriptionText
                , properties = llmTool.toolDescriptionParamProperties
                , raw =
                    Aeson.object
                        [ "type" .= ("function" :: Text)
                        , "function"
                            .= Aeson.object
                                [ "name" .= llmTool.toolDescriptionName.getToolName
                                , "description" .= llmTool.toolDescriptionText
                                , "parameters" .= toolParamsToJson llmTool.toolDescriptionParamProperties
                                ]
                        ]
                }
     in SystemTool $ V1 toolDefv1

-- | Convert tool parameters to JSON schema.
toolParamsToJson :: [ParamProperty] -> Aeson.Value
toolParamsToJson props =
    Aeson.object
        [ "type" .= ("object" :: Text)
        , "properties" .= KeyMap.fromList (map paramPropertyToJson props)
        , "required" .= map propertyKey (filter propertyRequired props)
        , "additionalProperties" .= False
        ]
  where
    paramPropertyToJson :: ParamProperty -> (Aeson.Key, Aeson.Value)
    paramPropertyToJson p = (AesonKey.fromText p.propertyKey, paramTypeToJson p)

    paramTypeToJson :: ParamProperty -> Aeson.Value
    paramTypeToJson p =
        Aeson.object $
            [ "type" .= paramTypeToString p.propertyType
            , "description" .= p.propertyDescription
            ]
                ++ case p.propertyType of
                    EnumParamType values -> ["enum" .= values]
                    _ -> []

    paramTypeToString :: ParamType -> Text
    paramTypeToString NullParamType = "null"
    paramTypeToString StringParamType = "string"
    paramTypeToString BoolParamType = "boolean"
    paramTypeToString NumberParamType = "number"
    paramTypeToString (EnumParamType _) = "string"
    paramTypeToString (OpaqueParamType t) = t
    paramTypeToString (MultipleParamType t) = t
    paramTypeToString (ObjectParamType _) = "object"

-------------------------------------------------------------------------------

-- | Set the user query on an agent.
agentSetQuery :: UserQuery -> Agent r -> Agent r
agentSetQuery query agent =
    agent{usrQuery = pure (Just query)}

-- | Extract text content from an LLM response.
extractResponseText :: LlmResponse -> Text
extractResponseText (LlmResponse txt _thinking _ _) =
    Maybe.fromMaybe "" txt
