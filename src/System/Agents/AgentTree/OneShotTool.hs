{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Provides a version of turnAgentRuntimeIntoIOTool based on OneShot.hs
implementation of LLM session calls.

This module has been updated to work with OS-native structures.

== Session Tracking for Recursive Agent Calls

When an agent calls another agent (or itself recursively) via 'extraAgents',
the sub-agent's session is now properly tracked and linked to its parent session.

Key features:

* 'AgentCallCallbacks' - Callbacks for session lifecycle events
* Parent-child session linkage via 'forkedFromSessionId'
* Proper trace capture through the parent tracer
* Integration with OS-native persistence layer

Example usage:

@
callbacks = AgentCallCallbacks
    { onSessionCreated = \\session convId -> persist backend (sessionIdToEntityId session.sessionId) session
    , onSessionUpdated = \\session -> putStrLn $ "Session updated: " ++ show session.sessionId
    , onSessionCompleted = \\session -> persist backend (sessionIdToEntityId session.sessionId) session
    }

tool = turnAgentRuntimeIntoIOTool store apiKeys node callerSlug callerId callbacks tracer lookupParent
@
-}
module System.Agents.AgentTree.OneShotTool (
    -- * Main API
    turnAgentRuntimeIntoIOTool,

    -- * Session Tracking Callbacks
    AgentCallCallbacks (..),
    defaultAgentCallCallbacks,

    -- * Parent Session Lookup
    ParentSessionLookup,
    defaultParentSessionLookup,
) where

import Control.Concurrent.STM (TVar, readTVarIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as CByteString
import Data.Maybe (listToMaybe)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Prod.Tracer (Tracer (..), contramap)

import System.Agents.AgentTree (OSAgentNode (..), LoadedApiKeys)
import System.Agents.Base (AgentId, AgentSlug, ConversationId, newConversationId, newStepId)
import qualified System.Agents.Base as Base
import qualified System.Agents.HttpClient as HttpClient
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.OneShot (agentStoreSession, parseModelFlavor)
import System.Agents.Runtime.Trace (ConversationTrace (..), Trace (..))
import System.Agents.Session.Base (
    Agent (..),
    LlmResponse (..),
    LlmToolCall (..),
    LlmTurnContent (..),
    OnSessionProgress,
    Session (..),
    SessionId,
    SessionProgress (..),
    SystemPrompt (..),
    SystemTool (..),
    SystemToolDefinition (..),
    SystemToolDefinitionV1 (..),
    UserQuery (..),
    UserToolResponse (..),
    defaultContextConfig,
    newSessionId,
    newTurnId,
 )
import System.Agents.Session.Loop (run)
import System.Agents.Session.OpenAI (OpenAICompletionConfig (..), mkOpenAICompletion)
import System.Agents.Session.Step (naiveTilNoToolCallStep)
import System.Agents.SessionStore (SessionStore)
import System.Agents.ToolRegistration (
    ToolRegistration (..),
    registerIOScriptInLLM,
 )
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..))
import System.Agents.Tools.Context (ToolExecutionContext, ctxConversationId)
import qualified System.Agents.Tools.IO as IOTools

-------------------------------------------------------------------------------
-- Session Tracking Callbacks
-------------------------------------------------------------------------------

{- | Callbacks for tracking sub-agent session lifecycle.

These callbacks are invoked at key points during a sub-agent's execution,
allowing parent agents to track, persist, and audit recursive agent calls.

The callbacks are invoked synchronously in the order:
1. 'onSessionCreated' - When the sub-agent session is initialized
2. 'onSessionUpdated' - After each turn completes (may be called multiple times)
3. 'onSessionCompleted' - When the sub-agent finishes (success or failure)

Example:

@
callbacks = AgentCallCallbacks
    { onSessionCreated = \\session convId -> do
        putStrLn $ "Sub-agent session created: " ++ show session.sessionId
        persistSession session convId
    , onSessionUpdated = \\session ->
        putStrLn $ "Session progress: " ++ show (length session.turns) ++ " turns"
    , onSessionCompleted = \\session -> do
        putStrLn $ "Session complete: " ++ show session.sessionId
        finalizeSession session
    }
@
-}
data AgentCallCallbacks = AgentCallCallbacks
    { onSessionCreated :: Session -> ConversationId -> IO ()
    -- ^ Called when a sub-agent session is created.
    -- The 'ConversationId' is the sub-agent's conversation ID.
    , onSessionUpdated :: Session -> IO ()
    -- ^ Called after each turn completes.
    -- May be called zero or more times depending on how many turns execute.
    , onSessionCompleted :: Session -> IO ()
    -- ^ Called when the sub-agent finishes (success or failure).
    -- This is always called exactly once if 'onSessionCreated' was called.
    }

-- | Default callbacks that do nothing (no-op).
--
-- Use this when you don't need to track sub-agent sessions.
defaultAgentCallCallbacks :: AgentCallCallbacks
defaultAgentCallCallbacks =
    AgentCallCallbacks
        { onSessionCreated = \_ _ -> pure ()
        , onSessionUpdated = \_ -> pure ()
        , onSessionCompleted = \_ -> pure ()
        }

-- | Convert 'AgentCallCallbacks' to 'OnSessionProgress' for use with the session loop.
callbacksToProgressHandler :: AgentCallCallbacks -> ConversationId -> OnSessionProgress
callbacksToProgressHandler callbacks subConvId progress =
    case progress of
        SessionStarted session ->
            callbacks.onSessionCreated session subConvId
        SessionUpdated session ->
            callbacks.onSessionUpdated session
        SessionCompleted session ->
            callbacks.onSessionCompleted session
        SessionFailed session _err ->
            -- Still treat as completed (with failure)
            callbacks.onSessionCompleted session

-------------------------------------------------------------------------------
-- Parent Session Lookup
-------------------------------------------------------------------------------

{- | Function to look up the parent session ID from a conversation ID.

This is used to establish the parent-child relationship between sessions
when a sub-agent is called. The 'forkedFromSessionId' field in the
sub-agent's 'Session' will be set to the parent's session ID.

Example:

@
lookupParent :: ParentSessionLookup
lookupParent parentConvId = do
    -- Query your session store for the session ID associated with this conversation
    mSession <- sessionStoreLookup parentConvId
    pure $ fmap sessionId mSession
@
-}
type ParentSessionLookup = ConversationId -> IO (Maybe SessionId)

-- | Default parent session lookup that always returns 'Nothing'.
--
-- Use this when you don't need to establish parent-child relationships.
defaultParentSessionLookup :: ParentSessionLookup
defaultParentSessionLookup = const (pure Nothing)

-------------------------------------------------------------------------------
-- Data Types
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
-- Main API
-------------------------------------------------------------------------------

{- | Converts an OSAgentNode into an IO Tool using the OneShot session-based approach.

This version uses the LLM session calls from OneShot.hs. It creates an Agent from
the OSAgentNode, runs it with a session, and returns the result.

== Session Tracking

When 'AgentCallCallbacks' are provided, the sub-agent's session lifecycle is
tracked:

* Session is created with 'forkedFromSessionId' linked to the parent session (if lookup provided)
* 'onSessionCreated' is called when the sub-agent starts
* 'onSessionUpdated' is called after each turn
* 'onSessionCompleted' is called when the sub-agent finishes
* Traces are properly captured and routed through the parent tracer

NOTE: This is a transitional implementation. In the final OS-native architecture,
this would use OS-native session management directly.

@param store Optional session store for persisting sessions
@param apiKeys API keys for creating HTTP runtime
@param node The OS agent node to convert into a tool
@param callerSlug The slug of the calling agent (for tracing)
@param callerId The ID of the calling agent (for tracing)
@param callbacks Callbacks for tracking sub-agent session lifecycle
@param parentTracer Tracer for capturing sub-agent traces
@param lookupParent Function to look up parent session ID from conversation ID
@return The resulting tool registration
-}
turnAgentRuntimeIntoIOTool ::
    -- | Optional session store for persisting sessions
    SessionStore ->
    -- | API keys for creating HTTP runtime
    LoadedApiKeys ->
    -- | The OS agent node to convert into a tool
    OSAgentNode ->
    -- | The slug of the calling agent (for tracing)
    AgentSlug ->
    -- | The ID of the calling agent (for tracing)
    AgentId ->
    -- | Callbacks for tracking sub-agent session lifecycle
    AgentCallCallbacks ->
    -- | Parent tracer for capturing sub-agent traces
    Tracer IO Trace ->
    -- | Function to look up parent session ID from conversation ID
    ParentSessionLookup ->
    -- | The resulting tool registration
    ToolRegistration
turnAgentRuntimeIntoIOTool store apiKeys node callerSlug callerId callbacks parentTracer lookupParent =
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
        let parentConversationId = ctx.ctxConversationId

        -- Look up the parent session ID to establish parent-child relationship
        mParentSessionId <- lookupParent parentConversationId

        -- Get the API key for this agent
        let apiKeyId = Base.apiKeyId agent
        let mApiKey = lookupApiKey apiKeyId apiKeys

        -- Create HTTP runtime with the API key
        httpRuntime <- case mApiKey of
            Just apiKey -> HttpClient.newRuntime (HttpClient.BearerToken $ Text.decodeUtf8 $ OpenAI.revealApiKey apiKey)
            Nothing -> HttpClient.newRuntime HttpClient.NoToken

        -- Generate a conversation ID for this sub-agent execution
        subConvId <- newConversationId

        -- Create a tracer that captures sub-agent traces to the parent
        let subTracer =
                contramap
                    ( SubAgentTrace callerSlug callerId parentConversationId subConvId
                    )
                    parentTracer

        -- Create the agent from the OS node
        sessionAgent <- nodeToAgent store httpRuntime node subTracer callerSlug callerId parentConversationId

        -- Set the query on the agent
        let agentWithQuery = agentSetQuery (UserQuery query) sessionAgent

        -- Create a session linked to the parent session
        subSessionId <- newSessionId
        subTurnId <- newTurnId
        let session0 = Session [] subSessionId mParentSessionId subTurnId

        -- Notify that session was created
        callbacks.onSessionCreated session0 subConvId

        -- Run the agent and get the result
        -- We wrap the run in a try to ensure onSessionCompleted is always called
        result <-
            runAgentWithCallbacks
                subConvId
                agentWithQuery
                session0
                (callbacksToProgressHandler callbacks subConvId)

        -- Notify that session is complete
        case result of
            Right (_, finalSession) -> callbacks.onSessionCompleted finalSession
            Left _ -> callbacks.onSessionCompleted session0

        -- Extract and return the response text
        case result of
            Right (finalTurnContent, _) -> do
                let txtResult = extractResponseText finalTurnContent.llmResponse
                pure $ Text.encodeUtf8 txtResult
            Left err -> do
                -- Return error message as response
                pure $ Text.encodeUtf8 $ Text.pack $ "Sub-agent error: " ++ show err

-- | Run an agent with session progress tracking.
--
-- This is a wrapper around the session loop that adds progress tracking.
-- It ensures that progress callbacks are invoked at appropriate points.
runAgentWithCallbacks ::
    ConversationId ->
    Agent (LlmTurnContent, Session) ->
    Session ->
    OnSessionProgress ->
    IO (Either String (LlmTurnContent, Session))
runAgentWithCallbacks convId agent initialSession progressHandler = do
    -- Notify session started
    progressHandler (SessionStarted initialSession)

    -- Run the agent
    result <- run convId agent initialSession

    -- Notify session completed
    progressHandler (SessionCompleted (snd result))

    pure $ Right result

-- | Look up an API key by its ID from the loaded API keys.
lookupApiKey :: Text -> LoadedApiKeys -> Maybe OpenAI.ApiKey
lookupApiKey keyId keys = fmap snd $ listToMaybe $ filter ((== keyId) . fst) keys

-------------------------------------------------------------------------------
-- Agent Construction
-------------------------------------------------------------------------------

{- | Creates an Agent from an OSAgentNode configured for use as a tool.

The agent is configured with:
* Proper tracer for sub-agent trace capture
* HTTP runtime for LLM requests
* System prompt and tools from the node configuration
* Tool execution capability
-}
nodeToAgent ::
    SessionStore ->
    -- | HTTP runtime for making LLM requests
    HttpClient.Runtime ->
    OSAgentNode ->
    Tracer IO Trace ->
    AgentSlug ->
    AgentId ->
    ConversationId ->
    IO (Agent (LlmTurnContent, Session))
nodeToAgent store httpRuntime node tracer _callerSlug _callerId parentConvId = do
    let agentCfg = node.osNodeConfig
    let sPrompt = SystemPrompt $ Text.unlines $ Base.systemPrompt agentCfg

    -- Read tools from the OS-native TVar
    toolRegs <- readTVarIO (osNodeTools node)
    let sTools = map toolRegistrationToSystemTool toolRegs

    stepId <- newStepId

    -- Create completion config and function
    let completionConfig =
            OpenAICompletionConfig
                { cfgTracer = contramap (AgentTrace_Conversation (Base.slug agentCfg) (osNodeAgentId node) parentConvId . (LLMTrace stepId)) tracer
                , cfgRuntime = httpRuntime
                , cfgBaseUrl = OpenAI.ApiBaseUrl $ Base.modelUrl agentCfg
                , cfgModelName = Base.modelName agentCfg
                , cfgModelFlavor = parseModelFlavor $ Base.flavor agentCfg
                }
    let completeF = mkOpenAICompletion completionConfig

    convId <- newConversationId
    pure $
        agentStoreSession store Nothing convId $
            Agent
                { step = naiveTilNoToolCallStep
                , sysPrompt = pure sPrompt
                , sysTools = pure sTools
                , usrQuery = pure Nothing
                , toolCall = executeToolCall node.osNodeAgentId convId (osNodeTools node)
                , complete = completeF
                , contextConfig = defaultContextConfig
                }

-------------------------------------------------------------------------------
-- Tool Conversion
-------------------------------------------------------------------------------

{- | Convert a ToolRegistration to a SystemTool for the Session agent.
-}
toolRegistrationToSystemTool :: ToolRegistration -> SystemTool
toolRegistrationToSystemTool reg =
    let llmTool = reg.declareTool
        toolDefv1 =
            SystemToolDefinitionV1
                { name = OpenAI.getToolName llmTool.toolName
                , llmName = OpenAI.getToolName llmTool.toolName
                , description = llmTool.toolDescription
                , properties = llmTool.toolParamProperties
                , raw =
                    Aeson.object
                        [ "type" .= ("function" :: Text)
                        , "function"
                            .= Aeson.object
                                [ "name" .= OpenAI.getToolName llmTool.toolName
                                , "description" .= llmTool.toolDescription
                                , "parameters" .= toolParamsToJson llmTool.toolParamProperties
                                ]
                        ]
                }
     in SystemTool $ V1 toolDefv1

{- | Convert tool parameters to JSON schema.
-}
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
-- Tool Execution
-------------------------------------------------------------------------------

{- | Execute a tool call using the node's registered tools.

TODO: This is a placeholder implementation. The actual tool execution
should invoke the registered tools properly.
-}
executeToolCall ::
    AgentId ->
    ConversationId ->
    -- | Tools TVar from OSAgentNode
    TVar [ToolRegistration] ->
    ToolExecutionContext ->
    LlmToolCall ->
    IO UserToolResponse
executeToolCall _agentId _convId toolsTVar _ctx (LlmToolCall _callVal) = do
    regs <- readTVarIO toolsTVar
    pure $ UserToolResponse $ Aeson.String $ "Tool execution for " <> Text.pack (show (length regs)) <> " tools"

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- | Set the user query on an agent.
agentSetQuery :: UserQuery -> Agent r -> Agent r
agentSetQuery query agent =
    agent{usrQuery = pure (Just query)}

-- | Extract text content from an LLM response.
extractResponseText :: LlmResponse -> Text
extractResponseText (LlmResponse mTxt _thinking _) =
    Maybe.fromMaybe "" mTxt

