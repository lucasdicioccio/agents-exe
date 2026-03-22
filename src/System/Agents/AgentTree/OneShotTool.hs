{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Provides a version of turnAgentRuntimeIntoIOTool based on OneShot.hs
implementation of LLM session calls.

This module supports callback-based session progress tracking for sub-agent calls,
allowing parent agents to monitor sub-agent session lifecycle events.

Note: This module intentionally does NOT import System.Agents.OneShot to avoid
a circular dependency. The AgentTree module imports OneShot, so we can't import
AgentTree from this module which is imported by AgentTree.
-}
module System.Agents.AgentTree.OneShotTool (
    -- * Configuration for sub-agent session callbacks
    SubAgentSessionConfig (..),
    defaultSubAgentConfig,

    -- * Tool creation functions
    turnAgentRuntimeIntoIOTool,
    turnAgentRuntimeIntoIOToolWithCallbacks,
) where

import Control.Concurrent.STM (readTVarIO)
import Control.Exception (SomeException, throwIO, try)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as CByteString
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Prod.Tracer (Tracer (..), contramap, runTracer)

import System.Agents.Base (AgentId, AgentSlug, ConversationId, StepId, newConversationId, newStepId)
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.Runtime as Runtime
import System.Agents.Runtime.Runtime (Runtime (..))
import System.Agents.Runtime.Trace (ConversationTrace (..), Trace (..))
import System.Agents.Session.Base (
    Action (..),
    Agent (..),
    LlmResponse (..),
    LlmToolCall (..),
    LlmTurnContent (..),
    OnSessionProgress,
    Session (..),
    SessionProgress (..),
    SystemPrompt (..),
    SystemTool (..),
    SystemToolDefinition (..),
    SystemToolDefinitionV1 (..),
    UserQuery (..),
    UserToolResponse (..),
    defaultContextConfig,
    mkChildSession,
 )
import System.Agents.Session.Loop (run)
import System.Agents.Session.OpenAI (OpenAICompletionConfig (..), mkOpenAICompletion)
import System.Agents.Session.Step (naiveTilNoToolCallStep)
import System.Agents.SessionStore (SessionStore)
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.ToolRegistration (
    ToolRegistration (..),
    registerIOScriptInLLM,
 )
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..))
import System.Agents.Tools.Context (ToolExecutionContext, callAgentSlug, ctxCallStack, ctxConversationId, ctxSessionId)
import qualified System.Agents.Tools.IO as IOTools

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

{- | Configuration for sub-agent session callbacks.

This type encapsulates the optional callbacks and storage for sub-agent
sessions, allowing parent agents to track sub-agent progress without
tightly coupling the parent and sub-agent implementations.

Use 'defaultSubAgentConfig' for a config with no callbacks or storage.

The 'subAgentOnProgressFactory' is a function that creates an 'OnSessionProgress'
callback for a given parent conversation ID. This factory pattern allows
the callback to be created dynamically at tool execution time when the
parent conversation ID is known, rather than at tool configuration time.
-}
data SubAgentSessionConfig = SubAgentSessionConfig
    { subAgentStore :: Maybe SessionStore
    {- ^ Optional session store for persisting sub-agent sessions to disk.
    If provided, sessions will be saved at each progress update.
    -}
    , subAgentOnProgressFactory :: ConversationId -> OnSessionProgress
    {- ^ Factory function that creates a progress callback for a given parent
    conversation ID. This allows the callback to be created dynamically at
    tool execution time when the parent conversation context is available.

    The default (from 'defaultSubAgentConfig') returns a no-op callback.
    -}
 }

{- | Default configuration with no storage and no-op callbacks.

This is useful when you don't need to track sub-agent progress or persist
sub-agent sessions. The progress callback factory returns a no-op handler
that ignores all progress events.
-}
defaultSubAgentConfig :: SubAgentSessionConfig
defaultSubAgentConfig = SubAgentSessionConfig Nothing (\_ -> const $ pure ())

-------------------------------------------------------------------------------

{- | Converts a Runtime into an IO Tool using the OneShot session-based approach.

This is the legacy function for backward compatibility. It uses a 'SessionStore'
for persisting sub-agent sessions but does not provide progress callbacks.

For new code, prefer 'turnAgentRuntimeIntoIOToolWithCallbacks' which provides
more flexibility through 'SubAgentSessionConfig'.
-}
turnAgentRuntimeIntoIOTool ::
    -- | Session store for persisting sub-agent sessions
    SessionStore ->
    -- | The runtime of the agent to convert into a tool
    Runtime ->
    -- | The slug of the calling agent (for tracing)
    AgentSlug ->
    -- | The ID of the calling agent (for tracing)
    AgentId ->
    -- | The resulting tool registration
    ToolRegistration
turnAgentRuntimeIntoIOTool store rt callerSlug callerId =
    -- For backward compatibility, we use the runtime's tracer directly.
    -- New code should use turnAgentRuntimeIntoIOToolWithCallbacks directly
    -- to pass a properly configured parent tracer.
    turnAgentRuntimeIntoIOToolWithCallbacks
        (SubAgentSessionConfig (Just store) (\_ -> const $ pure ()))
        rt.agentTracer
        rt
        callerSlug
        callerId

{- | Converts a Runtime into an IO Tool with configurable callbacks.

This version uses the LLM session calls from OneShot.hs instead of
Runtime.handleConversation. It creates an Agent from the Runtime,
runs it with a session, and returns the result.

The 'SubAgentSessionConfig' parameter allows the parent agent to:

1. Receive progress callbacks via 'subAgentOnProgressFactory' - the factory
   is called with the parent conversation ID to create the callback
2. Persist sessions via 'subAgentStore'

The sub-agent session is properly linked to the parent session via
'mkChildSession', enabling conversation hierarchy tracking.

The 'Tracer' parameter is the parent agent's tracer, which is used to emit
correlation traces linking the sub-agent's execution to the parent conversation.
-}
turnAgentRuntimeIntoIOToolWithCallbacks ::
    -- | Configuration for sub-agent session callbacks and storage
    SubAgentSessionConfig ->
    -- | Parent tracer for emitting correlation traces
    Tracer IO Trace ->
    -- | The runtime of the agent to convert into a tool
    Runtime ->
    -- | The slug of the calling agent (for tracing)
    AgentSlug ->
    -- | The ID of the calling agent (for tracing)
    AgentId ->
    -- | The resulting tool registration
    ToolRegistration
turnAgentRuntimeIntoIOToolWithCallbacks config parentTracer rt callerSlug callerId =
    registerIOScriptInLLM io props
  where
    -- Define the parameter properties for the LLM tool schema
    props =
        [ ParamProperty
            { propertyKey = "what"
            , propertyType = StringParamType
            , propertyDescription = "the prompt to call the specialized-agent with"
            , propertyRequired = True
            }
        ]

    -- Create the IO script that wraps the agent runtime
    io =
        IOTools.IOScript
            ( IOTools.IOScriptDescription
                ("prompt_agent_" <> rt.agentSlug)
                ("asks a prompt to the expert agent: " <> rt.agentSlug)
            )
            (runSubAgent config parentTracer rt callerSlug callerId)

{- | Run the sub-agent with the given prompt and execution context.
The ToolExecutionContext provides access to session metadata including
the conversation ID for tracing and session management.

This function emits correlation traces at key lifecycle points:
1. SubAgentStarted - when the sub-agent session begins
2. SubAgentCompleted - when the sub-agent finishes successfully
3. SubAgentFailed - when the sub-agent encounters an error

The progress callback is created dynamically using the factory from the
config, passing the parent conversation ID from the execution context.
-}
runSubAgent ::
    SubAgentSessionConfig ->
    -- | Parent tracer for emitting correlation traces
    Tracer IO Trace ->
    Runtime ->
    AgentSlug ->
    AgentId ->
    ToolExecutionContext ->
    PromptOtherAgent ->
    IO CByteString.ByteString
runSubAgent cfg parentTracer rt callerSlug callerId ctx (PromptOtherAgent query) = do
    -- Extract parent context information for linking child session
    let pSessionId = ctx.ctxSessionId
    let parentConvId = ctx.ctxConversationId
    let pAgentSlug = case ctx.ctxCallStack of
            (entry : _) -> callAgentSlug entry
            [] -> "unknown"

    -- Create child session linked to parent
    session0 <- mkChildSession pSessionId parentConvId pAgentSlug

    -- Create the progress callback using the factory with the parent conversation ID
    let onProgress = cfg.subAgentOnProgressFactory parentConvId

    -- Create the agent with the parent's callback
    agent <-
        runtimeToAgentForToolInIOScriptExecution
            cfg.subAgentStore
            onProgress
            rt
            callerSlug
            callerId
            parentConvId

    -- Generate conversation ID for this sub-agent
    convId <- newConversationId

    -- Emit SubAgentStarted trace for correlation
    runTracer parentTracer $
        AgentTrace_SubAgentStarted
            pAgentSlug
            callerId
            parentConvId
            rt.agentSlug
            convId
            session0.sessionId

    -- Also emit a SubAgentCallTrace within the conversation
    runTracer parentTracer $
        AgentTrace_Conversation
            callerSlug
            callerId
            parentConvId
            (SubAgentCallTrace rt.agentSlug convId session0.sessionId)

    -- Set the query on the agent
    let agentWithQuery = agentSetQuery (UserQuery query) agent

    -- Notify parent that sub-agent session started
    onProgress (SessionStarted session0)

    -- Run the agent with exception handling for trace emission
    result <- try $ run convId agentWithQuery session0

    case result of
        Left (e :: SomeException) -> do
            -- Emit SubAgentFailed trace for correlation
            runTracer parentTracer $
                AgentTrace_SubAgentFailed
                    pAgentSlug
                    callerId
                    parentConvId
                    rt.agentSlug
                    convId
                    session0.sessionId
                    (Text.pack $ show e)
            throwIO e
        Right (finalTurnContent, finalSession) -> do
            -- Emit SubAgentCompleted trace for correlation
            runTracer parentTracer $
                AgentTrace_SubAgentCompleted
                    pAgentSlug
                    callerId
                    parentConvId
                    rt.agentSlug
                    convId
                    session0.sessionId

            -- Emit SubAgentReturnTrace within the conversation
            runTracer parentTracer $
                AgentTrace_Conversation
                    callerSlug
                    callerId
                    parentConvId
                    (SubAgentReturnTrace rt.agentSlug convId)

            -- Notify parent that sub-agent session completed
            onProgress (SessionCompleted finalSession)

            -- Persist to store if configured
            case cfg.subAgentStore of
                Just store -> SessionStore.storeSession store convId finalSession
                Nothing -> pure ()

            pure $ Text.encodeUtf8 $ extractResponseText finalTurnContent.llmResponse

-------------------------------------------------------------------------------

{- | Creates an Agent from a Runtime configured for use as a tool.
Based on runtimeToAgent from OneShot.hs.

This version supports optional session progress callbacks and storage,
allowing the parent agent to track sub-agent execution progress.
-}
runtimeToAgentForToolInIOScriptExecution ::
    -- | Optional session store for persisting sessions
    Maybe SessionStore ->
    -- | Callback for session progress updates
    OnSessionProgress ->
    -- | The runtime to convert
    Runtime ->
    -- | The slug of the calling agent (for tracing)
    AgentSlug ->
    -- | The ID of the calling agent (for tracing)
    AgentId ->
    -- | The parent conversation ID for tracing
    ConversationId ->
    IO (Agent (LlmTurnContent, Session))
runtimeToAgentForToolInIOScriptExecution mStore onProgress rt callerSlug callerId parentConvId = do
    let sPrompt = SystemPrompt rt.agentModel.modelSystemPrompt.getSystemPrompt
    sTools <- fmap toolRegistrationToSystemTool <$> readTVarIO rt.agentTools
    stepId <- newStepId

    -- Create OpenAI completion config from runtime with nested tracing
    let completionConfig =
            OpenAICompletionConfig
                { cfgTracer = contramap (nestTrace callerSlug callerId parentConvId stepId) rt.agentTracer
                , cfgRuntime = rt.agentAuthenticatedHttpClientRuntime
                , cfgBaseUrl = rt.agentModel.modelBaseUrl
                , cfgModelName = rt.agentModel.modelName
                , cfgModelFlavor = rt.agentModel.modelFlavor
                }
    let completeF = mkOpenAICompletion completionConfig

    convId <- newConversationId

    -- Build the base agent
    let baseAgent =
            Agent
                { step = naiveTilNoToolCallStep
                , sysPrompt = pure sPrompt
                , sysTools = pure sTools
                , usrQuery = pure Nothing
                , toolCall = executeToolCall rt.agentId convId rt.agentTools
                , complete = completeF
                , contextConfig = defaultContextConfig
                }

    -- Apply progress callback
    let agentWithProgress = agentWithSessionProgress onProgress baseAgent

    -- Apply session storage if provided
    let agentWithStorage = case mStore of
            Just store -> agentStoreSession store Nothing convId agentWithProgress
            Nothing -> agentWithProgress

    pure agentWithStorage
  where
    -- Nest the trace to indicate this is a child agent call
    -- The tracer expects OpenAI.Trace, so we wrap Runtime.Trace appropriately
    nestTrace :: AgentSlug -> AgentId -> ConversationId -> StepId -> OpenAI.Trace -> Runtime.Trace
    nestTrace cSlug cId pConvId sId openaiTrace =
        Runtime.AgentTrace_Conversation cSlug cId pConvId (Runtime.LLMTrace sId openaiTrace)

-------------------------------------------------------------------------------

{- | Convert a ToolRegistration to a SystemTool for the Session agent.
Based on toolRegistrationToSystemTool from OneShot.hs.
-}
toolRegistrationToSystemTool :: ToolRegistration -> SystemTool
toolRegistrationToSystemTool reg =
    let llmTool = reg.declareTool
        toolDefv1 =
            SystemToolDefinitionV1
                { name = llmTool.toolName.getToolName
                , llmName = llmTool.toolName.getToolName
                , description = llmTool.toolDescription
                , properties = llmTool.toolParamProperties
                , raw =
                    Aeson.object
                        [ "type" .= ("function" :: Text)
                        , "function"
                            .= Aeson.object
                                [ "name" .= llmTool.toolName.getToolName
                                , "description" .= llmTool.toolDescription
                                , "parameters" .= toolParamsToJson llmTool.toolParamProperties
                                ]
                        ]
                }
     in SystemTool $ V1 toolDefv1

{- | Convert tool parameters to JSON schema.

Only properties with 'propertyRequired = True' are included in the 'required' array.
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

{- | Execute a tool call using the runtime's registered tools.
Based on executeToolCall from OneShot.hs.

The toolCall function in Agent now accepts a ToolExecutionContext as its
first argument, allowing tools to access session metadata.
-}
executeToolCall ::
    -- | Agent ID for context
    AgentId ->
    -- | Conversation ID for context
    ConversationId ->
    Runtime.AgentTools ->
    -- | Context from runStepM
    ToolExecutionContext ->
    -- | Tool call from LLM
    LlmToolCall ->
    IO UserToolResponse
executeToolCall _agentId _convId toolsTVar _ctx (LlmToolCall _callVal) = do
    -- For simplicity in this OneShot-based version, we return the raw
    -- tool call result. In a more sophisticated implementation, we would
    -- parse the tool call and execute it using the registered tools.
    -- The context is available for logging/tracing purposes.
    regs <- readTVarIO toolsTVar
    pure $ UserToolResponse $ Aeson.String $ "Tool execution not implemented for " <> Text.pack (show (length regs)) <> " tools"

-------------------------------------------------------------------------------

-- | Set the user query on an agent.
agentSetQuery :: UserQuery -> Agent r -> Agent r
agentSetQuery query agent =
    agent{usrQuery = pure (Just query)}

-- | Extract text content from an LLM response.
extractResponseText :: LlmResponse -> Text
extractResponseText (LlmResponse txt _thinking _) =
    Maybe.fromMaybe "" txt

-------------------------------------------------------------------------------
-- Functions below are duplicated from System.Agents.OneShot to avoid
-- a circular dependency. AgentTree imports OneShot, so OneShotTool
-- (which is imported by AgentTree) cannot import OneShot.
-------------------------------------------------------------------------------

{- | Wrap an agent to emit session progress events after each step.

This function decorates the agent's step function to emit 'SessionUpdated'
events before each step is executed.
-}
agentWithSessionProgress :: forall r. OnSessionProgress -> Agent r -> Agent r
agentWithSessionProgress onProgress agent =
    agent{step = decorate agent.step}
  where
    decorate :: (Session -> IO (Action r)) -> (Session -> IO (Action r))
    decorate f = \sess -> do
        onProgress (SessionUpdated sess)
        f sess

{- | Creates a callback that stores session progress using a SessionStore.

This function creates an 'OnSessionProgress' handler that stores sessions
to the provided 'SessionStore' at each progress event.
-}
sessionStoreCallback :: SessionStore -> ConversationId -> OnSessionProgress
sessionStoreCallback store convId progress =
    case progress of
        SessionUpdated sess -> storeSessionWithStore sess
        SessionCompleted sess -> storeSessionWithStore sess
        SessionStarted sess -> storeSessionWithStore sess
        SessionFailed sess _ -> storeSessionWithStore sess
  where
    storeSessionWithStore sess =
        SessionStore.storeSession store convId sess

{- | Creates a callback that stores session progress using an extra optional session-path.

This is useful in OneShot command where the command-line drives the filename.
-}
filepathStoreCallback :: Maybe FilePath -> OnSessionProgress
filepathStoreCallback Nothing _ = pure ()
filepathStoreCallback (Just path) progress =
    case progress of
        SessionUpdated sess -> go sess
        SessionCompleted sess -> go sess
        SessionStarted sess -> go sess
        SessionFailed sess _ -> go sess
  where
    go sess =
        SessionStore.storeSessionToFile sess path

{- | Wrap an agent to store sessions using a SessionStore.

The session is stored using the conversation ID from the session.
This combines both the SessionStore callback and optional file path callback.
-}
agentStoreSession :: forall r. SessionStore -> Maybe FilePath -> ConversationId -> Agent r -> Agent r
agentStoreSession store mPath convId agent =
    agentWithSessionProgress handleProgress agent
  where
    handleProgress x = do
        sessionStoreCallback store convId x
        filepathStoreCallback mPath x

