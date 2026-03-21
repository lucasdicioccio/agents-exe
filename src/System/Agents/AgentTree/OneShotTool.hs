{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Provides a version of turnAgentRuntimeIntoIOTool based on OneShot.hs
implementation of LLM session calls.
-}
module System.Agents.AgentTree.OneShotTool (
    turnAgentRuntimeIntoIOTool,
) where

import Control.Concurrent.STM (readTVarIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as CByteString
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Prod.Tracer (contramap)

import System.Agents.Base (AgentId, AgentSlug, ConversationId, StepId, newConversationId, newStepId)
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.OneShot (agentStoreSession)
import qualified System.Agents.Runtime as Runtime
import System.Agents.Runtime.Runtime (Runtime (..))
import System.Agents.Session.Base (
    Agent (..),
    LlmResponse (..),
    LlmToolCall (..),
    LlmTurnContent (..),
    Session (..),
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

-- | Data type for the prompt argument to the sub-agent.
newtype PromptOtherAgent = PromptOtherAgent
    { what :: Text
    }
    deriving (Show)

instance Aeson.FromJSON PromptOtherAgent where
    parseJSON = Aeson.withObject "PromptOtherAgent" $ \v ->
        PromptOtherAgent <$> v Aeson..: "what"

-------------------------------------------------------------------------------

-- | Create a new root session with no parent.
mkRootSession :: IO Session
mkRootSession = do
    sessId <- newSessionId
    tId <- newTurnId
    pure $
        Session
            { turns = []
            , sessionId = sessId
            , forkedFromSessionId = Nothing
            , turnId = tId
            , parentSessionId = Nothing
            , parentConversationId = Nothing
            , parentAgentSlug = Nothing
            }

{- | Converts a Runtime into an IO Tool using the OneShot session-based approach.

This version uses the LLM session calls from OneShot.hs instead of
Runtime.handleConversation. It creates an Agent from the Runtime,
runs it with a session, and returns the result.
-}
turnAgentRuntimeIntoIOTool ::
    -- | Optional session store for persisting sessions
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
            runSubAgent

    -- Run the sub-agent with the given prompt and execution context
    -- The ToolExecutionContext provides access to session metadata including
    -- the conversation ID for tracing and session management.
    runSubAgent :: ToolExecutionContext -> PromptOtherAgent -> IO CByteString.ByteString
    runSubAgent ctx (PromptOtherAgent query) = do
        -- Extract the conversation ID from the execution context for tracing
        let callerConvId = ctx.ctxConversationId

        -- Create the agent from the runtime with the OneShot configuration
        agent <- runtimeToAgentForToolInIOScriptExecution store rt callerSlug callerId callerConvId

        -- Set the query on the agent
        let agentWithQuery = agentSetQuery (UserQuery query) agent

        -- Create a fresh session
        session0 <- mkRootSession

        -- Generate a conversation ID for this execution
        convId <- newConversationId

        -- Run the agent and get the result
        -- Loop.run now requires convId as the first argument
        (finalTurnContent, _) <- run convId agentWithQuery session0

        -- Extract and return the response text
        let result = extractResponseText finalTurnContent.llmResponse
        pure $ Text.encodeUtf8 result

-------------------------------------------------------------------------------

{- | Creates an Agent from a Runtime configured for use as a tool.
Based on runtimeToAgent from OneShot.hs.
-}
runtimeToAgentForToolInIOScriptExecution ::
    SessionStore ->
    Runtime ->
    AgentSlug ->
    AgentId ->
    ConversationId ->
    IO (Agent (LlmTurnContent, Session))
runtimeToAgentForToolInIOScriptExecution store rt callerSlug callerId parentConvId = do
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
    pure $
        agentStoreSession store Nothing convId $
            Agent
                { step = naiveTilNoToolCallStep
                , sysPrompt = pure sPrompt
                , sysTools = pure sTools
                , usrQuery = pure Nothing
                , -- toolCall now accepts ToolExecutionContext as first argument
                  toolCall = executeToolCall rt.agentId convId rt.agentTools
                , complete = completeF
                , -- Add contextConfig field (required for Agent)
                  contextConfig = defaultContextConfig
                }
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
