{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Provides a version of turnAgentRuntimeIntoIOTool based on OneShot.hs
implementation of LLM session calls.

This module has been updated to work with OS-native structures.
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
import Data.Maybe (listToMaybe)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Prod.Tracer (Tracer (..), contramap)

import System.Agents.AgentTree (LoadedApiKeys, OSAgentNode (..))
import System.Agents.Base (AgentId, AgentSlug, ConversationId, newConversationId, newStepId)
import qualified System.Agents.Base as Base
import qualified System.Agents.HttpClient as HttpClient
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.OneShot (agentStoreSession, parseModelFlavor)
import System.Agents.Runtime.Trace (ConversationTrace (..), Trace (..))
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
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..), ToolDescription (..), ToolName (..))
import System.Agents.Tools.Context (ToolExecutionContext, ctxConversationId)
import System.Agents.Tools.ExecuteToolCall (executeLlmToolCall)
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

{- | Converts an OSAgentNode into an IO Tool using the OneShot session-based approach.

This version uses the LLM session calls from OneShot.hs. It creates an Agent from
the OSAgentNode, runs it with a session, and returns the result.

NOTE: This is a transitional implementation. In the final OS-native architecture,
this would use OS-native session management directly.
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
    -- | The resulting tool registration
    ToolRegistration
turnAgentRuntimeIntoIOTool store apiKeys node callerSlug callerId =
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

        -- Create a tracer (using a no-op tracer for now)
        let tracer = Tracer $ const $ pure ()

        -- Get the API key for this agent
        let apiKeyId = Base.apiKeyId agent
        let mApiKey = lookupApiKey apiKeyId apiKeys

        -- Create HTTP runtime with the API key
        httpRuntime <- case mApiKey of
            Just apiKey -> HttpClient.newRuntime (HttpClient.BearerToken $ Text.decodeUtf8 $ OpenAI.revealApiKey apiKey)
            Nothing -> HttpClient.newRuntime HttpClient.NoToken

        -- Create the agent from the OS node
        sessionAgent <- nodeToAgent store httpRuntime node tracer callerSlug callerId parentConversationId

        -- Set the query on the agent
        let agentWithQuery = agentSetQuery (UserQuery query) sessionAgent

        -- Create a fresh session
        session0 <- Session [] <$> newSessionId <*> pure Nothing <*> newTurnId

        -- Generate a conversation ID for this execution
        convId <- newConversationId

        -- Run the agent and get the result
        (finalTurnContent, _) <- run convId agentWithQuery session0

        -- Extract and return the response text
        let result = extractResponseText finalTurnContent.llmResponse
        pure $ Text.encodeUtf8 result

-- | Look up an API key by its ID from the loaded API keys.
lookupApiKey :: Text -> LoadedApiKeys -> Maybe OpenAI.ApiKey
lookupApiKey keyId keys = fmap snd $ listToMaybe $ filter ((== keyId) . fst) keys

-------------------------------------------------------------------------------

-- | Creates an Agent from an OSAgentNode configured for use as a tool.
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

    let tp = ToolPortal.makeToolPortal (contramap (ToolPortalTrace (Base.slug agentCfg)) tracer) (osNodeTools node)
    convId <- newConversationId
    pure $
        agentStoreSession store Nothing convId $
            Agent
                { step = naiveTilNoToolCallStep
                , sysPrompt = pure sPrompt
                , sysTools = pure sTools
                , usrQuery = pure Nothing
                , toolCall = executeLlmToolCall (contramap (ToolTrace (Base.slug agentCfg)) tracer) (osNodeTools node) (SessionCompat.parseToolCallFromLlmToolCall, SessionCompat.callResultToUserToolResponse)
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
extractResponseText (LlmResponse txt _thinking _) =
    Maybe.fromMaybe "" txt
