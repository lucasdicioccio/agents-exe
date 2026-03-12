{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.MCP.Server where

import Conduit (stdinC)
import Control.Monad (void)
import Control.Monad.Logger (LoggingT (..), defaultOutput, logDebugN)
import Control.Monad.Reader (runReaderT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import Data.Conduit.Combinators (sinkHandleFlush)
import Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Lazy as LText
import Formatting ((%))
import qualified Formatting as Format
import qualified Network.JSONRPC as Rpc
import Prod.Tracer (Tracer (..), contramap)
import UnliftIO (async, liftIO, stderr, stdout)

import qualified System.Agents.AgentTree as AgentTree
import System.Agents.Base (AgentId, announce, newConversationId, newStepId, slug)
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.MCP.Base as Mcp
import qualified System.Agents.Runtime as Runtime
import System.Agents.Runtime.Runtime (Runtime (..))
import System.Agents.Tools (mapCallResult, toolRun)
import System.Agents.Tools.Base (CallResult (..))
import System.Agents.Tools.Context (CallStackEntry (..), ToolExecutionContext, mkToolExecutionContext)

import System.Agents.MCP.Server.Runtime

-- Imports for LLM session-based approach (from OneShot.hs)
import qualified System.Agents.Session.Base as SessionBase
import System.Agents.Session.Loop (run)
import System.Agents.Session.OpenAI (OpenAICompletionConfig (..), mkOpenAICompletion)
import System.Agents.Session.Step (naiveTilNoToolCallStep)
import System.Agents.ToolRegistration (ToolRegistration (..))
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..))

-- | Configuration for the MCP server.
data McpServerConfig = McpServerConfig
    { mcpOnSessionProgress :: SessionBase.OnSessionProgress
    {- ^ Optional callback for session progress tracking.
    Defaults to 'SessionBase.ignoreSessionProgress'.
    -}
    }

-- | Default MCP server configuration with no session tracking.
defaultMcpServerConfig :: McpServerConfig
defaultMcpServerConfig =
    McpServerConfig
        { mcpOnSessionProgress = SessionBase.ignoreSessionProgress
        }

mainAgentServer :: AgentTree.Props -> IO ()
mainAgentServer props = do
    multiAgentsServer defaultMcpServerConfig [props]

-- | Run MCP server with a custom configuration.
runMcpServerWithConfig :: McpServerConfig -> [AgentTree.Props] -> IO ()
runMcpServerWithConfig config props = multiAgentsServer config props

multiAgentsServer :: McpServerConfig -> [AgentTree.Props] -> IO ()
multiAgentsServer config xs = multiAgentsServer' config 0 xs []

multiAgentsServer' :: McpServerConfig -> Int -> [AgentTree.Props] -> MappedTools -> IO ()
multiAgentsServer' _ _ [] [] = do
    print ("no agent definitions" :: Text)
multiAgentsServer' config _ [] mtools = do
    rt <- initRuntime mtools
    -- Store the callback in the runtime for access during tool calls
    let rtWithCallback = rt{mcpSessionProgress = Just (mcpOnSessionProgress config)}
    runLoggingT (runReaderT (runMcpStack mainMcp) rtWithCallback) logTrace
  where
    logTrace =
        defaultOutput stderr
multiAgentsServer' config idx (props : xs) mtools = do
    AgentTree.withAgentTreeRuntime props go
  where
    go (AgentTree.Initialized ai) = do
        let oai = ai.agentBase
        let toolname = Format.format ("ask_" % Format.text % "_" % Format.left 3 '0') (LText.fromStrict oai.slug) idx
        let tool = ExpertAgentAsPrompt (LText.toStrict toolname) ai
        multiAgentsServer' config (succ idx) xs (tool : mtools)
    go _ = do
        print ("failed to initialize" :: Text)

mainMcp :: McpStack ()
mainMcp = do
    runJSONRPCT'
        Rpc.V2
        False
        (sinkHandleFlush stdout)
        stdinC
        handlerLoop

debugString :: String -> Rpc.JSONRPCT McpStack ()
debugString = logDebugN . Text.pack

debugShow :: (Show a) => a -> Rpc.JSONRPCT McpStack ()
debugShow = debugString . show

-------------------------------------------------------------------------------
serverProtocolVersion :: Text
serverProtocolVersion = "2024-11-05"

serverImplem :: Mcp.Implementation
serverImplem = Implementation "agents-exe-mcp-server" "0.0.1"

serverCapabilities :: Mcp.ServerCapabilities
serverCapabilities =
    Mcp.ServerCapabilities
        (Just mempty)
        (Just $ Aeson.object [])
        [ToolsListChanged, PromptsListChanged, ResourcesListChanged]

-------------------------------------------------------------------------------
data ClientMsg
    = InitializeMsg Mcp.InitializeRequest
    | NotifyInitializedMsg Mcp.InitializedNotification
    | ListResourcesRequestMsg Mcp.ListResourcesRequest
    | ListToolsRequestMsg Mcp.ListToolsRequest
    | ListPromptsRequestMsg Mcp.ListPromptsRequest
    | CallToolRequestMsg Mcp.CallToolRequest
    | CancelledNotificationMsg Mcp.CancelledNotification
    deriving (Show)

instance Rpc.FromRequest ClientMsg where
    parseParams "initialize" =
        Just (fmap InitializeMsg <$> Aeson.parseJSON)
    parseParams "notifications/initialized" =
        Just (\_ -> pure (NotifyInitializedMsg Mcp.InitializedNotification))
    parseParams "notifications/cancelled" =
        Just (fmap CancelledNotificationMsg <$> Aeson.parseJSON)
    parseParams "resources/list" =
        Just (fmap ListResourcesRequestMsg <$> Aeson.parseJSON)
    parseParams "tools/list" =
        Just (fmap ListToolsRequestMsg <$> Aeson.parseJSON)
    parseParams "prompts/list" =
        Just (fmap ListPromptsRequestMsg <$> Aeson.parseJSON)
    parseParams "tools/call" =
        Just (fmap CallToolRequestMsg <$> Aeson.parseJSON)
    parseParams other =
        error $ Text.unpack $ "unhandled parseParams: " <> other

-------------------------------------------------------------------------------
respond :: (Aeson.ToJSON val) => Rpc.Request -> val -> Rpc.Response
respond req obj =
    Rpc.Response (req.getReqVer) (Aeson.toJSON obj) (req.getReqId)

handlerLoop :: Rpc.JSONRPCT McpStack ()
handlerLoop = do
    loop
  where
    loop :: Rpc.JSONRPCT McpStack ()
    loop = do
        mreq <- Rpc.receiveRequest
        maybe (debugString "no request received") handleReq mreq
        loop

    handleReq :: Rpc.Request -> Rpc.JSONRPCT McpStack ()
    handleReq req = do
        debugShow req
        let emsg = Rpc.fromRequest req :: Either Rpc.ErrorObj ClientMsg
        case emsg of
            (Left err) -> debugShow err
            (Right msg) -> handleParsedReq req msg

    handleParsedReq :: Rpc.Request -> ClientMsg -> Rpc.JSONRPCT McpStack ()
    handleParsedReq req msg = do
        rt <- askRuntime
        a <- async $ handleMsg req msg
        void $ liftIO $ addAsync rt req a

handleMsg :: Rpc.Request -> ClientMsg -> Rpc.JSONRPCT McpStack ()
handleMsg _ (CancelledNotificationMsg c) = do
    rt <- askRuntime
    liftIO $ cancellAsync rt c.requestId
handleMsg req (InitializeMsg _) = do
    let rsp =
            respond
                req
                (Mcp.InitializeResult serverProtocolVersion serverCapabilities serverImplem (Just ""))
    Rpc.sendResponse rsp
handleMsg _ (NotifyInitializedMsg _) =
    pure ()
handleMsg req (ListResourcesRequestMsg _) = do
    let rsp =
            respond
                req
                (Mcp.ListResourcesResult [] Nothing)
    Rpc.sendResponse rsp
handleMsg req (ListToolsRequestMsg _) = do
    toolset <- askMappedTools
    let rsp =
            respond
                req
                (Mcp.ListToolsResult (makeMappedTools toolset) Nothing)
    Rpc.sendResponse rsp
handleMsg req (ListPromptsRequestMsg _) = do
    let rsp =
            respond
                req
                (Mcp.ListPromptsResult [] Nothing)
    Rpc.sendResponse rsp
handleMsg req (CallToolRequestMsg callTool) = do
    mappedTool <- askMappedTools
    -- Get the session progress callback from runtime
    rt <- askRuntime
    let onProgress = Maybe.fromMaybe SessionBase.ignoreSessionProgress (mcpSessionProgress rt)
    res <- case lookupMappedTools mappedTool callTool.name of
        Just (ExpertAgentAsPrompt _ ai) -> do
            case extractPrompt callTool of
                Nothing -> pure $ Left "no prompt given"
                (Just query) -> do
                    liftIO $ runAgentWithQuery onProgress ai query
        Nothing -> do
            pure $ Left $ Text.unpack $ "no matching tool for " <> callTool.name
    let rsp =
            respond
                req
                (Mcp.CallToolResult [toolCallContent res] Nothing)
    Rpc.sendResponse rsp

{- | Run an agent with a query using the LLM session-based approach.
Based on the implementation in OneShot.hs and OneShotTool.hs.
-}
runAgentWithQuery :: SessionBase.OnSessionProgress -> AgentTree.AgentTree -> Text -> IO (Either String Text)
runAgentWithQuery onProgress agentTree query = do
    -- Create the agent from the runtime
    agent <- runtimeToAgent agentTree.agentRuntime

    -- Set the query on the agent with progress tracking
    let agentWithQuery =
            agentSetQuery (SessionBase.UserQuery query) $
                agentWithSessionProgress onProgress agent

    -- Create a fresh conversation ID for this execution
    convId <- newConversationId

    -- Create a fresh session with all required fields including sessionConversationId
    session0 <-
        SessionBase.Session []
            <$> SessionBase.newSessionId
            <*> pure Nothing
            <*> SessionBase.newTurnId

    -- Notify session start
    onProgress (SessionBase.SessionStarted session0)

    -- Run the agent and get the result
    (finalTurnContent, finalSession) <- run convId agentWithQuery session0

    -- Notify session completion
    onProgress (SessionBase.SessionCompleted finalSession)

    -- Extract and return the response text
    pure $ Right $ extractResponseText finalTurnContent.llmResponse

-- | Wrap an agent to emit session progress events after each step.
agentWithSessionProgress :: SessionBase.OnSessionProgress -> SessionBase.Agent r -> SessionBase.Agent r
agentWithSessionProgress onProgress agent =
    agent{SessionBase.step = decorate (SessionBase.step agent)}
  where
    decorate :: (SessionBase.Session -> IO (SessionBase.Action r)) -> (SessionBase.Session -> IO (SessionBase.Action r))
    decorate f = \sess -> do
        onProgress (SessionBase.SessionUpdated sess)
        f sess

-- | Set the user query on an agent.
agentSetQuery :: SessionBase.UserQuery -> SessionBase.Agent r -> SessionBase.Agent r
agentSetQuery query agent =
    agent{SessionBase.usrQuery = pure (Just query)}

{- | Creates an Agent from a Runtime.
Based on runtimeToAgent from OneShot.hs.

The agent is configured with the runtime's agent ID and will receive
a conversation ID at execution time. These identifiers are used to
construct the 'ToolExecutionContext' passed to tools.
-}
runtimeToAgent :: Runtime.Runtime -> IO (SessionBase.Agent (SessionBase.LlmTurnContent, SessionBase.Session))
runtimeToAgent rt = do
    let sPrompt = SessionBase.SystemPrompt rt.agentModel.modelSystemPrompt.getSystemPrompt
    let sTools = fmap toolRegistrationToSystemTool <$> rt.agentTools
    stepId <- newStepId

    -- Create a conversation ID for tracer setup
    -- Note: This tracer setup will be called for each new conversation
    convId <- newConversationId

    -- Create OpenAI completion config from runtime
    let completionConfig =
            OpenAICompletionConfig
                { cfgTracer = contramap (Runtime.AgentTrace_Conversation rt.agentSlug rt.agentId convId . (Runtime.LLMTrace stepId)) rt.agentTracer
                , cfgRuntime = rt.agentAuthenticatedHttpClientRuntime
                , cfgBaseUrl = rt.agentModel.modelBaseUrl
                , cfgModelName = rt.agentModel.modelName
                , cfgModelFlavor = rt.agentModel.modelFlavor
                }
    let completeF = mkOpenAICompletion completionConfig

    pure $
        SessionBase.Agent
            { SessionBase.step = naiveTilNoToolCallStep
            , SessionBase.sysPrompt = pure sPrompt
            , SessionBase.sysTools = sTools
            , SessionBase.usrQuery = pure Nothing
            , SessionBase.toolCall = executeToolCall rt.agentId rt.agentTools
            , SessionBase.complete = completeF
            , SessionBase.contextConfig = SessionBase.defaultContextConfig
            }

{- | Execute a tool call using the runtime's registered tools.
Based on executeToolCall from OneShot.hs.

Constructs a 'ToolExecutionContext' with the agent's ID and the identifiers
available at the point of execution. The context gives tools access to session
metadata.
-}
executeToolCall ::
    -- | Agent ID for context
    AgentId ->
    -- | Tool registrations
    IO [ToolRegistration] ->
    -- | Context from runStepM
    ToolExecutionContext ->
    -- | Tool call from LLM
    SessionBase.LlmToolCall ->
    IO SessionBase.UserToolResponse
executeToolCall agentId0 registrations _ctx (SessionBase.LlmToolCall callVal) =
    -- Extract the tool call ID and function info from the LlmToolCall
    case parseLlmToolCall callVal of
        Nothing -> pure $ SessionBase.UserToolResponse $ Aeson.String "Failed to parse tool call"
        Just tc -> do
            regs <- registrations
            -- Construct context for this tool execution
            -- We generate a minimal context here since we don't have direct access
            -- to the session from this point. The AgentId is included.
            sessId <- SessionBase.newSessionId
            convId <- newConversationId
            tId <- SessionBase.newTurnId
            let toolCtx =
                    mkToolExecutionContext
                        sessId
                        convId
                        tId
                        (Just agentId0)
                        Nothing -- No full session available at this point
                        [CallStackEntry "root" convId 0] -- Root call stack entry
                        Nothing -- No max recursion depth by default
            result <- llmCallTool regs toolCtx tc
            pure $ callResultToUserToolResponse tc result

{- | Parse an LlmToolCall into OpenAI's ToolCall format.
Based on parseLlmToolCall from OneShot.hs.
-}
parseLlmToolCall :: Aeson.Value -> Maybe OpenAI.ToolCall
parseLlmToolCall val =
    case AesonTypes.parseMaybe Aeson.parseJSON val of
        Just tc -> Just tc
        Nothing ->
            -- Try to extract from our LlmToolCall format
            case val of
                Aeson.Object obj ->
                    case (KeyMap.lookup "id" obj, KeyMap.lookup "function" obj) of
                        (Just (Aeson.String tid), Just funcVal) ->
                            Just $
                                OpenAI.ToolCall
                                    { OpenAI.rawToolCall = obj
                                    , OpenAI.toolCallId = tid
                                    , OpenAI.toolCallType = KeyMap.lookup "type" obj >>= \v -> case v of Aeson.String t -> Just t; _ -> Nothing
                                    , OpenAI.toolCallFunction = case AesonTypes.parseMaybe Aeson.parseJSON funcVal of
                                        Just f -> f
                                        Nothing -> OpenAI.ToolCallFunction (OpenAI.ToolName "") "" Nothing
                                    }
                        _ -> Nothing
                _ -> Nothing

{- | Execute a single tool call against registered tools.
Based on llmCallTool from OneShot.hs.

The 'ToolExecutionContext' is passed directly to the tool's 'toolRun' function,
providing tools with access to session metadata.
-}
llmCallTool :: [ToolRegistration] -> ToolExecutionContext -> OpenAI.ToolCall -> IO (CallResult OpenAI.ToolCall)
llmCallTool registrations ctx call =
    let
        script =
            Maybe.listToMaybe $
                Maybe.mapMaybe (\r -> r.findTool call) registrations
        args = call.toolCallFunction.toolCallFunctionArgs
        spec = (,) <$> script <*> args
     in
        case spec of
            Nothing -> pure $ ToolNotFound call
            Just (t, v) -> do
                ret <- t.toolRun (Tracer $ const $ pure ()) ctx v
                pure $ mapCallResult (const call) ret

{- | Convert a CallResult to UserToolResponse.
Based on callResultToUserToolResponse from OneShot.hs.
-}
callResultToUserToolResponse :: OpenAI.ToolCall -> CallResult OpenAI.ToolCall -> SessionBase.UserToolResponse
callResultToUserToolResponse _ result =
    case result of
        ToolNotFound _ ->
            SessionBase.UserToolResponse $ Aeson.String "Tool not found"
        BashToolError _ err ->
            SessionBase.UserToolResponse $ Aeson.String $ Text.pack $ show err
        IOToolError _ err ->
            SessionBase.UserToolResponse $ Aeson.String $ Text.pack $ show err
        McpToolError _ err ->
            SessionBase.UserToolResponse $ Aeson.String $ Text.unlines ["tool-error", Text.pack $ show err]
        McpToolResult _ res ->
            SessionBase.UserToolResponse $ Aeson.toJSON res
        BlobToolSuccess _ v ->
            SessionBase.UserToolResponse $ Aeson.String (TextEncoding.decodeUtf8 v)
        OpenAPIToolResult _ toolResult ->
            SessionBase.UserToolResponse $ Aeson.toJSON toolResult
        OpenAPIToolError _ err ->
            SessionBase.UserToolResponse $ Aeson.String $ Text.pack $ "OpenAPI tool error: " <> err
        PostgRESToolResult _ toolResult ->
            SessionBase.UserToolResponse $ Aeson.toJSON toolResult
        PostgRESToolError _ err ->
            SessionBase.UserToolResponse $ Aeson.String $ Text.pack $ "PostgREST tool error: " <> err

{- | Convert a ToolRegistration to a SystemTool for the Session agent.
Based on toolRegistrationToSystemTool from OneShot.hs.
-}
toolRegistrationToSystemTool :: ToolRegistration -> SessionBase.SystemTool
toolRegistrationToSystemTool reg =
    let llmTool = reg.declareTool
        toolDefv1 =
            SessionBase.SystemToolDefinitionV1
                { SessionBase.name = llmTool.toolName.getToolName
                , SessionBase.llmName = llmTool.toolName.getToolName
                , SessionBase.description = llmTool.toolDescription
                , SessionBase.properties = llmTool.toolParamProperties
                , SessionBase.raw =
                    Aeson.object
                        [ "type" Aeson..= ("function" :: Text)
                        , "function"
                            Aeson..= Aeson.object
                                [ "name" Aeson..= llmTool.toolName.getToolName
                                , "description" Aeson..= llmTool.toolDescription
                                , "parameters" Aeson..= toolParamsToJson llmTool.toolParamProperties
                                ]
                        ]
                }
     in SessionBase.SystemTool $ SessionBase.V1 toolDefv1

{- | Convert tool parameters to JSON schema.
Based on toolParamsToJson from OneShot.hs.

Only properties with 'propertyRequired = True' are included in the 'required' array.
-}
toolParamsToJson :: [ParamProperty] -> Aeson.Value
toolParamsToJson props =
    Aeson.object
        [ "type" Aeson..= ("object" :: Text)
        , "properties" Aeson..= KeyMap.fromList (map paramPropertyToJson props)
        , "required" Aeson..= map propertyKey (filter propertyRequired props)
        , "additionalProperties" Aeson..= False
        ]
  where
    paramPropertyToJson :: ParamProperty -> (Aeson.Key, Aeson.Value)
    paramPropertyToJson p = (AesonKey.fromText p.propertyKey, paramTypeToJson p)

    paramTypeToJson :: ParamProperty -> Aeson.Value
    paramTypeToJson p =
        Aeson.object $
            [ "type" Aeson..= paramTypeToString p.propertyType
            , "description" Aeson..= p.propertyDescription
            ]
                ++ case p.propertyType of
                    EnumParamType xs -> ["enum" Aeson..= xs]
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

{- | Extract text content from an LLM response.
Based on extractResponseText from OneShot.hs.
-}
extractResponseText :: SessionBase.LlmResponse -> Text
extractResponseText (SessionBase.LlmResponse txt _thinking _) =
    Maybe.fromMaybe "" txt

-------------------------------------------------------------------------------

lookupMappedTools :: MappedTools -> Mcp.Name -> Maybe MappedTool
lookupMappedTools xs mcpName =
    List.find f xs
  where
    f :: MappedTool -> Bool
    f (ExpertAgentAsPrompt n _) = n == mcpName

-------------------------------------------------------------------------------

makeMappedTools :: MappedTools -> [Mcp.Tool]
makeMappedTools = Maybe.catMaybes . fmap adapt
  where
    adapt :: MappedTool -> Maybe Mcp.Tool
    adapt (ExpertAgentAsPrompt n ai) = callExpertTool n ai

callExpertTool :: Mcp.Name -> AgentTree.AgentTree -> Maybe Mcp.Tool
callExpertTool mcpName ai =
    let oai = ai.agentBase
     in Just $
            Mcp.Tool
                mcpName
                (Just oai.announce)
                ( Mcp.InputSchema
                    (Just ["prompt"])
                    ( Just $
                        Mcp.pairz
                            [ "prompt"
                                Mcp..= Mcp.object
                                    [ "type" Mcp..= ("string" :: Text)
                                    , "description" Mcp..= ("the prompt asked when calling the expert" :: Text)
                                    ]
                            ]
                    )
                )

extractPrompt :: Mcp.CallToolRequest -> Maybe Text
extractPrompt (Mcp.CallToolRequest _ Nothing) = Nothing
extractPrompt (Mcp.CallToolRequest _ (Just arg)) =
    Aeson.lookup "prompt" arg >>= f
  where
    f (Aeson.String txt) = Just txt
    f _ = Nothing

toolCallContent :: Either String Text -> Mcp.Content
toolCallContent (Left err) =
    Mcp.TextContent $ Mcp.TextContentImpl (Text.unwords ["got an error:", Text.pack err]) (Just [])
toolCallContent (Right txt) =
    Mcp.TextContent $ Mcp.TextContentImpl txt (Just [])
