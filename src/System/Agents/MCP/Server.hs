{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.MCP.Server where

import Conduit (stdinC)
import Control.Concurrent.STM (readTVarIO)
import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Control.Monad.Logger (LoggingT (..), defaultOutput, logDebugN)
import Control.Monad.Reader (runReaderT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Conduit.Combinators (sinkHandleFlush)
import Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import Formatting ((%))
import qualified Formatting as Format
import qualified Network.JSONRPC as Rpc
import Prod.Tracer (Tracer, contramap)
import UnliftIO (async, liftIO, stderr, stdout)

import qualified System.Agents.AgentTree as AgentTree
import System.Agents.Base (
    announce,
    apiKeyId,
    newConversationId,
    newStepId,
    slug,
 )
import System.Agents.MCP.Base (
    ServerFlag(..),
    Implementation(..),
 )
import qualified System.Agents.MCP.Base as Mcp
import System.Agents.MCP.Server.Runtime

-- OneShot integration imports
import qualified System.Agents.HttpClient as HttpClient
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Session.Base (
    Agent (..),
    LlmResponse (..),
    LlmTurnContent (..),
    Session (..),
    SystemPrompt (..),
    UserQuery (..),
    defaultContextConfig,
 )
import qualified System.Agents.Session.Base as SessionBase
import qualified System.Agents.Session.Compat as SessionCompat
import System.Agents.Session.Loop (run)
import System.Agents.Session.OpenAI (
    OpenAICompletionConfig (..),
    mkOpenAICompletion,
 )
import System.Agents.Session.Step (naiveTilNoToolCallStep)
import System.Agents.Session.Types (
    newSessionId,
    newTurnId,
 )
import qualified System.Agents.Session.Types as SessionTypes
import qualified System.Agents.ToolPortal as ToolPortal
import System.Agents.ToolRegistration (ToolRegistration (..))
import qualified System.Agents.ToolRegistration as ToolRegistration
import qualified System.Agents.ToolSchema as ToolSchema
import System.Agents.Tools.ExecuteToolCall (executeLlmToolCall)
import qualified System.Agents.Tools.Trace as Tools

-------------------------------------------------------------------------------
data Trace
    = ToolRegistrationTrace !ToolRegistration.Trace
    | LlmCompletionTrace !OpenAI.Trace
    | ToolPortalTrace !ToolPortal.Trace
    | ToolTrace !Tools.ToolTrace
    deriving (Show)


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

mainAgentServer :: Tracer IO Trace -> AgentTree.Props -> IO ()
mainAgentServer tracer props = do
    multiAgentsServer tracer defaultMcpServerConfig [props]

-- | Run MCP server with a custom configuration.
runMcpServerWithConfig :: Tracer IO Trace -> McpServerConfig -> [AgentTree.Props] -> IO ()
runMcpServerWithConfig tracer config props = multiAgentsServer tracer config props

multiAgentsServer :: Tracer IO Trace -> McpServerConfig -> [AgentTree.Props] -> IO ()
multiAgentsServer tracer config xs = multiAgentsServer' tracer config 0 xs []

multiAgentsServer' :: Tracer IO Trace -> McpServerConfig -> Int -> [AgentTree.Props] -> MappedTools -> IO ()
multiAgentsServer' _ _ _ [] [] = do
    print ("no agent definitions" :: Text)
multiAgentsServer' tracer config _ [] mtools = do
    rt <- initRuntime mtools
    -- Store the callback in the runtime for access during tool calls
    let rtWithCallback = rt{mcpSessionProgress = Just (mcpOnSessionProgress config)}
    runLoggingT (runReaderT (runMcpStack (mainMcp tracer)) rtWithCallback) logTrace
  where
    logTrace =
        defaultOutput stderr
multiAgentsServer' tracer config idx (props : xs) mtools = do
    -- Use OS-native agent loading
    AgentTree.withAgentTree props go
  where
    go (AgentTree.Initialized tree) = do
        let oai = AgentTree.osNodeConfig (AgentTree.osTreeRoot tree)
        let toolname = Format.format ("ask_" % Format.text % "_" % Format.left 3 '0') (LText.fromStrict oai.slug) idx
        -- Store the tree along with the API keys from props
        let tool = ExpertAgentAsPrompt (LText.toStrict toolname) tree (AgentTree.apiKeys props)
        multiAgentsServer' tracer config (succ idx) xs (tool : mtools)
    go _ = do
        print ("failed to initialize" :: Text)

mainMcp :: Tracer IO Trace -> McpStack ()
mainMcp tracer = do
    runJSONRPCT'
        Rpc.V2
        False
        (Data.Conduit.Combinators.sinkHandleFlush stdout)
        stdinC
        (handlerLoop tracer)

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

handlerLoop :: Tracer IO Trace -> Rpc.JSONRPCT McpStack ()
handlerLoop tracer = do
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
        a <- async $ handleMsg tracer req msg
        void $ liftIO $ addAsync rt req a

handleMsg :: Tracer IO Trace -> Rpc.Request -> ClientMsg -> Rpc.JSONRPCT McpStack ()
handleMsg _ _ (CancelledNotificationMsg c) = do
    rt <- askRuntime
    liftIO $ cancellAsync rt c.requestId
handleMsg _ req (InitializeMsg _) = do
    let rsp =
            respond
                req
                (Mcp.InitializeResult serverProtocolVersion serverCapabilities serverImplem (Just ""))
    Rpc.sendResponse rsp
handleMsg _ _ (NotifyInitializedMsg _) =
    pure ()
handleMsg _ req (ListResourcesRequestMsg _) = do
    let rsp =
            respond
                req
                (Mcp.ListResourcesResult [] Nothing)
    Rpc.sendResponse rsp
handleMsg _ req (ListToolsRequestMsg _) = do
    toolset <- askMappedTools
    let rsp =
            respond
                req
                (Mcp.ListToolsResult (makeMappedTools toolset) Nothing)
    Rpc.sendResponse rsp
handleMsg _ req (ListPromptsRequestMsg _) = do
    let rsp =
            respond
                req
                (Mcp.ListPromptsResult [] Nothing)
    Rpc.sendResponse rsp
handleMsg tracer req (CallToolRequestMsg callTool) = do
    mappedTool <- askMappedTools
    -- Get the session progress callback from runtime
    rt <- askRuntime
    let onProgress = Maybe.fromMaybe SessionBase.ignoreSessionProgress (mcpSessionProgress rt)
    res <- case lookupMappedTools mappedTool callTool.name of
        Just (ExpertAgentAsPrompt _ tree apiKeys) -> do
            case extractPrompt callTool of
                Nothing -> pure $ Left "no prompt given"
                (Just query) -> do
                    -- Use OS-native agent execution via one-shot integration
                    liftIO $ runAgentWithQuery tracer onProgress apiKeys tree query
        Nothing -> do
            pure $ Left $ Text.unpack $ "no matching tool for " <> callTool.name
    let rsp =
            respond
                req
                (Mcp.CallToolResult [toolCallContent res] Nothing)
    Rpc.sendResponse rsp

{- | Run an agent with a query using the LLM session-based approach.

This implementation follows the same pattern as 'runOneShotWithConfig' from
System.Agents.OneShot, adapted for use within the MCP server context.

The function:
1. Creates a minimal in-memory session store
2. Converts the OSAgentNode to an Agent using the node's configuration
3. Sets up the OpenAI completion function with proper API key resolution
4. Executes the agent loop until completion
5. Returns the response text or an error
-}
runAgentWithQuery :: Tracer IO Trace -> SessionBase.OnSessionProgress -> AgentTree.LoadedApiKeys -> AgentTree.OSAgentTree -> Text -> IO (Either String Text)
runAgentWithQuery tracer onProgress apiKeys tree query = do
    -- Generate unique identifiers for this conversation
    convId <- newConversationId
    _stepId <- newStepId

    let node = AgentTree.osTreeRoot tree
    let agentCfg = AgentTree.osNodeConfig node

    -- Get the API key for this agent
    let apiKeyIdentifier = apiKeyId agentCfg
    let mApiKey = lookupApiKey apiKeyIdentifier apiKeys

    -- Create HTTP runtime with the API key
    httpRuntime <- case mApiKey of
        Just apiKey -> HttpClient.newRuntime (HttpClient.BearerToken $ Text.decodeUtf8 $ OpenAI.revealApiKey apiKey)
        Nothing -> HttpClient.newRuntime HttpClient.NoToken

    -- Create OpenAI completion config
    let completionConfig =
            OpenAICompletionConfig
                { cfgTracer = contramap LlmCompletionTrace tracer
                , cfgRuntime = httpRuntime
                , cfgBaseUrl = OpenAI.ApiBaseUrl $ AgentTree.modelUrl agentCfg
                , cfgModelName = AgentTree.modelName agentCfg
                , cfgModelFlavor = parseModelFlavor $ AgentTree.flavor agentCfg
                }
    let completeF = mkOpenAICompletion completionConfig

    -- Build the system prompt from the agent configuration
    let sPrompt = SystemPrompt $ Text.unlines $ AgentTree.systemPrompt agentCfg

    -- Read tools from the OS-native TVar
    sTools <- fmap toolRegistrationToSystemTool <$> readTVarIO (AgentTree.osNodeTools node)

    let tp = ToolPortal.makeToolPortal (contramap ToolPortalTrace tracer) (AgentTree.osNodeTools node)
    -- Create the agent with the naive step function that stops when no tool calls remain
    let agent =
            Agent
                { step = naiveTilNoToolCallStep
                , sysPrompt = pure sPrompt
                , sysTools = pure sTools
                , usrQuery = pure (Just $ UserQuery query)
                , toolCall = executeLlmToolCall (contramap ToolRegistrationTrace tracer) (AgentTree.osNodeTools node) (SessionCompat.parseToolCallFromLlmToolCall, SessionCompat.callResultToUserToolResponse)
                , toolPortal = tp
                , complete = completeF
                , contextConfig = defaultContextConfig
                }

    -- Create initial session
    session0 <- Session [] <$> newSessionId <*> pure Nothing <*> newTurnId

    -- Notify session start
    onProgress (SessionBase.SessionStarted session0)

    -- Run the agent loop with exception handling
    result <-
        (Right <$> run convId agent session0)
            `catch` (\e -> pure $ Left $ show (e :: SomeException))

    case result of
        Left err -> do
            onProgress (SessionBase.SessionFailed session0 $ Text.pack err)
            pure $ Left err
        Right (llmTurn, _) -> do
            onProgress (SessionBase.SessionCompleted session0)
            pure $ Right $ extractResponseText llmTurn.llmResponse
  where
    -- Extract response text from LLM response, handling Nothing case
    extractResponseText :: LlmResponse -> Text
    extractResponseText (LlmResponse mtxt _thinking _ _) = Maybe.fromMaybe "" mtxt

    -- Parse model flavor from text, defaulting to OpenAIv1
    parseModelFlavor :: Text -> OpenAI.ModelFlavor
    parseModelFlavor txt = Maybe.fromMaybe OpenAI.OpenAIv1 $ OpenAI.parseFlavor txt

    -- Look up an API key by its ID from the loaded API keys
    lookupApiKey :: Text -> AgentTree.LoadedApiKeys -> Maybe OpenAI.ApiKey
    lookupApiKey keyId keys = fmap snd $ List.find ((== keyId) . fst) keys

    -- Convert ToolRegistration to SystemTool for the Session agent
    toolRegistrationToSystemTool :: ToolRegistration -> SessionTypes.SystemTool
    toolRegistrationToSystemTool reg =
        let llmTool = reg.declareTool
            toolNameText = llmTool.toolDescriptionName.getToolName
            toolDesc = llmTool.toolDescriptionText
            toolProps = llmTool.toolDescriptionParamProperties
            toolDefv1 =
                SessionTypes.SystemToolDefinitionV1
                    toolNameText
                    toolNameText
                    toolDesc
                    toolProps
                    ( Aeson.object
                        [ "type" Aeson..= ("function" :: Text)
                        , "function"
                            Aeson..= Aeson.object
                                [ "name" Aeson..= toolNameText
                                , "description" Aeson..= toolDesc
                                , "parameters" Aeson..= toolParamsToJson toolProps
                                ]
                        ]
                    )
         in SessionTypes.SystemTool $ SessionTypes.V1 toolDefv1

    -- Convert tool parameters to JSON schema
    toolParamsToJson :: [ToolSchema.ParamProperty] -> Aeson.Value
    toolParamsToJson props =
        Aeson.object
            [ "type" Aeson..= ("object" :: Text)
            , "properties"
                Aeson..= Aeson.fromList (map paramPropertyToJson props)
            , "required" Aeson..= map ToolSchema.propertyKey (filter ToolSchema.propertyRequired props)
            , "additionalProperties" Aeson..= False
            ]
      where
        paramPropertyToJson p =
            ( AesonKey.fromText $ ToolSchema.propertyKey p
            , paramTypeToJson p
            )
        paramTypeToJson p =
            Aeson.object $
                [ "type" Aeson..= paramTypeToString (ToolSchema.propertyType p)
                , "description" Aeson..= ToolSchema.propertyDescription p
                ]
                    ++ case ToolSchema.propertyType p of
                        ToolSchema.EnumParamType enumVals -> ["enum" Aeson..= enumVals]
                        _ -> []

        paramTypeToString :: ToolSchema.ParamType -> Text
        paramTypeToString ToolSchema.NullParamType = "null"
        paramTypeToString ToolSchema.StringParamType = "string"
        paramTypeToString ToolSchema.BoolParamType = "boolean"
        paramTypeToString ToolSchema.NumberParamType = "number"
        paramTypeToString (ToolSchema.EnumParamType _) = "string"
        paramTypeToString (ToolSchema.OpaqueParamType t) = t
        paramTypeToString (ToolSchema.MultipleParamType t) = t
        paramTypeToString (ToolSchema.ObjectParamType _) = "object"

-------------------------------------------------------------------------------

lookupMappedTools :: MappedTools -> Mcp.Name -> Maybe MappedTool
lookupMappedTools xs mcpName =
    List.find f xs
  where
    f :: MappedTool -> Bool
    f (ExpertAgentAsPrompt n _ _) = n == mcpName

-------------------------------------------------------------------------------

makeMappedTools :: MappedTools -> [Mcp.Tool]
makeMappedTools = Maybe.catMaybes . fmap adapt
  where
    adapt :: MappedTool -> Maybe Mcp.Tool
    adapt (ExpertAgentAsPrompt n tree _) = callExpertTool n tree

callExpertTool :: Mcp.Name -> AgentTree.OSAgentTree -> Maybe Mcp.Tool
callExpertTool mcpName tree =
    let oai = AgentTree.osNodeConfig (AgentTree.osTreeRoot tree)
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

