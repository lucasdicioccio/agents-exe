{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.MCP.Server where

import Conduit (stdinC)
import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Control.Monad.Logger (LoggingT (..), defaultOutput, logDebugN)
import Control.Monad.Reader (runReaderT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Conduit.Combinators (sinkHandleFlush)
import Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Formatting ((%))
import qualified Formatting as Format
import qualified Network.JSONRPC as Rpc
import Prod.Tracer (Tracer, contramap)
import UnliftIO (async, liftIO, stderr, stdout)

import qualified System.Agents.AgentTree as AgentTree
import System.Agents.Base (
    announce,
    newConversationId,
    slug,
 )
import System.Agents.MCP.Base (
    Implementation (..),
    ServerFlag (..),
 )
import qualified System.Agents.MCP.Base as Mcp
import System.Agents.MCP.Server.Runtime

-- Agent execution imports
import System.Agents.AgentFactory (AgentRole (..), buildAgent, defaultAgentDeps)
import qualified System.Agents.AgentFactory as AgentFactory
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Session.Base (
    Agent (..),
    LlmResponse (..),
    LlmTurnContent (..),
    Session (..),
    UserQuery (..),
 )
import qualified System.Agents.Session.Base as SessionBase
import System.Agents.Session.Loop (runUntilBlocked)
import System.Agents.Session.Types (
    newSessionId,
    newTurnId,
 )
import qualified System.Agents.ToolPortal as ToolPortal
import qualified System.Agents.ToolRegistration as ToolRegistration
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

{- | Run the root agent of a tree on a query, without storing sessions.

The agent is built with 'buildAgent', like in every other front-end. A turn
that waits on deferred tool calls cannot progress here: nothing stores the
session for an external worker to complete, so it is reported as an error.
-}
runAgentWithQuery :: Tracer IO Trace -> SessionBase.OnSessionProgress -> AgentTree.LoadedApiKeys -> AgentTree.OSAgentTree -> Text -> IO (Either String Text)
runAgentWithQuery tracer onProgress apiKeys tree query = do
    convId <- newConversationId
    let node = AgentTree.osTreeRoot tree
    agent0 <- buildAgent (contramap agentFactoryTrace tracer) (defaultAgentDeps apiKeys) RootAgent convId node
    let agent = agent0{usrQuery = pure (Just $ UserQuery query [])}

    -- Create initial session with media support (version 1)
    session0 <- Session [] <$> newSessionId <*> pure Nothing <*> newTurnId <*> pure (Just 1) <*> pure Nothing

    -- Notify session start
    onProgress (SessionBase.SessionStarted session0)

    -- Run the agent loop with exception handling
    result <-
        (Right <$> runUntilBlocked convId agent session0)
            `catch` (\e -> pure $ Left $ show (e :: SomeException))

    case result of
        Left err -> do
            onProgress (SessionBase.SessionFailed session0 $ Text.pack err)
            pure $ Left err
        Right (Right paused) -> do
            let err = "the agent is waiting for deferred tool calls, which the MCP server cannot complete"
            onProgress (SessionBase.SessionFailed paused $ Text.pack err)
            pure $ Left err
        Right (Left (llmTurn, _)) -> do
            onProgress (SessionBase.SessionCompleted session0)
            pure $ Right $ extractResponseText llmTurn.llmResponse
  where
    -- Extract response text from LLM response, handling Nothing case
    extractResponseText :: LlmResponse -> Text
    extractResponseText (LlmResponse mtxt _thinking _ _) = Maybe.fromMaybe "" mtxt

-- | Report agent-building traces with the server's own trace constructors.
agentFactoryTrace :: AgentFactory.Trace -> Trace
agentFactoryTrace (AgentFactory.ToolRegistrationTrace t) = ToolRegistrationTrace t
agentFactoryTrace (AgentFactory.ToolPortalTrace t) = ToolPortalTrace t
agentFactoryTrace (AgentFactory.OpenAITrace t) = LlmCompletionTrace t

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
