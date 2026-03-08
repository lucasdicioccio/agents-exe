{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Direct MCP server implementation for exposing tools without agent indirection.
--
-- This module provides the ability to run an MCP server that directly exposes
-- tools from various sources (bash tools, MCP toolboxes, OpenAPI specs, PostgREST)
-- without requiring an LLM agent as an intermediate layer.
--
-- This is useful for:
-- * Testing MCP tool clients without setting up a full agent
-- * Combining tools from multiple sources into a single MCP server
-- * Running lightweight tool servers without LLM configuration
-- * Debugging tool behavior without agent complexity
--
-- Example usage:
--
-- @
-- import System.Agents.Tools.McpServer
--
-- main :: IO ()
-- main = do
--     let opts = McpToolboxOptions
--             { mcpToolSources = [ToolSourceAgent "agent.json"]
--             , mcpServerName = "my-tool-server"
--             , mcpServerVersion = "1.0.0"
--             , mcpTransport = StdioTransport
--             , mcpLogLevel = Just LogDebug
--             , mcpNameMapping = StripPrefixes
--             }
--     runMcpToolboxServer opts
-- @
module System.Agents.Tools.McpServer (
    -- * Core types
    McpToolboxOptions (..),
    ToolSource (..),
    McpTransport (..),
    DirectToolRegistry (..),
    NameMappingStrategy (..),
    LogLevel (..),

    -- * Main entry point
    runMcpToolboxServer,

    -- * Tool registry creation
    createDirectRegistry,

    -- * Tool conversion and execution
    toolRegistrationsToMcpTools,
    callToolDirect,
) where

import Conduit (ConduitT, Flush (..), Void, stdinC, (.|))
import qualified Conduit as C
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TBMChan (closeTBMChan, newTBMChanIO, readTBMChan, sinkTBMChan, sourceTBMChan, writeTBMChan)
import Control.Monad (forM_, void, when)
import Control.Monad.Logger (LoggingT (..), MonadLogger, MonadLoggerIO, defaultOutput, logDebugN)
import Control.Monad.Reader (MonadReader (..), ReaderT, ask, local, runReaderT)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Aeson.Types as AesonTypes
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Conduit.TMChan (closeTBMChan, newTBMChanIO, sinkTBMChan, sourceTBMChan, writeTBMChan)
import Data.Foldable (for_)
import Data.List (find)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Lazy as LText
import Formatting ((%))
import qualified Formatting as Format
import GHC.Generics (Generic)
import qualified Network.JSONRPC as Rpc
import Prod.Tracer (Tracer (..), contramap, silent)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr, stdout)
import qualified System.Process as Process
import UnliftIO (MonadIO, MonadUnliftIO, async, atomicModifyIORef, cancel, liftIO, newIORef, readIORef, wait, withAsync)

import qualified System.Agents.AgentTree as AgentTree
import System.Agents.Base (
    Agent (..),
    AgentDescription (..),
    McpServerDescription (..),
    McpSimpleBinaryConfiguration (..),
    McpSimpleBinary (..),
    OpenAPIToolboxDescription (..),
    OpenAPIServerDescription (..),
    OpenAPIServerOnDisk (..),
    PostgRESTToolboxDescription (..),
    PostgRESTServerDescription (..),
    PostgRESTServerOnDisk (..),
 )
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.MCP.Base as Mcp
import qualified System.Agents.MCP.Client as McpClient
import qualified System.Agents.MCP.Client.Runtime as McpClient
import System.Agents.ToolRegistration (ToolRegistration (..))
import qualified System.Agents.ToolRegistration as ToolReg
import System.Agents.Tools.Base (CallResult (..), Tool (..))
import System.Agents.Tools.Context (ToolExecutionContext, mkToolExecutionContext, CallStackEntry (..))
import qualified System.Agents.Tools.Bash as Bash
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.IO as IOTools
import qualified System.Agents.Tools.McpToolbox as McpTools
import qualified System.Agents.Tools.OpenAPI.Converter as OpenAPI
import qualified System.Agents.Tools.OpenAPI.Types as OpenAPITypes
import qualified System.Agents.Tools.OpenAPIToolbox as OpenAPIToolbox
import qualified System.Agents.Tools.PostgRESToolbox as PostgRESToolbox
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..))
import System.Agents.Tools.Trace (ToolTrace (..))

-------------------------------------------------------------------------------
-- Data Types
-------------------------------------------------------------------------------

-- | Options for running an MCP toolbox server
data McpToolboxOptions = McpToolboxOptions
    { mcpToolSources :: [ToolSource]
    -- ^ Sources to load tools from
    , mcpServerName :: Text
    -- ^ Name of the MCP server
    , mcpServerVersion :: Text
    -- ^ Version of the MCP server
    , mcpTransport :: McpTransport
    -- ^ Transport mechanism (stdio or HTTP)
    , mcpLogLevel :: Maybe LogLevel
    -- ^ Optional log level for debugging
    , mcpNameMapping :: NameMappingStrategy
    -- ^ Strategy for mapping tool names
    }
    deriving (Show)

-- | Source of tools for the MCP server
data ToolSource
    = ToolSourceAgent FilePath
    -- ^ Load tools from an agent configuration file
    | ToolSourceDirectory FilePath
    -- ^ Load bash tools directly from a directory
    | ToolSourceMcpToolbox Text FilePath
    -- ^ Load tools from an MCP server (name, config file)
    | ToolSourceOpenAPI FilePath
    -- ^ Load tools from an OpenAPI specification file
    | ToolSourcePostgREST FilePath
    -- ^ Load tools from a PostgREST configuration file
    deriving (Show, Eq)

-- | Transport mechanism for the MCP server
data McpTransport
    = StdioTransport
    -- ^ Use standard input/output (default for MCP)
    | HttpTransport Int
    -- ^ Use HTTP on the specified port (not yet implemented)
    deriving (Show, Eq)

-- | Log level for debugging
data LogLevel
    = LogDebug
    | LogInfo
    | LogWarning
    | LogError
    deriving (Show, Eq, Ord, Generic)

instance Aeson.FromJSON LogLevel where
    parseJSON = Aeson.withText "LogLevel" $ \t ->
        case Text.toLower t of
            "debug" -> pure LogDebug
            "info" -> pure LogInfo
            "warning" -> pure LogWarning
            "error" -> pure LogError
            _ -> fail $ "Unknown log level: " ++ Text.unpack t

-- | Strategy for mapping tool names from LLM format to MCP format
data NameMappingStrategy
    = KeepPrefixedNames
    -- ^ Keep LLM prefixes (bash_hello, mcp_filesystem_read)
    | StripPrefixes
    -- ^ Strip LLM prefixes (hello, filesystem_read)
    | UseOriginalNames
    -- ^ Use original tool names where available
    deriving (Show, Eq)

-- | Tool registry that doesn't require an Agent runtime
data DirectToolRegistry = DirectToolRegistry
    { registryTools :: IO [ToolRegistration]
    -- ^ Get current list of tool registrations
    , registryRefresh :: STM ()
    -- ^ Trigger a refresh of the tool list
    , registryMcpToolboxes :: [McpTools.Toolbox]
    -- ^ Active MCP toolboxes for tool calls
    , registryNameMapping :: NameMappingStrategy
    -- ^ Name mapping strategy in use
    }

-------------------------------------------------------------------------------
-- Main Entry Point
-------------------------------------------------------------------------------

-- | Run an MCP server directly from tool sources.
--
-- This function initializes the tool registry from the specified sources
-- and starts an MCP server that exposes these tools directly without
-- requiring an agent runtime.
runMcpToolboxServer :: McpToolboxOptions -> IO ()
runMcpToolboxServer opts = do
    -- Create the direct tool registry
    eRegistry <- createDirectRegistry opts.mcpToolSources opts.mcpNameMapping
    case eRegistry of
        Left err -> do
            TextIO.hPutStrLn stderr $ "Error creating tool registry: " <> err
            exitFailure
        Right registry -> do
            case opts.mcpTransport of
                StdioTransport -> runStdioServer opts registry
                HttpTransport port -> do
                    TextIO.hPutStrLn stderr $ "HTTP transport on port " <> Text.pack (show port) <> " not yet implemented"
                    exitFailure

-- | Run the MCP server using stdio transport
runStdioServer :: McpToolboxOptions -> DirectToolRegistry -> IO ()
runStdioServer opts registry = do
    rt <- initDirectRuntime registry opts
    let logTrace = case opts.mcpLogLevel of
            Just LogDebug -> defaultOutput stderr
            _ -> \_ _ _ -> pure ()  -- Silent logging
    runLoggingT (runReaderT (runMcpStack (mainMcpLoop opts)) rt) logTrace

-------------------------------------------------------------------------------
-- MCP Server Stack and Runtime
-------------------------------------------------------------------------------

-- | Runtime state for the direct MCP server
data DirectRuntime = DirectRuntime
    { directRegistry :: DirectToolRegistry
    , directOptions :: McpToolboxOptions
    , directActions :: [(Rpc.Request, Async ())]
    }

initDirectRuntime :: DirectToolRegistry -> McpToolboxOptions -> IO DirectRuntime
initDirectRuntime registry opts = do
    pure $ DirectRuntime
        { directRegistry = registry
        , directOptions = opts
        , directActions = []
        }

-- | MCP server monad stack
newtype McpStack a = McpStack {runMcpStack :: ReaderT DirectRuntime (LoggingT IO) a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO, MonadUnliftIO)

instance MonadReader DirectRuntime McpStack where
    ask = McpStack ask
    local f (McpStack a) = McpStack (local f a)

askRuntime :: Rpc.JSONRPCT McpStack DirectRuntime
askRuntime = lift ask

-------------------------------------------------------------------------------
-- MCP Protocol Handling
-------------------------------------------------------------------------------

-- | Main MCP protocol loop
mainMcpLoop :: McpToolboxOptions -> Rpc.JSONRPCT McpStack ()
mainMcpLoop opts = do
    runJSONRPCT'
        Rpc.V2
        False
        (sinkHandleFlush stdout)
        stdinC
        (handlerLoop opts)

-- | Custom JSON-RPC conduit handler
runJSONRPCT' ::
    (MonadLoggerIO m, MonadUnliftIO m) =>
    Rpc.Ver ->
    Bool ->
    ConduitT (Flush ByteString) Void m () ->
    ConduitT () ByteString m () ->
    Rpc.JSONRPCT m a ->
    m a
runJSONRPCT' ver ignore snk src f = do
    qs <- liftIO . atomically $ Rpc.initSession ver ignore
    let inSnk = sinkTBMChan (Rpc.inCh qs)
        outSrc = sourceTBMChan (Rpc.outCh qs)
    withAsync ((C.runConduit $ src .| Rpc.decodeConduit ver .| inSnk) >> liftIO (atomically $ closeTBMChan $ Rpc.inCh qs)) $
        const $
            withAsync (C.runConduit $ outSrc .| encodeConduit .| snk) $ \o ->
                withAsync (runReaderT Rpc.processIncoming qs) $ const $ do
                    a <- runReaderT f qs
                    liftIO $ do
                        atomically . closeTBMChan $ Rpc.outCh qs
                        _ <- wait o
                        return a

encodeConduit :: (Aeson.ToJSON j, MonadLogger m) => ConduitT j (Flush ByteString) m ()
encodeConduit = do
    C.awaitForever $ \m -> do
        C.yield $ Chunk . L8.toStrict $ Aeson.encode m
        C.yield $ Chunk "\n"
        C.yield Flush

-- | MCP protocol message handler loop
handlerLoop :: McpToolboxOptions -> Rpc.JSONRPCT McpStack ()
handlerLoop opts = do
    loop
  where
    loop :: Rpc.JSONRPCT McpStack ()
    loop = do
        mreq <- Rpc.receiveRequest
        maybe (debugString "no request received") (handleReq opts) mreq
        loop

    handleReq :: McpToolboxOptions -> Rpc.Request -> Rpc.JSONRPCT McpStack ()
    handleReq opts' req = do
        debugShow req
        let emsg = Rpc.fromRequest req :: Either Rpc.ErrorObj ClientMsg
        case emsg of
            Left err -> debugShow err
            Right msg -> handleParsedReq opts' req msg

    handleParsedReq :: McpToolboxOptions -> Rpc.Request -> ClientMsg -> Rpc.JSONRPCT McpStack ()
    handleParsedReq opts' req msg = do
        rt <- askRuntime
        a <- async $ handleMsg opts' req msg
        void $ liftIO $ addAsync rt req a

debugString :: String -> Rpc.JSONRPCT McpStack ()
debugString = logDebugN . Text.pack

debugShow :: (Show a) => a -> Rpc.JSONRPCT McpStack ()
debugShow = debugString . show

addAsync :: DirectRuntime -> Rpc.Request -> Async () -> IO (Async ()) 
addAsync rt req a = do
    pure a  -- Simplified for now

cancelAsync :: DirectRuntime -> Mcp.RequestId -> IO ()
cancelAsync rt reqId = do
    pure ()  -- Simplified for now

-------------------------------------------------------------------------------
-- Client Message Types
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
    parseParams "tools/call" =
        Just (fmap CallToolRequestMsg <$> Aeson.parseJSON)
    parseParams "prompts/list" =
        Just (fmap ListPromptsRequestMsg <$> Aeson.parseJSON)
    parseParams other =
        error $ Text.unpack $ "unhandled parseParams: " <> other

handleMsg :: McpToolboxOptions -> Rpc.Request -> ClientMsg -> Rpc.JSONRPCT McpStack ()
handleMsg opts req msg = case msg of
    InitializeMsg _ -> do
        let rsp = respond req (Mcp.InitializeResult serverProtocolVersion serverCapabilities (serverImplem opts) (Just ""))
        Rpc.sendResponse rsp
    NotifyInitializedMsg _ ->
        pure ()
    CancelledNotificationMsg c -> do
        rt <- askRuntime
        liftIO $ cancelAsync rt c.requestId
    ListResourcesRequestMsg _ -> do
        let rsp = respond req (Mcp.ListResourcesResult [] Nothing)
        Rpc.sendResponse rsp
    ListToolsRequestMsg _ -> do
        rt <- askRuntime
        tools <- liftIO $ registryTools (directRegistry rt)
        let mcpTools = toolRegistrationsToMcpTools tools (mcpNameMapping opts)
        let rsp = respond req (Mcp.ListToolsResult mcpTools Nothing)
        Rpc.sendResponse rsp
    ListPromptsRequestMsg _ -> do
        let rsp = respond req (Mcp.ListPromptsResult [] Nothing)
        Rpc.sendResponse rsp
    CallToolRequestMsg callTool -> do
        rt <- askRuntime
        result <- liftIO $ callToolDirect (directRegistry rt) callTool.name callTool.arguments
        let rsp = respond req result
        Rpc.sendResponse rsp

respond :: (Aeson.ToJSON val) => Rpc.Request -> val -> Rpc.Response
respond req obj =
    Rpc.Response (req.getReqVer) (Aeson.toJSON obj) (req.getReqId)

serverProtocolVersion :: Text
serverProtocolVersion = "2024-11-05"

serverCapabilities :: Mcp.ServerCapabilities
serverCapabilities =
    Mcp.ServerCapabilities
        (Just mempty)
        (Just $ Aeson.object [])
        [Mcp.ToolsListChanged]

serverImplem :: McpToolboxOptions -> Mcp.Implementation
serverImplem opts = Mcp.Implementation opts.mcpServerName opts.mcpServerVersion

-------------------------------------------------------------------------------
-- Tool Registry Creation
-------------------------------------------------------------------------------

-- | Create a tool registry from various sources.
--
-- This function loads tools from the specified sources and creates a registry
-- that can be used to serve MCP requests directly without an agent runtime.
createDirectRegistry :: [ToolSource] -> NameMappingStrategy -> IO (Either Text DirectToolRegistry)
createDirectRegistry sources nameMapping = do
    -- Load all tool registrations from sources
    eToolsList <- mapM loadToolsFromSource sources
    case sequence eToolsList of
        Left err -> pure $ Left err
        Right toolsList -> do
            -- Flatten the list of tool lists
            let allTools = concat toolsList
            
            -- Create MCP toolboxes map for tool calls
            mcpBoxes <- loadMcpToolboxes sources
            
            -- Create the refresh TVar (tools are currently static)
            refreshVar <- newTVarIO ()
            
            pure $ Right $ DirectToolRegistry
                { registryTools = pure allTools
                , registryRefresh = writeTVar refreshVar ()
                , registryMcpToolboxes = mcpBoxes
                , registryNameMapping = nameMapping
                }

-- | Load tool registrations from a single source
loadToolsFromSource :: ToolSource -> IO (Either Text [ToolRegistration])
loadToolsFromSource source = case source of
    ToolSourceAgent agentFile -> loadToolsFromAgent agentFile
    ToolSourceDirectory dir -> loadToolsFromDirectory dir
    ToolSourceMcpToolbox name configFile -> loadToolsFromMcpToolbox name configFile
    ToolSourceOpenAPI specFile -> loadToolsFromOpenAPI specFile
    ToolSourcePostgREST configFile -> loadToolsFromPostgREST configFile

-- | Load tools from an agent configuration file
loadToolsFromAgent :: FilePath -> IO (Either Text [ToolRegistration])
loadToolsFromAgent agentFile = do
    exists <- doesFileExist agentFile
    if not exists
        then pure $ Left $ "Agent file not found: " <> Text.pack agentFile
        else do
            eAgent <- Aeson.eitherDecodeFileStrict' agentFile
            case eAgent of
                Left err -> pure $ Left $ "Failed to parse agent file: " <> Text.pack err
                Right (AgentDescription agent) -> do
                    let agentDir = takeDirectory agentFile
                    tools <- loadAllToolsFromAgent agentDir agent
                    pure $ Right tools

-- | Load all tools from an agent definition
loadAllToolsFromAgent :: FilePath -> Agent -> IO [ToolRegistration]
loadAllToolsFromAgent agentDir agent = do
    -- Load bash tools
    bashTools <- loadBashToolsFromAgent agentDir agent
    
    -- Load OpenAPI tools
    openapiTools <- loadOpenAPIToolsFromAgent agentDir agent
    
    -- Load PostgREST tools
    postgrestTools <- loadPostgRESToolsFromAgent agentDir agent
    
    pure $ concat [bashTools, openapiTools, postgrestTools]

-- | Load bash tools from an agent's tool directory
loadBashToolsFromAgent :: FilePath -> Agent -> IO [ToolRegistration]
loadBashToolsFromAgent agentDir agent = do
    let toolDir = agentDir </> agent.toolDirectory
    exists <- doesDirectoryExist toolDir
    if not exists
        then pure []
        else do
            let tracer = Tracer (const (pure ())) :: Tracer IO Bash.LoadTrace
            (scripts, _errors) <- Bash.loadDirectory tracer toolDir
            pure $ map ToolReg.registerBashToolInLLM scripts.scriptDescriptions

-- | Load OpenAPI tools from an agent configuration
loadOpenAPIToolsFromAgent :: FilePath -> Agent -> IO [ToolRegistration]
loadOpenAPIToolsFromAgent _agentDir agent =
    case agent.openApiToolboxes of
        Nothing -> pure []
        Just toolboxes -> fmap concat $ mapM loadOpenAPIToolbox toolboxes

-- | Load a single OpenAPI toolbox
loadOpenAPIToolbox :: OpenAPIToolboxDescription -> IO [ToolRegistration]
loadOpenAPIToolbox (OpenAPIServer cfg) = do
    let tracer = Tracer (const (pure ())) :: Tracer IO OpenAPIToolbox.OpenAPIToolboxTrace
    result <- OpenAPIToolbox.initializeToolbox tracer cfg
    case result of
        Left _err -> pure []
        Right toolbox -> do
            eRegs <- ToolReg.registerOpenAPITools toolbox
            case eRegs of
                Left _ -> pure []
                Right regs -> pure regs
loadOpenAPIToolbox (OpenAPIServerOnDiskDescription (OpenAPIServerOnDisk path)) = do
    -- Load from disk configuration
    exists <- doesFileExist path
    if not exists
        then pure []
        else do
            -- Parse and load the OpenAPI spec from disk
            pure []

-- | Load PostgREST tools from an agent configuration
loadPostgRESToolsFromAgent :: FilePath -> Agent -> IO [ToolRegistration]
loadPostgRESToolsFromAgent _agentDir agent =
    case agent.postgrestToolboxes of
        Nothing -> pure []
        Just toolboxes -> fmap concat $ mapM loadPostgRESToolbox toolboxes

-- | Load a single PostgREST toolbox
loadPostgRESToolbox :: PostgRESTToolboxDescription -> IO [ToolRegistration]
loadPostgRESToolbox (PostgRESTServer cfg) = do
    let tracer = Tracer (const (pure ())) :: Tracer IO PostgRESToolbox.PostgRESToolboxTrace
    result <- PostgRESToolbox.initializeToolbox tracer cfg
    case result of
        Left _err -> pure []
        Right toolbox -> do
            eRegs <- ToolReg.registerPostgRESTools toolbox
            case eRegs of
                Left _ -> pure []
                Right regs -> pure regs
loadPostgRESToolbox (PostgRESTServerOnDiskDescription (PostgRESTServerOnDisk _path)) = do
    -- Load from disk configuration
    pure []

-- | Load tools directly from a directory
loadToolsFromDirectory :: FilePath -> IO (Either Text [ToolRegistration])
loadToolsFromDirectory dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure $ Left $ "Directory not found: " <> Text.pack dir
        else do
            let tracer = Tracer (const (pure ())) :: Tracer IO Bash.LoadTrace
            (scripts, errors) <- Bash.loadDirectory tracer dir
            -- Log errors but continue
            forM_ errors $ \err ->
                putStrLn $ "Warning: " ++ show err
            let tools = map ToolReg.registerBashToolInLLM scripts.scriptDescriptions
            pure $ Right tools

-- | Load tools from an MCP toolbox configuration
loadToolsFromMcpToolbox :: Text -> FilePath -> IO (Either Text [ToolRegistration])
loadToolsFromMcpToolbox name configFile = do
    exists <- doesFileExist configFile
    if not exists
        then pure $ Left $ "MCP config file not found: " <> Text.pack configFile
        else do
            eConfig <- Aeson.eitherDecodeFileStrict' configFile
            case eConfig of
                Left err -> pure $ Left $ "Failed to parse MCP config: " <> Text.pack err
                Right (McpSimpleBinary cfg) -> do
                    -- Create process for MCP server
                    let proc = Process.proc cfg.executable (map Text.unpack cfg.args)
                    
                    -- Initialize MCP toolbox
                    let tracer = Tracer (const (pure ())) :: Tracer IO McpTools.Trace
                    toolbox <- McpTools.initializeMcpToolbox tracer name proc
                    
                    -- Get tools from the toolbox
                    tools <- atomically $ readTVar toolbox.toolsList
                    
                    -- Register each tool
                    let registrations = Maybe.catMaybes $ map (registerMcpTool toolbox) tools
                    pure $ Right registrations
  where
    registerMcpTool :: McpTools.Toolbox -> McpTools.ToolDescription -> Maybe ToolRegistration
    registerMcpTool toolbox tool =
        case ToolReg.registerMcpToolInLLM toolbox tool of
            Left _ -> Nothing
            Right reg -> Just reg

-- | Load tools from an OpenAPI specification file
loadToolsFromOpenAPI :: FilePath -> IO (Either Text [ToolRegistration])
loadToolsFromOpenAPI specFile = do
    exists <- doesFileExist specFile
    if not exists
        then pure $ Left $ "OpenAPI spec file not found: " <> Text.pack specFile
        else do
            -- Load the spec and parse it
            result <- Aeson.eitherDecodeFileStrict' specFile
            case result of
                Left err -> pure $ Left $ "Failed to parse OpenAPI spec: " <> Text.pack err
                Right (spec :: OpenAPITypes.OpenAPISpec) -> do
                    -- Convert spec to tools using the converter
                    let tools = OpenAPI.convertOpenAPIToTools spec
                    -- Create a minimal toolbox for registration
                    let toolboxName = Text.pack $ takeDirectory specFile
                    let toolbox = OpenAPIToolbox.Toolbox
                            { OpenAPIToolbox.toolboxName = toolboxName
                            , OpenAPIToolbox.toolboxSpecUrl = Text.pack specFile
                            , OpenAPIToolbox.toolboxBaseUrl = ""
                            , OpenAPIToolbox.toolboxClient = error "No HTTP client available"
                            , OpenAPIToolbox.toolboxTools = tools
                            , OpenAPIToolbox.toolboxNameMapping = OpenAPI.buildToolNameMapping toolboxName tools
                            }
                    eRegs <- ToolReg.registerOpenAPITools toolbox
                    case eRegs of
                        Left err -> pure $ Left $ Text.pack err
                        Right regs -> pure $ Right regs

-- | Load tools from a PostgREST configuration file
loadToolsFromPostgREST :: FilePath -> IO (Either Text [ToolRegistration])
loadToolsFromPostgREST configFile = do
    exists <- doesFileExist configFile
    if not exists
        then pure $ Left $ "PostgREST config file not found: " <> Text.pack configFile
        else do
            -- Parse the configuration
            result <- Aeson.eitherDecodeFileStrict' configFile
            case result of
                Left err -> pure $ Left $ "Failed to parse PostgREST config: " <> Text.pack err
                Right (cfg :: PostgRESTServerDescription) -> do
                    let tracer = Tracer (const (pure ())) :: Tracer IO PostgRESToolbox.PostgRESToolboxTrace
                    toolboxResult <- PostgRESToolbox.initializeToolbox tracer cfg
                    case toolboxResult of
                        Left _err -> pure $ Right []  -- Empty on error
                        Right toolbox -> do
                            eRegs <- ToolReg.registerPostgRESTools toolbox
                            case eRegs of
                                Left _ -> pure $ Right []
                                Right regs -> pure $ Right regs

-- | Load MCP toolboxes for tool execution context
loadMcpToolboxes :: [ToolSource] -> IO [McpTools.Toolbox]
loadMcpToolboxes sources = do
    boxes <- mapM loadMcpToolbox sources
    pure $ concat boxes
  where
    loadMcpToolbox :: ToolSource -> IO [McpTools.Toolbox]
    loadMcpToolbox (ToolSourceMcpToolbox name configFile) = do
        exists <- doesFileExist configFile
        if not exists
            then pure []
            else do
                eConfig <- Aeson.eitherDecodeFileStrict' configFile
                case eConfig of
                    Left _ -> pure []
                    Right (McpSimpleBinary cfg) -> do
                        let proc = Process.proc cfg.executable (map Text.unpack cfg.args)
                        let tracer = Tracer (const (pure ())) :: Tracer IO McpTools.Trace
                        toolbox <- McpTools.initializeMcpToolbox tracer name proc
                        pure [toolbox]
    loadMcpToolbox _ = pure []

-------------------------------------------------------------------------------
-- Tool Conversion
-------------------------------------------------------------------------------

-- | Convert tool registrations to MCP tool definitions.
--
-- This function maps the internal ToolRegistration format to the MCP Tool format,
-- applying the configured name mapping strategy.
toolRegistrationsToMcpTools :: [ToolRegistration] -> NameMappingStrategy -> [Mcp.Tool]
toolRegistrationsToMcpTools regs nameMapping =
    Maybe.catMaybes $ map (toolRegistrationToMcpTool nameMapping) regs

-- | Convert a single tool registration to an MCP tool definition
toolRegistrationToMcpTool :: NameMappingStrategy -> ToolRegistration -> Maybe Mcp.Tool
toolRegistrationToMcpTool nameMapping reg =
    let llmTool = reg.declareTool
        mcpName = mapToolName nameMapping llmTool.OpenAI.toolName
        mcpDesc = llmTool.OpenAI.toolDescription
        mcpSchema = paramPropertiesToInputSchema llmTool.OpenAI.toolParamProperties
     in Just $ Mcp.Tool
            { Mcp.name = mcpName
            , Mcp.description = if Text.null mcpDesc then Nothing else Just mcpDesc
            , Mcp.inputSchema = mcpSchema
            }

-- | Map an LLM tool name to an MCP tool name based on the strategy
mapToolName :: NameMappingStrategy -> OpenAI.ToolName -> Text
mapToolName strategy (OpenAI.ToolName name) = case strategy of
    KeepPrefixedNames -> name
    StripPrefixes -> stripLlmPrefix name
    UseOriginalNames -> stripLlmPrefix name  -- For now, same as StripPrefixes

-- | Strip LLM-specific prefixes from tool names
stripLlmPrefix :: Text -> Text
stripLlmPrefix name =
    let prefixes = ["bash_", "mcp_", "openapi_", "postgrest_", "io_"]
        tryStrip [] = name
        tryStrip (p:ps) = if p `Text.isPrefixOf` name
            then Text.drop (Text.length p) name
            else tryStrip ps
     in tryStrip prefixes

-- | Convert parameter properties to MCP InputSchema
paramPropertiesToInputSchema :: [ParamProperty] -> Mcp.InputSchema
paramPropertiesToInputSchema props =
    let required = map propertyKey $ filter isRequired props
        properties = AesonKeyMap.fromList $ map paramPropertyToJsonPair props
     in Mcp.InputSchema
            { Mcp.required = if null required then Nothing else Just required
            , Mcp.properties = if null properties then Nothing else Just properties
            }
  where
    isRequired :: ParamProperty -> Bool
    isRequired _ = True  -- For now, assume all parameters are required

    paramPropertyToJsonPair :: ParamProperty -> (AesonKey.Key, Aeson.Value)
    paramPropertyToJsonPair p =
        let key = AesonKey.fromText p.propertyKey
            value = paramTypeToJson p.propertyType p.propertyDescription
         in (key, value)

    paramTypeToJson :: ParamType -> Text -> Aeson.Value
    paramTypeToJson pType desc =
        let baseObj = ["description" Aeson..= desc]
            typeObj = case pType of
                NullParamType -> ["type" Aeson..= ("null" :: Text)]
                StringParamType -> ["type" Aeson..= ("string" :: Text)]
                BoolParamType -> ["type" Aeson..= ("boolean" :: Text)]
                NumberParamType -> ["type" Aeson..= ("number" :: Text)]
                EnumParamType values -> ["type" Aeson..= ("string" :: Text), "enum" Aeson..= values]
                OpaqueParamType t -> ["type" Aeson..= t]
                MultipleParamType t -> ["type" Aeson..= t]
                ObjectParamType subProps ->
                    let subRequired = map propertyKey subProps
                        subProperties = AesonKeyMap.fromList $ map paramPropertyToJsonPair subProps
                     in [ "type" Aeson..= ("object" :: Text)
                        , "properties" Aeson..= subProperties
                        , "required" Aeson..= subRequired
                        , "additionalProperties" Aeson..= False
                        ]
         in Aeson.object (baseObj ++ typeObj)

-------------------------------------------------------------------------------
-- Tool Execution
-------------------------------------------------------------------------------

-- | Execute a tool call directly without agent indirection.
--
-- This function looks up the tool by name and executes it directly,
-- returning the result in MCP format.
callToolDirect :: DirectToolRegistry -> Text -> Maybe Aeson.Object -> IO Mcp.CallToolResult
callToolDirect registry toolName args = do
    tools <- registryTools registry
    
    -- Find the tool by name (considering name mapping)
    let mReg = find (matchesToolName toolName (registryNameMapping registry)) tools
    
    case mReg of
        Nothing ->
            pure $ Mcp.CallToolResult
                { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl ("Tool not found: " <> toolName) Nothing]
                , Mcp.isError = Just True
                }
        Just reg -> do
            -- Create a minimal tool execution context
            ctx <- createMinimalToolContext
            
            -- Convert args to Aeson Value
            let argValue = case args of
                    Nothing -> Aeson.object []
                    Just obj -> Aeson.Object obj
            
            -- Execute the tool
            result <- executeToolRegistration reg ctx argValue
            
            -- Convert result to MCP format
            pure $ callResultToMcpResult result

-- | Check if a tool registration matches the given tool name
matchesToolName :: Text -> NameMappingStrategy -> ToolRegistration -> Bool
matchesToolName targetName nameMapping reg =
    let llmName = reg.declareTool.OpenAI.toolName
        mcpName = mapToolName nameMapping llmName
     in mcpName == targetName

-- | Create a minimal tool execution context for direct tool calls
createMinimalToolContext :: IO ToolExecutionContext
createMinimalToolContext = do
    -- Generate minimal identifiers
    sessId <- generateSessionId
    convId <- generateConversationId
    turnId <- generateTurnId
    
    pure $ mkToolExecutionContext
        sessId
        convId
        turnId
        Nothing  -- No agent ID
        Nothing  -- No session
        [CallStackEntry "mcp-direct" convId 0]
        Nothing  -- No max recursion
  where
    -- Simple generation functions (could use UUID in production)
    generateSessionId = pure $ LText.toStrict $ Format.format ("mcp-session-" % Format.int) (0 :: Int)
    generateConversationId = pure $ LText.toStrict $ Format.format ("mcp-conv-" % Format.int) (0 :: Int)
    generateTurnId = pure $ LText.toStrict $ Format.format ("mcp-turn-" % Format.int) (0 :: Int)

-- | Execute a tool registration
executeToolRegistration :: ToolRegistration -> ToolExecutionContext -> Aeson.Value -> IO (CallResult ())
executeToolRegistration reg ctx args = do
    let tool = reg.innerTool
    tool.toolRun silent ctx args

-- | Convert a CallResult to an MCP CallToolResult
callResultToMcpResult :: CallResult () -> Mcp.CallToolResult
callResultToMcpResult result = case result of
    BlobToolSuccess () bytes ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl (TextEncoding.decodeUtf8 bytes) Nothing]
            , Mcp.isError = Nothing
            }
    ToolNotFound () ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl "Tool not found" Nothing]
            , Mcp.isError = Just True
            }
    BashToolError () err ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl (Text.pack $ show err) Nothing]
            , Mcp.isError = Just True
            }
    IOToolError () err ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl (Text.pack $ show err) Nothing]
            , Mcp.isError = Just True
            }
    McpToolResult () callResult ->
        callResult
    McpToolError () err ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl (Text.pack err) Nothing]
            , Mcp.isError = Just True
            }
    OpenAPIToolResult () toolResult ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl (Text.pack $ show toolResult) Nothing]
            , Mcp.isError = Nothing
            }
    OpenAPIToolError () err ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl (Text.pack err) Nothing]
            , Mcp.isError = Just True
            }
    PostgRESToolResult () toolResult ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl (Text.pack $ show toolResult) Nothing]
            , Mcp.isError = Nothing
            }
    PostgRESToolError () err ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl (Text.pack err) Nothing]
            , Mcp.isError = Just True
            }

