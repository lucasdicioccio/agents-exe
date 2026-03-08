{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tool listing functionality for introspecting available tools.
--
-- This module provides a lightweight way to list available tools from
-- various sources (agent files, tool directories, MCP servers) without
-- loading a full agent runtime.
--
-- Part of #142 - Add CLI `toolbox list` command
module System.Agents.Tools.List (
    -- * Configuration types
    ListToolsOptions (..),
    ToolSource (..),
    ListFormat (..),

    -- * Tool summary types
    ToolSummary (..),
    ToolType (..),

    -- * Main entry point
    runListCommand,

    -- * Tool listing functions
    listToolsFromAgent,
    listToolsFromDirectory,
    listToolsFromMcpConfig,

    -- * Formatters
    formatToolsTable,
    formatToolsJson,
    formatToolsNames,
) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as AesonText
import qualified Data.ByteString.Lazy as LByteString
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Lazy as LazyText
import GHC.Generics (Generic)
import qualified Network.JSONRPC as Rpc
import Prod.Tracer (Tracer (..))
import System.Directory (doesDirectoryExist)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr)
import System.Process (CreateProcess, proc)

import qualified System.Agents.Tools.Bash as BashTools
import System.Agents.Base (Agent (..), AgentDescription (..), McpServerDescription (..), McpSimpleBinaryConfiguration (..))
import qualified System.Agents.MCP.Base as McpBase
import qualified System.Agents.MCP.Client as McpClient
import qualified System.Agents.MCP.Client.Runtime as McpClient

-------------------------------------------------------------------------------
-- Configuration Types
-------------------------------------------------------------------------------

-- | Options for the list command
data ListToolsOptions = ListToolsOptions
    { listSource :: ToolSource
    , listFormat :: ListFormat
    }
    deriving (Show)

-- | Source of tools to list
data ToolSource
    = ToolSourceAgent FilePath
    -- ^ Load tools from an agent's tool directory
    | ToolSourceDirectory FilePath
    -- ^ Load tools from a specific directory
    | ToolSourceMcp FilePath
    -- ^ Load tools from an MCP server config
    deriving (Show)

-- | Output format for the list command
data ListFormat
    = FormatTable
    -- ^ Human-readable table (default)
    | FormatJson
    -- ^ JSON for scripting
    | FormatNames
    -- ^ Just tool names, one per line
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Tool Summary Types
-------------------------------------------------------------------------------

-- | Summary information about a tool
data ToolSummary = ToolSummary
    { toolName :: Text
    , toolType :: ToolType
    , toolDescription :: Text
    , toolSource :: Text
    }
    deriving (Show, Generic)

instance ToJSON ToolSummary where
    toJSON t =
        Aeson.object
            [ "name" .= t.toolName
            , "type" .= t.toolType
            , "description" .= t.toolDescription
            , "source" .= t.toolSource
            ]

-- | Type of tool
data ToolType
    = BashToolType
    | McpToolType
    | IOToolType
    | OpenAPIToolType
    | PostgRESToolType
    deriving (Show, Generic, Eq)

instance ToJSON ToolType where
    toJSON t = case t of
        BashToolType -> Aeson.String "bash"
        McpToolType -> Aeson.String "mcp"
        IOToolType -> Aeson.String "io"
        OpenAPIToolType -> Aeson.String "openapi"
        PostgRESToolType -> Aeson.String "postgrest"

-------------------------------------------------------------------------------
-- Main Entry Point
-------------------------------------------------------------------------------

-- | Main entry point for the list command
runListCommand :: ListToolsOptions -> IO ()
runListCommand opts = do
    eTools <- case listSource opts of
        ToolSourceAgent agentFile -> listToolsFromAgent agentFile
        ToolSourceDirectory dir -> listToolsFromDirectory dir
        ToolSourceMcp configFile -> listToolsFromMcpConfig configFile

    case eTools of
        Left err -> do
            TextIO.hPutStrLn stderr $ "Error: " <> err
            exitFailure
        Right tools -> do
            let formatted = case listFormat opts of
                    FormatTable -> formatToolsTable tools
                    FormatJson -> formatToolsJson tools
                    FormatNames -> formatToolsNames tools
            TextIO.putStrLn formatted

-------------------------------------------------------------------------------
-- Tool Listing Functions
-------------------------------------------------------------------------------

-- | List tools from an agent configuration file
listToolsFromAgent :: FilePath -> IO (Either Text [ToolSummary])
listToolsFromAgent agentFile = do
    -- Load the agent configuration
    eAgent <- loadAgentFromFile agentFile
    case eAgent of
        Left err -> pure $ Left err
        Right agent -> do
            let toolDir = takeDirectory agentFile </> toolDirectory agent
            dirExists <- doesDirectoryExist toolDir
            bashTools <-
                if dirExists
                    then do
                        eBashTools <- listToolsFromDirectory toolDir
                        pure $ either (const []) id eBashTools
                    else pure []
            -- For now, we only list bash tools from agent files
            -- MCP, OpenAPI, and PostgREST tools require runtime initialization
            -- which is beyond the scope of a simple listing
            pure $ Right $ sortOn toolName bashTools

-- | List tools from a directory of executable scripts
listToolsFromDirectory :: FilePath -> IO (Either Text [ToolSummary])
listToolsFromDirectory dir = do
    dirExists <- doesDirectoryExist dir
    if not dirExists
        then pure $ Left $ "Directory does not exist: " <> Text.pack dir
        else do
            -- Load scripts using BashTools
            let silentTracer = Tracer (const (pure ()) :: BashTools.LoadTrace -> IO ())
            (scripts, errs) <- BashTools.loadDirectory silentTracer dir

            -- Convert ScriptDescriptions to ToolSummaries
            let summaries =
                    map
                        ( \desc ->
                            ToolSummary
                                { toolName = desc.scriptInfo.scriptSlug
                                , toolType = BashToolType
                                , toolDescription = desc.scriptInfo.scriptDescription
                                , toolSource = Text.pack desc.scriptPath
                                }
                        )
                        scripts.scriptDescriptions

            -- Log warnings about invalid scripts (but don't fail)
            mapM_ (TextIO.hPutStrLn stderr . formatLoadError) errs

            pure $ Right $ sortOn toolName summaries
  where
    formatLoadError :: BashTools.InvalidScriptError -> Text
    formatLoadError (BashTools.InvalidScriptError path code _) =
        "Warning: Failed to load script " <> Text.pack path <> " (exit code: " <> Text.pack (show code) <> ")"
    formatLoadError (BashTools.InvalidDescriptionError path err) =
        "Warning: Invalid description from " <> Text.pack path <> ": " <> Text.pack err

-- | List tools from an MCP server configuration
--
-- This briefly connects to the MCP server to fetch the tool list.
listToolsFromMcpConfig :: FilePath -> IO (Either Text [ToolSummary])
listToolsFromMcpConfig configFile = do
    -- Load the MCP configuration
    eConfig <- loadMcpConfigFromFile configFile
    case eConfig of
        Left err -> pure $ Left err
        Right (McpSimpleBinary cfg) -> do
            -- Try to connect and list tools
            eTools <- connectAndListTools cfg
            case eTools of
                Left err -> pure $ Left err
                Right tools -> pure $ Right $ sortOn toolName tools

-- | Create a CreateProcess for an MCP server executable
createMcpProcess :: FilePath -> [String] -> CreateProcess
createMcpProcess executable args = proc executable args

-- | Connect to an MCP server and list available tools
connectAndListTools :: McpSimpleBinaryConfiguration -> IO (Either Text [ToolSummary])
connectAndListTools cfg = do
    -- executable is FilePath (String), args is [Text]
    let createProc = createMcpProcess cfg.executable (map Text.unpack cfg.args)

    -- Use an MVar to collect results from the callback
    resultVar <- newEmptyMVar

    result <- try $ do
        -- Initialize the MCP client runtime
        mcpRt <- McpClient.initRuntime (Tracer (const (pure ()))) createProc

        -- Run a brief client session to list tools
        McpClient.runClient (Tracer (const (pure ()))) mcpRt $ \_clientInfo -> do
            -- Enumerate tools
            results <- McpClient.enumerateTools
            let tools = extractTools cfg.name results
            -- Store results in the MVar
            liftIO $ putMVar resultVar tools

        -- Retrieve the results
        takeMVar resultVar

    case result of
        Left (e :: IOError) -> pure $ Left $ "Failed to connect to MCP server: " <> Text.pack (show e)
        Right tools -> pure $ Right tools

-- | Extract ToolSummaries from MCP list results
extractTools :: Text -> [Maybe (Either Rpc.ErrorObj McpClient.ListToolsResultRsp)] -> [ToolSummary]
extractTools serverName results =
    let -- Get successful results
        successfulResults =
            [ rsp
            | Just (Right rsp) <- results
            ]

        -- Extract tools from all results
        allTools = concatMap (McpBase.tools . McpClient.getListToolsResult) successfulResults

        -- Convert to summaries using pattern matching to avoid ambiguous fields
        summaries = map toolToSummary allTools
     in summaries
  where
    -- Use pattern matching to extract fields from Tool, avoiding ambiguous record fields
    toolToSummary :: McpBase.Tool -> ToolSummary
    toolToSummary McpBase.Tool{..} =
        ToolSummary
            { toolName = name
            , toolType = McpToolType
            , toolDescription = fromMaybe "" description
            , toolSource = "mcp-server:" <> serverName
            }

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- | Load an agent from a JSON file
loadAgentFromFile :: FilePath -> IO (Either Text Agent)
loadAgentFromFile path = do
    result <- try $ Aeson.eitherDecodeFileStrict' path
    case result of
        Left (e :: IOError) -> pure $ Left $ "Failed to read agent file: " <> Text.pack (show e)
        Right (Left err) -> pure $ Left $ "Failed to parse agent file: " <> Text.pack err
        Right (Right (AgentDescription agent)) -> pure $ Right agent

-- | Load MCP configuration from a JSON file
--
-- The file can contain either a single MCP server description or an array
loadMcpConfigFromFile :: FilePath -> IO (Either Text McpServerDescription)
loadMcpConfigFromFile path = do
    result <- try $ LByteString.readFile path
    case result of
        Left (e :: IOError) -> pure $ Left $ "Failed to read MCP config: " <> Text.pack (show e)
        Right content -> do
            -- Try parsing as a single server first
            case Aeson.eitherDecode content of
                Right desc -> pure $ Right desc
                Left _ ->
                    -- Try parsing as an array and take the first one
                    case Aeson.eitherDecode content of
                        Right (descs :: [McpServerDescription]) ->
                            case descs of
                                [] -> pure $ Left "MCP config contains empty array"
                                (d : _) -> pure $ Right d
                        Left err -> pure $ Left $ "Failed to parse MCP config: " <> Text.pack err

-------------------------------------------------------------------------------
-- Formatters
-------------------------------------------------------------------------------

-- | Format tools as a human-readable table
formatToolsTable :: [ToolSummary] -> Text
formatToolsTable tools =
    let -- Calculate column widths
        nameWidth = max 20 $ maximum $ 0 : map (Text.length . toolName) tools
        typeWidth = 12

        -- Header
        header = padRight nameWidth "NAME" <> "  " <> padRight typeWidth "TYPE" <> "  DESCRIPTION"
        separator = Text.replicate (nameWidth + 2 + typeWidth + 2 + 11) "-"

        -- Rows
        rows = map (formatRow nameWidth typeWidth) tools

        -- Combine
        allLines = [header, separator] ++ rows
     in Text.unlines allLines
  where
    formatRow nameW typeW tool =
        padRight nameW (toolName tool)
            <> "  "
            <> padRight typeW (toolTypeToText $ toolType tool)
            <> "  "
            <> toolDescription tool

    padRight :: Int -> Text -> Text
    padRight n t = t <> Text.replicate (max 0 (n - Text.length t)) " "

    toolTypeToText :: ToolType -> Text
    toolTypeToText t = case t of
        BashToolType -> "bash"
        McpToolType -> "mcp"
        IOToolType -> "io"
        OpenAPIToolType -> "openapi"
        PostgRESToolType -> "postgrest"

-- | Format tools as JSON
formatToolsJson :: [ToolSummary] -> Text
formatToolsJson tools =
    let obj =
            Aeson.object
                [ "tools" .= tools
                , "total" .= length tools
                ]
     in LazyText.toStrict $ AesonText.encodeToLazyText obj

-- | Format tools as simple names, one per line
formatToolsNames :: [ToolSummary] -> Text
formatToolsNames tools = Text.unlines $ map toolName tools

