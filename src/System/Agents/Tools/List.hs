{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tool listing functionality for agents-exe.
--
-- This module provides the ability to list available tools from various sources:
-- * Agent configuration files
-- * Tool directories
-- * MCP server configurations
--
-- It supports multiple output formats (table, JSON, names) for both
-- human readability and machine processing.
--
-- Example usage:
--
-- @
-- import System.Agents.Tools.List
--
-- main :: IO ()
-- main = do
--     let opts = ListToolsOptions
--             { listSource = ToolSourceAgent "agent.json"
--             , listFormat = FormatTable
--             }
--     runListCommand opts
-- @
module System.Agents.Tools.List (
    -- * Core types
    ListToolsOptions (..),
    ToolSource (..),
    ListFormat (..),
    ToolSummary (..),
    ToolType (..),

    -- * Main entry point
    runListCommand,

    -- * Tool listing from sources
    listToolsFromAgent,
    listToolsFromDirectory,
    listToolsFromMcpConfig,

    -- * Formatters
    formatToolsTable,
    formatToolsJson,
    formatToolsNames,
) where

import Control.Exception (try)
import Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LByteString
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import GHC.Generics (Generic)
import Prod.Tracer (Tracer (..))
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr)
import qualified System.Process as Process

import System.Agents.Base (
    Agent (..),
    AgentDescription (..),
    McpServerDescription (..),
    McpSimpleBinaryConfiguration (..),
    OpenAPIToolboxDescription (..),
    OpenAPIServerDescription (..),
    OpenAPIServerOnDisk (..),
    PostgRESTToolboxDescription (..),
    PostgRESTServerDescription (..),
    PostgRESTServerOnDisk (..),
 )
import qualified System.Agents.Tools.Bash as Bash
import qualified System.Agents.Tools.McpToolbox as McpTools

-------------------------------------------------------------------------------
-- Data Types
-------------------------------------------------------------------------------

-- | Options for the list tools command
data ListToolsOptions = ListToolsOptions
    { listSource :: ToolSource
    , listFormat :: ListFormat
    }
    deriving (Show)

-- | Source of tools to list
data ToolSource
    = ToolSourceAgent FilePath
    | -- ^ Load tools from an agent's tool directory
      ToolSourceDirectory FilePath
    | -- ^ Load tools directly from a directory
      ToolSourceMcp FilePath
    deriving (Show)

-- | Output format for tool listing
data ListFormat
    = FormatTable
    | -- ^ Human-readable table (default)
      FormatJson
    | -- ^ JSON for scripting
      FormatNames
    deriving (Show)

-- | Summary of a tool for listing
data ToolSummary = ToolSummary
    { toolName :: Text
    , toolType :: ToolType
    , toolDescription :: Text
    , toolSource :: Text
    }
    deriving (Show, Generic)

instance ToJSON ToolSummary where
    toJSON ts =
        Aeson.object
            [ "name" .= toolName ts
            , "type" .= toolType ts
            , "description" .= toolDescription ts
            , "source" .= toolSource ts
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
    toJSON BashToolType = Aeson.String "bash"
    toJSON McpToolType = Aeson.String "mcp"
    toJSON IOToolType = Aeson.String "io"
    toJSON OpenAPIToolType = Aeson.String "openapi"
    toJSON PostgRESToolType = Aeson.String "postgrest"

-------------------------------------------------------------------------------
-- Main Entry Point
-------------------------------------------------------------------------------

-- | Main entry point for the list command
runListCommand :: ListToolsOptions -> IO ()
runListCommand opts = do
    eTools <- case listSource opts of
        ToolSourceAgent agentFile -> listToolsFromAgent agentFile
        ToolSourceDirectory dir -> listToolsFromDirectory dir
        ToolSourceMcp mcpConfig -> listToolsFromMcpConfig mcpConfig

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
-- Tool Listing from Sources
-------------------------------------------------------------------------------

-- | List tools from an agent configuration file
listToolsFromAgent :: FilePath -> IO (Either Text [ToolSummary])
listToolsFromAgent agentFile = do
    eAgent <- loadAgentFromFile agentFile
    case eAgent of
        Left err -> pure $ Left err
        Right agent -> do
            let agentDir = takeDirectory agentFile

            -- Load bash tools from tool directory
            bashTools <- loadBashTools agentDir agent

            -- Load MCP tools from MCP server configs
            mcpTools <- loadMcpTools agent

            -- Load OpenAPI tools
            openapiTools <- loadOpenAPITools agent

            -- Load PostgREST tools
            postgrestTools <- loadPostgRESTools agent

            let allTools = concat [bashTools, mcpTools, openapiTools, postgrestTools]
            pure $ Right $ sortOn toolName allTools

-- | List tools from a directory containing bash scripts
listToolsFromDirectory :: FilePath -> IO (Either Text [ToolSummary])
listToolsFromDirectory dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure $ Left $ "Directory does not exist: " <> Text.pack dir
        else do
            let tracer = Tracer (const (pure ())) :: Tracer IO Bash.LoadTrace
            (scripts, errors) <- Bash.loadDirectory tracer dir

            -- Log errors to stderr but continue
            mapM_ logBashError errors

            let tools = map (bashScriptToSummary dir) scripts.scriptDescriptions
            pure $ Right $ sortOn toolName tools

-- | List tools from an MCP server configuration file
--
-- This briefly connects to the MCP server to fetch the tools list.
-- The server is then disconnected.
listToolsFromMcpConfig :: FilePath -> IO (Either Text [ToolSummary])
listToolsFromMcpConfig mcpConfig = do
    exists <- doesFileExist mcpConfig
    if not exists
        then pure $ Left $ "MCP config file does not exist: " <> Text.pack mcpConfig
        else do
            eConfig <- Aeson.eitherDecodeFileStrict' mcpConfig
            case eConfig of
                Left err -> pure $ Left $ "Failed to parse MCP config: " <> Text.pack err
                Right (McpSimpleBinary cfg) -> do
                    -- Create process for MCP server
                    let proc =
                            Process.proc
                                cfg.executable
                                (map Text.unpack cfg.args)

                    -- Initialize MCP toolbox briefly to get tools
                    let tracer = Tracer (const (pure ())) :: Tracer IO McpTools.Trace
                    eTools <- try $ do
                        _toolbox <- McpTools.initializeMcpToolbox tracer cfg.name proc
                        -- MCP initialization is async - tools may not be available immediately
                        -- For a proper implementation, we would need to wait for ToolsRefreshed event
                        -- For now, return the placeholder entry
                        pure $ Right []

                    case eTools of
                        Left (e :: IOError) ->
                            pure $ Left $ "Failed to connect to MCP server: " <> Text.pack (show e)
                        Right (Left err) -> pure $ Left err
                        Right (Right tools) -> pure $ Right tools

-------------------------------------------------------------------------------
-- Helper Functions for Loading Tools
-------------------------------------------------------------------------------

-- | Load agent from file
loadAgentFromFile :: FilePath -> IO (Either Text Agent)
loadAgentFromFile path = do
    result <- Aeson.eitherDecodeFileStrict' path
    case result of
        Left err -> pure $ Left $ "Failed to parse agent file: " <> Text.pack err
        Right (AgentDescription agent) -> pure $ Right agent

-- | Load bash tools from an agent's tool directory
loadBashTools :: FilePath -> Agent -> IO [ToolSummary]
loadBashTools agentDir agent = do
    let toolDir = agentDir </> agent.toolDirectory
    exists <- doesDirectoryExist toolDir
    if not exists
        then pure []
        else do
            let tracer = Tracer (const (pure ())) :: Tracer IO Bash.LoadTrace
            (scripts, errors) <- Bash.loadDirectory tracer toolDir

            -- Log errors to stderr but continue
            mapM_ logBashError errors

            pure $ map (bashScriptToSummary toolDir) scripts.scriptDescriptions

-- | Load MCP tools from agent configuration
loadMcpTools :: Agent -> IO [ToolSummary]
loadMcpTools agent =
    case agent.mcpServers of
        Nothing -> pure []
        Just servers -> fmap concat $ mapM (mcpServerToTools agent.slug) servers

-- | Convert MCP server config to tool summaries
--
-- For now, this returns placeholder entries since actually connecting
-- to MCP servers requires async initialization.
mcpServerToTools :: Text -> McpServerDescription -> IO [ToolSummary]
mcpServerToTools _agentSlug (McpSimpleBinary cfg) = do
    -- For listing purposes without full connection,
    -- we create a placeholder entry indicating the MCP server
    pure
        [ ToolSummary
            { toolName = cfg.name <> "_mcp_tools"
            , toolType = McpToolType
            , toolDescription = "MCP tools from server: " <> cfg.name
            , toolSource = Text.pack cfg.executable <> " " <> Text.unwords cfg.args
            }
        ]

-- | Load OpenAPI tools from agent configuration
loadOpenAPITools :: Agent -> IO [ToolSummary]
loadOpenAPITools agent =
    case agent.openApiToolboxes of
        Nothing -> pure []
        Just toolboxes -> fmap concat $ mapM openapiToolboxToTools toolboxes

-- | Convert OpenAPI toolbox to tool summaries
openapiToolboxToTools :: OpenAPIToolboxDescription -> IO [ToolSummary]
openapiToolboxToTools (OpenAPIServer cfg) = do
    -- Extract toolbox name from URL
    let tboxName = extractToolboxName cfg.openApiSpecUrl
    -- For listing purposes without fetching spec,
    -- we create a placeholder entry
    pure
        [ ToolSummary
            { toolName = "openapi_" <> tboxName <> "_tools"
            , toolType = OpenAPIToolType
            , toolDescription = "OpenAPI tools from: " <> cfg.openApiSpecUrl
            , toolSource = cfg.openApiBaseUrl
            }
        ]
openapiToolboxToTools (OpenAPIServerOnDiskDescription (OpenAPIServerOnDisk path)) = do
    pure
        [ ToolSummary
            { toolName = "openapi_disk_tools"
            , toolType = OpenAPIToolType
            , toolDescription = "OpenAPI tools from config: " <> Text.pack path
            , toolSource = Text.pack path
            }
        ]

-- | Load PostgREST tools from agent configuration
loadPostgRESTools :: Agent -> IO [ToolSummary]
loadPostgRESTools agent =
    case agent.postgrestToolboxes of
        Nothing -> pure []
        Just toolboxes -> fmap concat $ mapM postgrestToolboxToTools toolboxes

-- | Convert PostgREST toolbox to tool summaries
postgrestToolboxToTools :: PostgRESTToolboxDescription -> IO [ToolSummary]
postgrestToolboxToTools (PostgRESTServer cfg) = do
    let tboxName = extractPostgrestName cfg.postgrestSpecUrl
    pure
        [ ToolSummary
            { toolName = "postgrest_" <> tboxName <> "_tools"
            , toolType = PostgRESToolType
            , toolDescription = "PostgREST tools from: " <> cfg.postgrestSpecUrl
            , toolSource = cfg.postgrestBaseUrl
            }
        ]
postgrestToolboxToTools (PostgRESTServerOnDiskDescription (PostgRESTServerOnDisk path)) = do
    pure
        [ ToolSummary
            { toolName = "postgrest_disk_tools"
            , toolType = PostgRESToolType
            , toolDescription = "PostgREST tools from config: " <> Text.pack path
            , toolSource = Text.pack path
            }
        ]

-- | Convert bash script to tool summary
bashScriptToSummary :: FilePath -> Bash.ScriptDescription -> ToolSummary
bashScriptToSummary dir script =
    ToolSummary
        { toolName = script.scriptInfo.scriptSlug
        , toolType = BashToolType
        , toolDescription = script.scriptInfo.scriptDescription
        , toolSource = Text.pack dir
        }

-- | Extract toolbox name from spec URL
extractToolboxName :: Text -> Text
extractToolboxName url =
    let withoutProtocol = Text.dropWhile (/= '/') $ Text.drop 8 url
        hostPart = Text.takeWhile (/= '/') withoutProtocol
     in if Text.null hostPart
            then "openapi"
            else normalizeForLLM hostPart

-- | Extract PostgREST name from spec URL
extractPostgrestName :: Text -> Text
extractPostgrestName url =
    let withoutProtocol = Text.dropWhile (/= '/') $ Text.drop 8 url
        hostPart = Text.takeWhile (/= '/') withoutProtocol
     in if Text.null hostPart
            then "postgrest"
            else normalizeForLLM hostPart

-- | Normalize a name for LLM compatibility
normalizeForLLM :: Text -> Text
normalizeForLLM name =
    let -- Replace invalid characters with underscores
        normalized = Text.map replaceInvalid name
        -- Ensure doesn't start with a digit
        withPrefix =
            if Text.null normalized
                then "tool"
                else case Text.head normalized of
                    c | c >= '0' && c <= '9' -> "t" <> normalized
                    _ -> normalized
     in withPrefix
  where
    replaceInvalid c
        | c >= 'a' && c <= 'z' = c
        | c >= 'A' && c <= 'Z' = c
        | c >= '0' && c <= '9' = c
        | c == '_' = c
        | otherwise = '_'

-- | Log a bash loading error
logBashError :: Bash.InvalidScriptError -> IO ()
logBashError err = case err of
    Bash.InvalidScriptError path code _ ->
        TextIO.hPutStrLn stderr $
            "Warning: Failed to load script "
                <> Text.pack path
                <> " (exit code: "
                <> Text.pack (show code)
                <> ")"
    Bash.InvalidDescriptionError path jsonErr ->
        TextIO.hPutStrLn stderr $
            "Warning: Invalid description in "
                <> Text.pack path
                <> ": "
                <> Text.pack jsonErr

-------------------------------------------------------------------------------
-- Output Formatters
-------------------------------------------------------------------------------

-- | Format tools as a human-readable table
formatToolsTable :: [ToolSummary] -> Text
formatToolsTable tools =
    if null tools
        then "No tools found."
        else
            let nameWidth = max 20 $ maximum $ map (Text.length . toolName) tools
                typeWidth = 12
                descWidth = 50

                header = formatRow nameWidth typeWidth descWidth "NAME" "TYPE" "DESCRIPTION"
                separator = formatSeparator nameWidth typeWidth
             in Text.unlines $ header : separator : map (formatToolRow nameWidth typeWidth descWidth) tools

-- | Format a single row of the table
formatRow :: Int -> Int -> Int -> Text -> Text -> Text -> Text
formatRow nameW _typeW _descW name type_ desc =
    Text.justifyLeft nameW ' ' name <> " "
        <> Text.justifyLeft 12 ' ' type_
        <> " "
        <> desc

-- | Format a tool as a table row
formatToolRow :: Int -> Int -> Int -> ToolSummary -> Text
formatToolRow nameW typeW descW ts =
    let name = truncateText nameW $ toolName ts
        type_ = truncateText typeW $ toolTypeToText $ toolType ts
        desc = truncateText descW $ toolDescription ts
     in formatRow nameW typeW descW name type_ desc

-- | Format the separator line
formatSeparator :: Int -> Int -> Text
formatSeparator nameW typeW =
    Text.replicate nameW "-" <> " " <> Text.replicate typeW "-" <> " " <> Text.replicate 50 "-"

-- | Truncate text to a maximum width, adding ellipsis if needed
truncateText :: Int -> Text -> Text
truncateText n t =
    if Text.length t > n
        then Text.take (n - 3) t <> "..."
        else t

-- | Convert ToolType to display text
toolTypeToText :: ToolType -> Text
toolTypeToText BashToolType = "bash"
toolTypeToText McpToolType = "mcp"
toolTypeToText IOToolType = "io"
toolTypeToText OpenAPIToolType = "openapi"
toolTypeToText PostgRESToolType = "postgrest"

-- | Format tools as JSON
formatToolsJson :: [ToolSummary] -> Text
formatToolsJson tools =
    TextEncoding.decodeUtf8 $
        LByteString.toStrict $
            AesonPretty.encodePretty $
                Aeson.object
                    [ "tools" .= tools
                    , "total" .= length tools
                    ]

-- | Format tools as just names, one per line
formatToolsNames :: [ToolSummary] -> Text
formatToolsNames tools =
    if null tools
        then ""
        else Text.unlines $ map toolName tools

