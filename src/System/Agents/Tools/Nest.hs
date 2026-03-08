{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tool nesting functionality - wraps multiple tools into a single nested tool.
--
-- This module provides the ability to map an agent's toolbox into a single
-- nested tool with a bash-tool-compatible interface. This is useful for:
--
-- 1. Testing MCP tools using simple bash scripts
-- 2. Creating composite tools that delegate to multiple underlying tools
-- 3. Using tools in environments that only support bash-style tool invocation
--
-- The generated nested tool implements the standard bash-tool interface with
-- 'describe' and 'run' commands.
module System.Agents.Tools.Nest (
    -- * Configuration types
    NestOptions (..),
    NestConfig (..),
    NestedToolRef (..),
    ToolType (..),

    -- * Main entry points
    runNestCommand,
    generateNestedTool,
    buildNestConfig,
    writeNestedTool,
) where

import Control.Exception (try, IOException)
import Data.Aeson (FromJSON, ToJSON, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.Posix.Files (unionFileModes, ownerExecuteMode, ownerReadMode, ownerWriteMode, setFileMode, fileMode, getFileStatus)
import System.Posix.Types (FileMode)
import System.Process (readProcessWithExitCode)

import System.Agents.Base (Agent (..), AgentDescription (..), McpServerDescription (..), McpSimpleBinaryConfiguration (..))
import System.Agents.Tools.Bash (ScriptInfo (..), ScriptArg (..), ScriptArgArity (..), ScriptArgCallingMode (..))

-------------------------------------------------------------------------------
-- Configuration Types
-------------------------------------------------------------------------------

-- | Options for the nest command
data NestOptions = NestOptions
    { nestAgentFile :: FilePath
    -- ^ Path to the agent JSON file
    , nestOutputPath :: FilePath
    -- ^ Path where the nested tool script will be written
    , nestToolName :: Maybe Text
    -- ^ Optional name for the nested tool (defaults to agent slug)
    , nestIncludeTools :: [Text]
    -- ^ Specific tools to include (empty = all)
    , nestExcludeTools :: [Text]
    -- ^ Tools to exclude
    }
    deriving (Show)

-- | Type of tool being nested
data ToolType
    = BashToolType
    -- ^ Standalone bash script tool
    | McpToolType Text
    -- ^ MCP tool with server name
    | IOToolType
    -- ^ In-process Haskell IO tool
    | OpenAPIToolType Text
    -- ^ OpenAPI tool with operation ID
    | PostgRESToolType Text
    -- ^ PostgREST tool with table path
    deriving (Show, Eq, Generic)

instance ToJSON ToolType where
    toJSON BashToolType = Aeson.String "bash"
    toJSON (McpToolType server) = Aeson.object ["type" .= ("mcp" :: Text), "server" .= server]
    toJSON IOToolType = Aeson.String "io"
    toJSON (OpenAPIToolType opId) = Aeson.object ["type" .= ("openapi" :: Text), "operation" .= opId]
    toJSON (PostgRESToolType path) = Aeson.object ["type" .= ("postgrest" :: Text), "path" .= path]

instance FromJSON ToolType where
    parseJSON (Aeson.String "bash") = pure BashToolType
    parseJSON (Aeson.String "io") = pure IOToolType
    parseJSON (Aeson.Object v) = do
        ty <- v .: "type"
        case (ty :: Text) of
            "mcp" -> McpToolType <$> v .: "server"
            "openapi" -> OpenAPIToolType <$> v .: "operation"
            "postgrest" -> PostgRESToolType <$> v .: "path"
            _ -> fail $ "Unknown tool type: " ++ Text.unpack ty
    parseJSON _ = fail "Invalid tool type format"

-- | Configuration embedded in the generated script
data NestConfig = NestConfig
    { nestConfigName :: Text
    -- ^ Name of the nested tool
    , nestConfigTools :: [NestedToolRef]
    -- ^ References to all nested tools
    }
    deriving (Show, Generic)

instance ToJSON NestConfig where
    toJSON cfg =
        Aeson.object
            [ "name" .= cfg.nestConfigName
            , "tools" .= cfg.nestConfigTools
            ]

instance FromJSON NestConfig where
    parseJSON = Aeson.withObject "NestConfig" $ \v ->
        NestConfig
            <$> v .: "name"
            <*> v .: "tools"

-- | Reference to a single nested tool
data NestedToolRef = NestedToolRef
    { nestedToolName :: Text
    -- ^ Name of the tool (used for dispatch)
    , nestedToolType :: ToolType
    -- ^ Type of the tool
    , nestedToolConfig :: Aeson.Value
    -- ^ Tool-specific configuration
    }
    deriving (Show, Generic)

instance ToJSON NestedToolRef where
    toJSON ref =
        Aeson.object
            [ "name" .= ref.nestedToolName
            , "type" .= ref.nestedToolType
            , "config" .= ref.nestedToolConfig
            ]

instance FromJSON NestedToolRef where
    parseJSON = Aeson.withObject "NestedToolRef" $ \v ->
        NestedToolRef
            <$> v .: "name"
            <*> v .: "type"
            <*> v .: "config"

-------------------------------------------------------------------------------
-- Main Entry Point
-------------------------------------------------------------------------------

-- | Main entry point for the nest command
runNestCommand :: NestOptions -> IO ()
runNestCommand opts = do
    -- Build the nest configuration from the agent file
    eConfig <- buildNestConfig opts
    case eConfig of
        Left err -> do
            putStrLn $ "Error: " ++ Text.unpack err
            failWithExit
        Right (config, tools) -> do
            -- Generate the nested tool script
            let script = generateNestedTool config tools
            -- Write the script with executable permissions
            writeNestedTool opts.nestOutputPath script
            putStrLn $ "Generated nested tool: " ++ opts.nestOutputPath
            putStrLn $ "  Name: " ++ Text.unpack (nestConfigName config)
            putStrLn $ "  Tools: " ++ show (length (nestConfigTools config))
  where
    failWithExit = error "Exit with failure"

-- | Build a NestConfig from an agent's tools
buildNestConfig :: NestOptions -> IO (Either Text (NestConfig, [ToolRegistration]))
buildNestConfig opts = do
    -- Load the agent configuration
    eAgent <- loadAgentFromFile opts.nestAgentFile
    case eAgent of
        Left err -> pure $ Left err
        Right agent -> do
            -- Discover bash tools from the tool directory
            let toolDir = takeDirectory opts.nestAgentFile </> toolDirectory agent
            bashTools <- discoverBashTools toolDir

            -- Build MCP tool references from agent configuration
            let mcpTools = case mcpServers agent of
                    Just servers -> concatMap mcpServerTools servers
                    Nothing -> []

            -- Combine all tool references
            let allTools = bashTools ++ mcpTools

            -- Filter tools based on include/exclude lists
            let filteredTools = filterTools opts allTools

            -- Build the nest config
            let toolName = fromMaybe (slug agent) opts.nestToolName
            let config = NestConfig
                    { nestConfigName = toolName
                    , nestConfigTools = map toolRegistrationToRef filteredTools
                    }

            pure $ Right (config, filteredTools)

-- | Filter tools based on include/exclude options
filterTools :: NestOptions -> [ToolRegistration] -> [ToolRegistration]
filterTools opts tools =
    let included = if null opts.nestIncludeTools
            then tools
            else filter (\t -> toolRegName t `elem` opts.nestIncludeTools) tools
    in filter (\t -> toolRegName t `notElem` opts.nestExcludeTools) included

-- | Load an agent from a JSON file
loadAgentFromFile :: FilePath -> IO (Either Text Agent)
loadAgentFromFile path = do
    result <- try $ ByteString.readFile path
    case result of
        Left (e :: IOException) ->
            pure $ Left $ Text.pack $ "Failed to read agent file: " ++ show e
        Right content ->
            case Aeson.eitherDecode (LByteString.fromStrict content) of
                Left err ->
                    pure $ Left $ Text.pack $ "Failed to parse agent file: " ++ err
                Right (AgentDescription agent) ->
                    pure $ Right agent

-------------------------------------------------------------------------------
-- Tool Discovery
-------------------------------------------------------------------------------

-- | Internal representation of a discovered tool
data ToolRegistration = ToolRegistration
    { toolRegName :: Text
    , toolRegType :: ToolType
    , toolRegScriptInfo :: ScriptInfo
    , toolRegPath :: Maybe FilePath
    -- ^ Path for bash tools
    , toolRegMcpServer :: Maybe Text
    -- ^ Server name for MCP tools
    }
    deriving (Show)

-- | Discover bash tools in a directory
discoverBashTools :: FilePath -> IO [ToolRegistration]
discoverBashTools toolDir = do
    result <- try $ do
        exists <- doesDirectoryExist toolDir
        if not exists
            then pure []
            else do
                entries <- listDirectorySafe toolDir
                let fullPaths = map (toolDir </>) entries
                executableFiles <- filterM isExecutableFile fullPaths
                mapM loadBashTool executableFiles
    case result of
        Left (_ :: IOException) -> pure []
        Right tools -> pure $ catMaybes tools

-- | Load a single bash tool by running its describe command
loadBashTool :: FilePath -> IO (Maybe ToolRegistration)
loadBashTool path = do
    result <- try $ do
        (code, out, _err) <- readProcessWithExitCode path ["describe"] ""
        case code of
            ExitSuccess ->
                case Aeson.eitherDecode (LByteString.fromStrict $ TextEncoding.encodeUtf8 $ Text.pack out) of
                    Left _err -> pure Nothing
                    Right info ->
                        pure $ Just $ ToolRegistration
                            { toolRegName = scriptSlug info
                            , toolRegType = BashToolType
                            , toolRegScriptInfo = info
                            , toolRegPath = Just path
                            , toolRegMcpServer = Nothing
                            }
            ExitFailure _ -> pure Nothing
    case result of
        Left (_ :: IOException) -> pure Nothing
        Right mTool -> pure mTool

-- | Generate MCP tool references from a server configuration
mcpServerTools :: McpServerDescription -> [ToolRegistration]
mcpServerTools (McpSimpleBinary cfg) =
    -- MCP tools are discovered dynamically at runtime
    -- We create a placeholder that will be resolved when the nested tool runs
    [ToolRegistration
        { toolRegName = mconcat [cfg.name, "_tools"]
        , toolRegType = McpToolType cfg.name
        , toolRegScriptInfo = ScriptInfo
            { scriptArgs = []
            , scriptSlug = mconcat [cfg.name, "_tools"]
            , scriptDescription = "MCP tools from server: " <> cfg.name
            , scriptEmptyResultBehavior = Nothing
            }
        , toolRegPath = Nothing
        , toolRegMcpServer = Just cfg.name
        }]

-- | Convert a tool registration to a nested tool reference
toolRegistrationToRef :: ToolRegistration -> NestedToolRef
toolRegistrationToRef reg = NestedToolRef
    { nestedToolName = toolRegName reg
    , nestedToolType = toolRegType reg
    , nestedToolConfig = buildToolConfig reg
    }

-- | Build tool-specific configuration
buildToolConfig :: ToolRegistration -> Aeson.Value
buildToolConfig reg = case toolRegType reg of
    BashToolType ->
        Aeson.object
            [ "path" .= toolRegPath reg
            , "info" .= toolRegScriptInfo reg
            ]
    McpToolType server ->
        Aeson.object
            [ "server" .= server
            ]
    IOToolType ->
        Aeson.object
            [ "description" .= scriptDescription (toolRegScriptInfo reg)
            ]
    OpenAPIToolType opId ->
        Aeson.object
            [ "operation" .= opId
            ]
    PostgRESToolType path ->
        Aeson.object
            [ "path" .= path
            ]

-------------------------------------------------------------------------------
-- Script Generation
-------------------------------------------------------------------------------

-- | Generate the nested tool script content
generateNestedTool :: NestConfig -> [ToolRegistration] -> Text
generateNestedTool config tools =
    Text.unlines
        [ "#!/bin/bash"
        , "# Generated nested tool: " <> nestConfigName config
        , "# This script was generated by agents-exe toolbox nest"
        , "#"
        , "# Usage:"
        , "#   " <> nestConfigName config <> " describe     - Show tool information"
        , "#   " <> nestConfigName config <> " run --tool=<name> --arg='<json>' - Run a sub-tool"
        , ""
        , "# Embedded nest configuration"
        , "NEST_CONFIG='" <> escapeSingleQuotes (TextEncoding.decodeUtf8 $ LByteString.toStrict $ Aeson.encode config) <> "'"
        , ""
        , "# Command dispatch"
        , "case \"$1\" in"
        , "  describe)"
        , generateDescribeCommand config tools
        , "    ;;"
        , "  run)"
        , generateRunCommand config
        , "    ;;"
        , "  *)"
        , "    echo \"Unknown command: $1\" >&2"
        , "    echo \"Usage: $0 {describe|run}\" >&2"
        , "    exit 1"
        , "    ;;"
        , "esac"
        ]

-- | Generate the describe command implementation
generateDescribeCommand :: NestConfig -> [ToolRegistration] -> Text
generateDescribeCommand config tools =
    Text.unlines
        [ "    # Return ScriptInfo for this nested tool"
        , "    cat << 'NESTED_DESCRIBE_EOF'"
        , TextEncoding.decodeUtf8 $ LByteString.toStrict $ Aeson.encode $ buildNestedScriptInfo config tools
        , "NESTED_DESCRIBE_EOF"
        ]

-- | Generate the run command implementation
generateRunCommand :: NestConfig -> Text
generateRunCommand config =
    let header = Text.unlines
            [ "    # Parse arguments"
            , "    TOOL_NAME=\"\""
            , "    TOOL_ARG=\"{}\""
            , "    for arg in \"$@\"; do"
            , "      case \"$arg\" in"
            , "        --tool=*)"
            , "          TOOL_NAME=\"${arg#*=}\""
            , "          ;;"
            , "        --arg=*)"
            , "          TOOL_ARG=\"${arg#*=}\""
            , "          ;;"
            , "      esac"
            , "    done"
            , ""
            , "    if [ -z \"$TOOL_NAME\" ]; then"
            , "      echo \"Error: --tool argument required\" >&2"
            , "      exit 1"
            , "    fi"
            , ""
            , "    # Dispatch to the appropriate tool handler"
            , "    case \"$TOOL_NAME\" in"
            ]
        cases = map (generateToolDispatch config) (nestConfigTools config)
        footer = Text.unlines
            [ "      *)"
            , "        echo \"Unknown tool: $TOOL_NAME\" >&2"
            , "        exit 1"
            , "        ;;"
            , "    esac"
            ]
    in header <> Text.concat cases <> footer

-- | Generate dispatch code for a single tool
generateToolDispatch :: NestConfig -> NestedToolRef -> Text
generateToolDispatch _config ref =
    let toolName = nestedToolName ref
    in Text.unlines
        [ "      \"" <> toolName <> "\")"
        , generateToolHandler ref
        , "        ;;"
        ]

-- | Generate the handler code for a specific tool type
generateToolHandler :: NestedToolRef -> Text
generateToolHandler ref = case nestedToolType ref of
    BashToolType ->
        let path = case nestedToolConfig ref of
                Aeson.Object obj ->
                    case KeyMap.lookup (AesonKey.fromText "path") obj of
                        Just (Aeson.String p) -> p
                        _ -> ""
                _ -> ""
        in Text.unlines
            [ "        # Run bash tool: " <> nestedToolName ref
            , "        " <> Text.pack (show path) <> " run --arg=\"$TOOL_ARG\""
            ]

    McpToolType _server ->
        Text.unlines
            [ "        # Run MCP tool: " <> nestedToolName ref
            , "        # MCP tools require agents-exe to be installed"
            , "        if command -v agents-exe >/dev/null 2>&1; then"
            , "          agents-exe toolbox call-mcp --server \"" <> nestedToolName ref <> "\" --arg \"$TOOL_ARG\""
            , "        else"
            , "          echo \"Error: MCP tool requires agents-exe to be installed\" >&2"
            , "          exit 1"
            , "        fi"
            ]

    IOToolType ->
        Text.unlines
            [ "        # Run IO tool: " <> nestedToolName ref
            , "        # IO tools require agents-exe to be installed"
            , "        if command -v agents-exe >/dev/null 2>&1; then"
            , "          agents-exe toolbox call-io --tool \"" <> nestedToolName ref <> "\" --arg \"$TOOL_ARG\""
            , "        else"
            , "          echo \"Error: IO tool requires agents-exe to be installed\" >&2"
            , "          exit 1"
            , "        fi"
            ]

    OpenAPIToolType _opId ->
        Text.unlines
            [ "        # Run OpenAPI tool: " <> nestedToolName ref
            , "        # OpenAPI tools require agents-exe to be installed"
            , "        if command -v agents-exe >/dev/null 2>&1; then"
            , "          agents-exe toolbox call-openapi --tool \"" <> nestedToolName ref <> "\" --arg \"$TOOL_ARG\""
            , "        else"
            , "          echo \"Error: OpenAPI tool requires agents-exe to be installed\" >&2"
            , "          exit 1"
            , "        fi"
            ]

    PostgRESToolType _path ->
        Text.unlines
            [ "        # Run PostgREST tool: " <> nestedToolName ref
            , "        # PostgREST tools require agents-exe to be installed"
            , "        if command -v agents-exe >/dev/null 2>&1; then"
            , "          agents-exe toolbox call-postgrest --tool \"" <> nestedToolName ref <> "\" --arg \"$TOOL_ARG\""
            , "        else"
            , "          echo \"Error: PostgREST tool requires agents-exe to be installed\" >&2"
            , "          exit 1"
            , "        fi"
            ]

-- | Build the ScriptInfo for the nested tool
buildNestedScriptInfo :: NestConfig -> [ToolRegistration] -> ScriptInfo
buildNestedScriptInfo config tools =
    let numTools = length tools
        description = mconcat
            [ "A nested tool providing access to "
            , Text.pack (show numTools)
            , " sub-tool"
            , if numTools == 1 then "" else "s"
            ]
    in ScriptInfo
        { scriptArgs =
            [ ScriptArg
                { argName = "tool"
                , argDescription = "Name of the sub-tool to invoke"
                , argTypeString = "string"
                , argBackingTypeString = "string"
                , argTypeArity = Single
                , argCallingMode = DashDashEqual
                }
            , ScriptArg
                { argName = "arg"
                , argDescription = "JSON arguments for the sub-tool"
                , argTypeString = "string"
                , argBackingTypeString = "string"
                , argTypeArity = Single
                , argCallingMode = DashDashEqual
                }
            ]
        , scriptSlug = nestConfigName config
        , scriptDescription = description
        , scriptEmptyResultBehavior = Nothing
        }

-------------------------------------------------------------------------------
-- File Operations
-------------------------------------------------------------------------------

-- | Write the generated script with executable permissions
writeNestedTool :: FilePath -> Text -> IO ()
writeNestedTool path content = do
    -- Ensure the output directory exists
    createDirectoryIfMissing True (takeDirectory path)

    -- Write the script content
    ByteString.writeFile path (TextEncoding.encodeUtf8 content)

    -- Set executable permissions (owner read/write/execute)
    let execPerms = unionFileModes ownerReadMode $ unionFileModes ownerWriteMode ownerExecuteMode
    setFileMode path execPerms

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Escape single quotes for embedding in bash single-quoted strings
escapeSingleQuotes :: Text -> Text
escapeSingleQuotes = Text.replace "'" "'\"'\"'"

-- | Safe version of listDirectory that catches exceptions
listDirectorySafe :: FilePath -> IO [FilePath]
listDirectorySafe path = do
    result <- try $ listDirectory path
    case result of
        Left (_ :: IOException) -> pure []
        Right entries -> pure entries

-- | Check if a file is executable
isExecutableFile :: FilePath -> IO Bool
isExecutableFile path = do
    result <- try $ do
        isFile <- doesFileExist path
        if not isFile
            then pure False
            else do
                perms <- getFileModeSafe path
                pure $ (perms `unionFileModes` ownerExecuteMode) == perms
    case result of
        Left (_ :: IOException) -> pure False
        Right r -> pure r

-- | Safe version of getFileMode
getFileModeSafe :: FilePath -> IO FileMode
getFileModeSafe path = do
    result <- try $ getFileStatus path
    case result of
        Left (_ :: IOException) -> pure 0
        Right status -> pure $ fileMode status

-- | FilterM for IO
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p = foldr (\x acc -> do
    b <- p x
    xs <- acc
    pure $ if b then x : xs else xs) (pure [])

