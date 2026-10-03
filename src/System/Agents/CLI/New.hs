{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'new' command handler.

The new command provides scaffolding for creating new agents and tools.
Model selection is guided by a model catalog: when a model name is given,
the provider preset is inferred automatically (e.g. @kimi-k2.5@ selects
the Moonshot preset). When no model is given, the default OpenAI preset
is used.
-}
module System.Agents.CLI.New (
    handleNew,
    NewOptions (..),
    NewCommand (..),
    NewAgentOptions (..),
    NewToolOptions (..),
    NewModelsOptions (..),
    NewModelsSubcommand (..),
    ModelPreset (..),
    ToolLanguage (..),
    ConfigUpdateMode (..),
    -- Exported for testing
    buildAgentConfig,
    workspaceSandboxName,
    defaultWorkspaceSandbox,
    agentListedInConfig,
    addAgentFileToConfig,
    configEntryForAgent,
    defaultPresets,
    defaultSystemPrompt,
    toolLanguageToExtension,
    makeToolTemplate,
    supportedLanguages,
) where

import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.Exit (exitFailure)
import System.FilePath (
    dropTrailingPathSeparator,
    makeRelative,
    normalise,
    takeDirectory,
    takeExtension,
    (<.>),
    (</>),
 )
import System.IO (hFlush, hIsTerminalDevice, stderr, stdin, stdout)
import System.Posix.Files (ownerExecuteMode, ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)

import System.Agents.Base (
    Agent (..),
    AgentDescription (..),
    BuiltinToolboxDescription (..),
    DeveloperToolCapability (..),
    DeveloperToolboxDescription (..),
    FileSandboxConfig (..),
    FileSandboxSpec (..),
    SqliteToolboxDescription (..),
    SqliteVersioningConfig (..),
    SystemToolCapability (..),
    SystemToolboxDescription (..),
 )
import System.Agents.CLI.ConfigLoader (AgentsExeConfig (..), locateAgentsExeConfig)
import System.Agents.FileSandbox.Predicate (PathPredicate (..))
import System.Agents.CLI.New.ModelCatalog (
    ModelCatalog (..),
    catalogEntriesText,
    defaultModelCatalog,
    defaultModelCatalogUrl,
    loadModelCatalog,
    lookupPresetForModel,
    modelCatalogPath,
    saveModelCatalog,
    updateModelCatalogFromUrl,
 )

-- | Name of the file sandbox a new agent declares and its toolboxes share.
workspaceSandboxName :: Text
workspaceSandboxName = "workspace"

{- | The file sandbox of a new agent: read and write access to the directory
the agent runs in, and everything below it.
-}
defaultWorkspaceSandbox :: FileSandboxConfig
defaultWorkspaceSandbox =
    FileSandboxConfig
        { fsbPredicate = DirectoryRecursive "./"
        , fsbMaxFileSize = Just (50 * 1024 * 1024)
        , fsbName = Nothing
        }

{- | Default developer toolbox configuration for new agents: the scaffolding
tools, plus file reading and editing within the workspace sandbox.
-}
defaultDeveloperToolbox :: BuiltinToolboxDescription
defaultDeveloperToolbox =
    DeveloperToolbox $
        DeveloperToolboxDescription
            { developerToolboxName = "developer"
            , developerToolboxDescription = "Tools for developing agents and tools"
            , developerToolboxCapabilities =
                [ DevToolShowSpec
                , DevToolValidateAgent
                , DevToolCreateAgent
                , DevToolCreateTool
                , DevToolReadFileRange
                , DevToolWriteFileRange
                , DevToolPatchFile
                ]
            , developerToolboxActivation = Nothing -- Uses default: AlwaysActivated
            , developerToolboxFileSandbox = Just (NamedFileSandbox workspaceSandboxName)
            , developerToolboxBuildCommand = Nothing
            }

-- | Default system toolbox for new agents: listing the workspace's directories.
defaultSystemToolbox :: BuiltinToolboxDescription
defaultSystemToolbox =
    SystemToolbox $
        SystemToolboxDescription
            { systemToolboxName = "system"
            , systemToolboxDescription = "Working directory and directory listings"
            , systemToolboxCapabilities =
                [ SystemToolWorkingDirectory
                , SystemToolListDirectory
                ]
            , systemToolboxEnvVarFilter = Nothing
            , systemToolboxActivation = Nothing
            , systemToolboxSessionIntrospectionScope = Nothing
            , systemToolboxSessionIntrospectionMaxResults = Nothing
            , systemToolboxSessionIntrospectionIncludeToolOutputs = Nothing
            , systemToolboxFileSandbox = Just (NamedFileSandbox workspaceSandboxName)
            , systemToolboxCommandFilter = Nothing
            }

-- | Path of a new agent's memory database, relative to where the agent runs.
defaultMemoryPath :: Text -> FilePath
defaultMemoryPath agentSlug = "./" <> Text.unpack agentSlug <> "-memory.sqlite"

-- | Default memory for new agents: a read-write SQLite database.
defaultMemoryToolbox :: Text -> BuiltinToolboxDescription
defaultMemoryToolbox agentSlug =
    SqliteToolbox $
        SqliteToolboxDescription
            { sqliteToolboxName = "memory"
            , sqliteToolboxDescription = "Notes and facts to remember across conversations"
            , sqliteToolboxVersioning = SqliteReadWrite (defaultMemoryPath agentSlug)
            , sqliteToolboxActivation = Nothing
            }

-- | Model preset configurations
data ModelPreset = ModelPreset
    { presetFlavor :: Text
    , presetModelUrl :: Text
    , presetModelName :: Text
    , presetApiKeyId :: Text
    }
    deriving (Show, Eq)

-- | Options for new agent command
data NewAgentOptions = NewAgentOptions
    { newAgentSlug :: Text
    , newAgentFilePath :: FilePath
    , newAgentModel :: Maybe Text
    -- ^ Optional model name override. When given, the provider preset is
    -- inferred from the model catalog.
    , newAgentConfigUpdate :: ConfigUpdateMode
    -- ^ What to do when the new agent is not listed in @agents-exe.cfg.json@.
    }
    deriving (Show, Eq)

-- | Whether to add a new agent to @agents-exe.cfg.json@ when it is not listed.
data ConfigUpdateMode
    = -- | Ask on an interactive terminal, otherwise only say how to do it
      ConfigUpdateAsk
    | -- | Add it without asking
      ConfigUpdateAlways
    | -- | Leave the config file alone
      ConfigUpdateNever
    deriving (Show, Eq)

-- | Programming language for tool scaffolding
data ToolLanguage
    = BashLang
    | PythonLang
    | HaskellLang
    | NodeLang
    deriving (Show, Eq, Ord)

-- | Supported languages mapping for CLI parsing
supportedLanguages :: Map Text ToolLanguage
supportedLanguages =
    Map.fromList
        [ ("bash", BashLang)
        , ("python", PythonLang)
        , ("haskell", HaskellLang)
        , ("node", NodeLang)
        , ("nodejs", NodeLang)
        ]

-- | Get file extension for a language
toolLanguageToExtension :: ToolLanguage -> String
toolLanguageToExtension BashLang = ""
toolLanguageToExtension PythonLang = ".py"
toolLanguageToExtension HaskellLang = ".hs"
toolLanguageToExtension NodeLang = ".js"

-- | Options for new tool command
data NewToolOptions = NewToolOptions
    { newToolSlug :: Text
    , newToolLanguage :: ToolLanguage
    , newToolFilePath :: FilePath
    }
    deriving (Show, Eq)

-- | Subcommands for managing the model catalog.
data NewModelsSubcommand
    = ListModels
    | UpdateModels (Maybe Text)
    -- ^ Download the catalog from an optional URL override.
    | InitModels
    -- ^ Write the built-in catalog to the local config file.
    deriving (Show, Eq)

-- | Options for the @new models@ subcommand.
data NewModelsOptions = NewModelsOptions
    { newModelsConfigDir :: FilePath
    , newModelsSubcommand :: NewModelsSubcommand
    }
    deriving (Show, Eq)

-- | Subcommands for the 'new' command
data NewCommand
    = -- | Create a new agent with given options
      NewAgent NewAgentOptions
    | -- | Create a new tool with given options
      NewTool NewToolOptions
    | -- | Manage the model catalog
      NewModels NewModelsOptions
    deriving (Show, Eq)

-- | Options for the new command
data NewOptions = NewOptions
    { newCommand :: NewCommand
    , newForce :: Bool
    -- ^ Overwrite existing files
    }
    deriving (Show, Eq)

-- | Default presets for common providers
defaultPresets :: Map Text ModelPreset
defaultPresets =
    Map.fromList
        [
            ( "openai"
            , ModelPreset
                { presetFlavor = "OpenAIv1"
                , presetModelUrl = "https://api.openai.com/v1"
                , presetModelName = "gpt-4-turbo-preview"
                , presetApiKeyId = "main-key"
                }
            )
        ,
            ( "mistral"
            , ModelPreset
                { presetFlavor = "OpenAIv1"
                , presetModelUrl = "https://api.mistral.ai/v1"
                , presetModelName = "mistral-large-latest"
                , presetApiKeyId = "mistral-key"
                }
            )
        ,
            ( "ollama"
            , ModelPreset
                { presetFlavor = "OpenAIv1"
                , presetModelUrl = "http://localhost:11434/v1"
                , presetModelName = "llama3.2"
                , presetApiKeyId = "ollama-key"
                }
            )
        ,
            ( "kimi"
            , ModelPreset
                { presetFlavor = "KimiV1"
                , presetModelUrl = "https://api.moonshot.ai/v1"
                , presetModelName = "kimi-k2.5"
                , presetApiKeyId = "kimi-key"
                }
            )
        ]

-- | Default system prompt based on agent slug
defaultSystemPrompt :: Text -> [Text]
defaultSystemPrompt agentSlug =
    [ "You are " <> agentSlug <> ", a helpful AI assistant."
    , "You provide clear, accurate, and concise responses."
    , "When using tools, you explain your actions to the user."
    ]

-- | Resolve the preset name to use, inferring from the model catalog when
-- a model name is supplied and defaulting to OpenAI otherwise.
resolvePresetName :: ModelCatalog -> NewAgentOptions -> Either String Text
resolvePresetName catalog opts =
    case opts.newAgentModel of
        Nothing -> Right "openai"
        Just model ->
            case lookupPresetForModel catalog model of
                Just (preset, _entry) -> Right preset
                Nothing ->
                    Left $
                        "Cannot infer provider preset for model '"
                            ++ Text.unpack model
                            ++ "'. Add a matching entry to the model catalog."

-- | Build agent configuration from options.
--
-- Returns the resolved preset name alongside the agent so that callers can
-- report which preset was actually used (especially useful when it was
-- inferred from the model catalog).
buildAgentConfig :: ModelCatalog -> NewAgentOptions -> Either String (Text, Agent)
buildAgentConfig catalog opts = do
    presetName <- resolvePresetName catalog opts
    preset <- case Map.lookup presetName defaultPresets of
        Nothing -> Left $ "Unknown preset: " ++ Text.unpack presetName
        Just p -> Right p

    let selectedModelName = fromMaybe preset.presetModelName opts.newAgentModel

    let agent =
            Agent
                { slug = opts.newAgentSlug
                , apiKeyId = preset.presetApiKeyId
                , flavor = preset.presetFlavor
                , modelUrl = preset.presetModelUrl
                , modelName = selectedModelName
                , announce = "a helpful assistant powered by " <> selectedModelName
                , systemPrompt = defaultSystemPrompt opts.newAgentSlug
                , toolDirectory = Just "tools"
                , bashToolboxes = Nothing
                , mcpServers = Just []
                , openApiToolboxes = Nothing
                , postgrestToolboxes = Nothing
                , builtinToolboxes =
                    Just
                        [ defaultDeveloperToolbox
                        , defaultSystemToolbox
                        , defaultMemoryToolbox opts.newAgentSlug
                        ]
                , fileSandboxes = Just (Map.singleton workspaceSandboxName defaultWorkspaceSandbox)
                , extraAgents = Nothing
                , skillSources = Nothing
                , autoEnableSkills = Nothing
                , executionMode = Nothing
                , toolCallPolicyConfig = Nothing
                , asyncYieldStrategy = Nothing
                , maxConcurrency = Nothing
                , asyncCallTimeoutSeconds = Nothing
                , bindings = Nothing
                , parameters = Nothing
                , pauseCancelsCalls = Nothing
                , resumeOnAnyMail = Nothing
                , interruptCompletions = Nothing
                , mailInToolResult = Nothing
                , mailScope = Nothing
                , interruptScope = Nothing, wakeOn = Nothing
                }

    pure (presetName, agent)

-- | Load the model catalog, falling back to built-in defaults and warning
-- on parse errors.
loadCatalogOrDefault :: FilePath -> IO ModelCatalog
loadCatalogOrDefault configDir = do
    result <- loadModelCatalog (modelCatalogPath configDir)
    case result of
        Left err -> do
            Text.hPutStrLn stderr $
                "Warning: " <> Text.pack err <> ". Using built-in model catalog."
            pure defaultModelCatalog
        Right catalog -> pure catalog

-- | Handle the new command: create agent or tool scaffolding, or manage
-- the model catalog.
handleNew ::
    -- | Config directory (used for the model catalog)
    FilePath ->
    -- | Options for new command
    NewOptions ->
    IO ()
handleNew configDir opts = case opts.newCommand of
    NewAgent agentOpts -> do
        catalog <- loadCatalogOrDefault configDir
        handleNewAgent opts.newForce catalog agentOpts
    NewTool toolOpts ->
        handleNewTool opts.newForce toolOpts
    NewModels modelsOpts ->
        handleNewModels opts.newForce modelsOpts

-- | Handle the new agent command
handleNewAgent :: Bool -> ModelCatalog -> NewAgentOptions -> IO ()
handleNewAgent force catalog opts = do
    -- Check if file already exists
    unless force $ do
        exists <- doesFileExist opts.newAgentFilePath
        when exists $ do
            Text.hPutStrLn stderr $
                "Error: File already exists: " <> Text.pack opts.newAgentFilePath
            Text.hPutStrLn stderr "Use --force to overwrite"
            exitFailure

    -- Build agent config
    case buildAgentConfig catalog opts of
        Left err -> do
            Text.hPutStrLn stderr $ "Error: " <> Text.pack err
            exitFailure
        Right (presetName, agent) -> do
            -- Create directory structure
            createDirectoryIfMissing True (takeDirectory opts.newAgentFilePath)
            -- Create tool directory if toolDirectory is specified
            case agent.toolDirectory of
                Just toolDir -> do
                    createDirectoryIfMissing True (takeDirectory opts.newAgentFilePath </> toolDir)
                    Text.putStrLn $ "Created agent: " <> Text.pack opts.newAgentFilePath
                    Text.putStrLn $ "Tool directory: " <> Text.pack (takeDirectory opts.newAgentFilePath </> toolDir)
                Nothing -> do
                    Text.putStrLn $ "Created agent: " <> Text.pack opts.newAgentFilePath
                    Text.putStrLn $ "No tool directory configured"

            -- Write agent file
            LByteString.writeFile opts.newAgentFilePath $
                Aeson.encodePretty (AgentDescription agent)

            Text.putStrLn $ "Model: " <> agent.modelName
            Text.putStrLn $ "Preset: " <> presetName
            Text.putStrLn $ "Provider: " <> Text.pack (show agent.flavor)
            Text.putStrLn "Files: read/write under ./ (file sandbox 'workspace')"
            Text.putStrLn $ "Memory: " <> Text.pack (defaultMemoryPath agent.slug)

            reportConfigCoverage opts

{- | Whether an agent file is loaded by a config, either named in
@agentsFiles@ or sitting directly in one of the @agentsDirectories@ (which
only pick up @.json@ and template files, and do not recurse).

Config paths are resolved against the working directory, as the loader does.
-}
agentListedInConfig ::
    -- | Working directory (absolute)
    FilePath ->
    AgentsExeConfig ->
    -- | Agent file
    FilePath ->
    Bool
agentListedInConfig cwd cfg agentFile =
    agentPath `elem` fmap absolute cfg.agentsFiles
        || ( takeExtension agentPath == ".json"
                && takeDirectory agentPath `elem` fmap absolute cfg.agentsDirectories
           )
  where
    agentPath = absolute agentFile
    absolute = dropTrailingPathSeparator . normalise . (cwd </>)

{- | The @agentsFiles@ entry for an agent file: relative when the config file
sits in the working directory, absolute otherwise (config paths are resolved
against the working directory, not against the config file).
-}
configEntryForAgent ::
    -- | Working directory (absolute)
    FilePath ->
    -- | Config file
    FilePath ->
    -- | Agent file
    FilePath ->
    FilePath
configEntryForAgent cwd configPath agentFile
    | configDir == dropTrailingPathSeparator (normalise cwd) = "./" <> makeRelative cwd agentPath
    | otherwise = agentPath
  where
    configDir = dropTrailingPathSeparator (normalise (takeDirectory (cwd </> configPath)))
    agentPath = normalise (cwd </> agentFile)

-- | Append an entry to the @agentsFiles@ of a config, keeping every other field.
addAgentFileToConfig :: FilePath -> Aeson.Value -> Either String Aeson.Value
addAgentFileToConfig entry (Aeson.Object obj) =
    case KeyMap.lookup "agentsFiles" obj of
        Nothing -> Right (withFiles (Vector.singleton entryValue))
        Just (Aeson.Array files) -> Right (withFiles (Vector.snoc files entryValue))
        Just _ -> Left "agentsFiles is not an array"
  where
    entryValue = Aeson.String (Text.pack entry)
    withFiles files = Aeson.Object (KeyMap.insert "agentsFiles" (Aeson.Array files) obj)
addAgentFileToConfig _ _ = Left "the config file is not a JSON object"

{- | Tell whether the new agent is picked up by @agents-exe.cfg.json@, and
offer to add it to @agentsFiles@ when it is not.
-}
reportConfigCoverage :: NewAgentOptions -> IO ()
reportConfigCoverage opts = do
    cwd <- getCurrentDirectory
    located <- locateAgentsExeConfig
    case located of
        Nothing ->
            Text.putStrLn $
                "No agents-exe.cfg.json found: use --agent-file "
                    <> Text.pack opts.newAgentFilePath
                    <> ", or create a config with 'agents-exe config local init'."
        Just configPath -> do
            decoded <- Aeson.eitherDecodeFileStrict' configPath
            case decoded >>= \value -> (,) value <$> fromResult (Aeson.fromJSON value) of
                Left err ->
                    Text.hPutStrLn stderr $
                        "Warning: cannot read " <> Text.pack configPath <> ": " <> Text.pack err
                Right (value, cfg)
                    | agentListedInConfig cwd cfg opts.newAgentFilePath ->
                        Text.putStrLn $ "Listed in: " <> Text.pack configPath
                    | otherwise -> do
                        let entry = configEntryForAgent cwd configPath opts.newAgentFilePath
                        Text.putStrLn $ "Not listed in: " <> Text.pack configPath
                        accepted <- case opts.newAgentConfigUpdate of
                            ConfigUpdateAlways -> pure True
                            ConfigUpdateNever -> pure False
                            ConfigUpdateAsk -> do
                                interactive <- hIsTerminalDevice stdin
                                if interactive then askYesNo "Add it to agentsFiles?" else pure False
                        if accepted
                            then case addAgentFileToConfig entry value of
                                Left err -> do
                                    Text.hPutStrLn stderr $
                                        "Error: cannot update " <> Text.pack configPath <> ": " <> Text.pack err
                                    exitFailure
                                Right updated -> do
                                    LByteString.writeFile configPath (Aeson.encodePretty updated)
                                    Text.putStrLn $ "Added " <> Text.pack entry <> " to agentsFiles"
                            else
                                Text.putStrLn $
                                    "Add \"" <> Text.pack entry <> "\" to its agentsFiles, or re-run with --add-to-config."
  where
    fromResult :: Aeson.Result a -> Either String a
    fromResult (Aeson.Success a) = Right a
    fromResult (Aeson.Error err) = Left err

-- | Ask a yes/no question on the terminal; anything but y/yes is a no.
askYesNo :: Text -> IO Bool
askYesNo question = do
    Text.putStr (question <> " [y/N] ")
    hFlush stdout
    answer <- Text.toLower . Text.strip <$> Text.getLine
    pure (answer `elem` ["y", "yes"])

-- | Handle the new tool command
handleNewTool :: Bool -> NewToolOptions -> IO ()
handleNewTool force opts = do
    let extension = toolLanguageToExtension opts.newToolLanguage
    let finalPath = opts.newToolFilePath <.> extension

    -- Check if file already exists
    unless force $ do
        exists <- doesFileExist finalPath
        when exists $ do
            Text.hPutStrLn stderr $
                "Error: File already exists: " <> Text.pack finalPath
            Text.hPutStrLn stderr "Use --force to overwrite"
            exitFailure

    -- Create directory
    createDirectoryIfMissing True (takeDirectory finalPath)

    -- Generate and write tool content
    let content = makeToolTemplate opts.newToolLanguage opts.newToolSlug
    Text.writeFile finalPath content

    -- Set executable permissions for all languages
    let mode = ownerReadMode `unionFileModes` ownerWriteMode `unionFileModes` ownerExecuteMode
    setFileMode finalPath mode

    -- Output success message with next steps
    Text.putStrLn $ "Created tool: " <> Text.pack finalPath
    Text.putStrLn $ "Language: " <> Text.pack (show opts.newToolLanguage)
    Text.putStrLn ""
    Text.putStrLn "Next steps:"
    Text.putStrLn "  1. Edit the 'args' array in the describe function"
    Text.putStrLn "  2. Implement the run function"
    Text.putStrLn $ "  3. Test with: agents-exe describe-tool " <> Text.pack finalPath

-- | Handle the @new models@ subcommand.
handleNewModels :: Bool -> NewModelsOptions -> IO ()
handleNewModels force opts = case opts.newModelsSubcommand of
    ListModels -> do
        catalog <- loadCatalogOrDefault opts.newModelsConfigDir
        let path = modelCatalogPath opts.newModelsConfigDir
        localExists <- doesFileExist path
        Text.putStrLn $ catalogEntriesText catalog
        unless localExists $
            Text.putStrLn $
                "Using built-in defaults. Run 'agents-exe new models init' to persist a local catalog."
    UpdateModels mUrl -> do
        let url = fromMaybe defaultModelCatalogUrl mUrl
        result <- updateModelCatalogFromUrl url
        case result of
            Left err -> do
                Text.hPutStrLn stderr $ "Error: " <> Text.pack err
                exitFailure
            Right catalog -> do
                let path = modelCatalogPath opts.newModelsConfigDir
                unless force $ do
                    exists <- doesFileExist path
                    when exists $ do
                        Text.hPutStrLn stderr $
                            "Error: Model catalog already exists: " <> Text.pack path
                        Text.hPutStrLn stderr "Use --force to overwrite"
                        exitFailure
                saveModelCatalog path catalog
                Text.putStrLn $ "Updated model catalog: " <> Text.pack path
                Text.putStrLn $ "Entries: " <> Text.pack (show (length catalog.catalogEntries))
    InitModels -> do
        let path = modelCatalogPath opts.newModelsConfigDir
        unless force $ do
            exists <- doesFileExist path
            when exists $ do
                Text.hPutStrLn stderr $
                    "Error: Model catalog already exists: " <> Text.pack path
                Text.hPutStrLn stderr "Use --force to overwrite"
                exitFailure
        saveModelCatalog path defaultModelCatalog
        Text.putStrLn $ "Initialized model catalog: " <> Text.pack path
        Text.putStrLn "Edit this file to add custom model patterns."

-- | Create a tool template for a given language
makeToolTemplate :: ToolLanguage -> Text -> Text
makeToolTemplate language toolSlug = case language of
    BashLang -> makeBashToolTemplate toolSlug
    PythonLang -> makePythonToolTemplate toolSlug
    HaskellLang -> makeHaskellToolTemplate toolSlug
    NodeLang -> makeNodeToolTemplate toolSlug

-- | Create a bash tool template
makeBashToolTemplate :: Text -> Text
makeBashToolTemplate toolSlug =
    Text.unlines
        [ "#!/bin/bash"
        , "# " <> toolSlug <> " - A bash tool for agents-exe"
        , ""
        , "set -euo pipefail"
        , ""
        , "# Agents-exe tool protocol: describe|run"
        , "# Environment variables available during 'run':"
        , "#   AGENT_SESSION_ID      - UUID of the current session"
        , "#   AGENT_CONVERSATION_ID - UUID of the conversation"
        , "#   AGENT_TURN_ID         - UUID of the current turn"
        , "#   AGENT_AGENT_ID        - UUID of the executing agent (if available)"
        , ""
        , "case \"${1:-}\" in"
        , "  describe)"
        , "    cat <<'DESCRIBE_EOF'"
        , "{"
        , "  \"slug\": \"" <> toolSlug <> "\","
        , "  \"description\": \"Tool " <> toolSlug <> " - describe what this tool does\","
        , "  \"args\": [],"
        , "  \"empty-result\": {"
        , "    \"tag\": \"AddMessage\","
        , "    \"contents\": \"--no output--\""
        , "  }"
        , "}"
        , "DESCRIBE_EOF"
        , "    ;;"
        , "  run)"
        , "    # TODO: Implement tool logic"
        , "    # Access arguments via environment or command line"
        , "    echo \"Tool " <> toolSlug <> " executed\""
        , "    ;;"
        , "  *)"
        , "    echo \"Usage: " <> toolSlug <> " <describe|run>\" >&2"
        , "    exit 1"
        , "    ;;"
        , "esac"
        ]

-- | Create a Python tool template
makePythonToolTemplate :: Text -> Text
makePythonToolTemplate toolSlug =
    Text.unlines
        [ "#!/usr/bin/env python3"
        , "# " <> toolSlug <> " - A Python tool for agents-exe"
        , ""
        , "\"\"\"Agents-exe tool protocol: describe|run"
        , ""
        , "Environment variables available during 'run':"
        , "  AGENT_SESSION_ID      - UUID of the current session"
        , "  AGENT_CONVERSATION_ID - UUID of the conversation"
        , "  AGENT_TURN_ID         - UUID of the current turn"
        , "  AGENT_AGENT_ID        - UUID of the executing agent (if available)"
        , "\"\"\""
        , ""
        , "import json"
        , "import sys"
        , "import os"
        , ""
        , ""
        , "def describe() -> dict:"
        , "    \"\"\"Return tool description metadata.\"\"\""
        , "    return {"
        , "        \"slug\": \"" <> toolSlug <> "\","
        , "        \"description\": \"Tool " <> toolSlug <> " - describe what this tool does\","
        , "        \"args\": [],"
        , "        \"empty-result\": {"
        , "            \"tag\": \"AddMessage\","
        , "            \"contents\": \"--no output--\""
        , "        }"
        , "    }"
        , ""
        , ""
        , "def run() -> None:"
        , "    \"\"\"Execute the tool logic.\"\"\""
        , "    # TODO: Implement tool logic"
        , "    # Access environment variables:"
        , "    # session_id = os.environ.get('AGENT_SESSION_ID')"
        , "    print(f\"Tool " <> toolSlug <> " executed\")"
        , ""
        , ""
        , "def main() -> int:"
        , "    \"\"\"Main entry point.\"\"\""
        , "    if len(sys.argv) < 2:"
        , "        print(f\"Usage: {sys.argv[0]} <describe|run>\", file=sys.stderr)"
        , "        return 1"
        , ""
        , "    command = sys.argv[1]"
        , "    if command == \"describe\":"
        , "        print(json.dumps(describe()))"
        , "        return 0"
        , "    elif command == \"run\":"
        , "        run()"
        , "        return 0"
        , "    else:"
        , "        print(f\"Unknown command: {command}\", file=sys.stderr)"
        , "        return 1"
        , ""
        , ""
        , "if __name__ == \"__main__\":"
        , "    sys.exit(main())"
        ]

-- | Create a Haskell tool template
makeHaskellToolTemplate :: Text -> Text
makeHaskellToolTemplate toolSlug =
    Text.unlines
        [ "#!/usr/bin/env runhaskell"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "-- | " <> toolSlug <> " - A Haskell tool for agents-exe"
        , ""
        , "-- Agents-exe tool protocol: describe|run"
        , "-- Environment variables available during 'run':"
        , "--   AGENT_SESSION_ID      - UUID of the current session"
        , "--   AGENT_CONVERSATION_ID - UUID of the conversation"
        , "--   AGENT_TURN_ID         - UUID of the current turn"
        , "--   AGENT_AGENT_ID        - UUID of the executing agent (if available)"
        , ""
        , "import Data.Aeson ((.=), object, encode, Value)"
        , "import qualified Data.ByteString.Lazy.Char8 as LBS"
        , "import System.Environment (getArgs, lookupEnv)"
        , "import System.Exit (exitFailure)"
        , "import System.IO (hPutStrLn, stderr)"
        , ""
        , "main :: IO ()"
        , "main = do"
        , "    args <- getArgs"
        , "    case args of"
        , "        [\"describe\"] -> describe"
        , "        [\"run\"] -> run"
        , "        _ -> do"
        , "            hPutStrLn stderr \"Usage: " <> toolSlug <> " <describe|run>\""
        , "            exitFailure"
        , ""
        , "describe :: IO ()"
        , "describe = do"
        , "    LBS.putStrLn $ encode $ object"
        , "        [ \"args\" .= ([] :: [Value])"
        , "        , \"slug\" .= (\"" <> toolSlug <> "\" :: String)"
        , "        , \"description\" .= (\"Tool " <> toolSlug <> " - describe what this tool does\" :: String)"
        , "        , \"empty-result\" .= object"
        , "            [ \"tag\" .= (\"AddMessage\" :: String)"
        , "            , \"contents\" .= (\"--no output--\" :: String)"
        , "            ]"
        , "        ]"
        , ""
        , "run :: IO ()"
        , "run = do"
        , "    -- TODO: Implement tool logic"
        , "    -- Access environment variables:"
        , "    -- sessionId <- lookupEnv \"AGENT_SESSION_ID\""
        , "    putStrLn \"Tool " <> toolSlug <> " executed\""
        ]

-- | Create a Node.js tool template
makeNodeToolTemplate :: Text -> Text
makeNodeToolTemplate toolSlug =
    Text.unlines
        [ "#!/usr/bin/env node"
        , "// " <> toolSlug <> " - A Node.js tool for agents-exe"
        , ""
        , "// Agents-exe tool protocol: describe|run"
        , "// Environment variables available during 'run':"
        , "//   AGENT_SESSION_ID      - UUID of the current session"
        , "//   AGENT_CONVERSATION_ID - UUID of the conversation"
        , "//   AGENT_TURN_ID         - UUID of the current turn"
        , "//   AGENT_AGENT_ID        - UUID of the executing agent (if available)"
        , ""
        , "function describe() {"
        , "    return {"
        , "        args: [],"
        , "        slug: \"" <> toolSlug <> "\","
        , "        description: \"Tool " <> toolSlug <> " - describe what this tool does\","
        , "        'empty-result': {"
        , "            tag: 'AddMessage',"
        , "            contents: '--no output--'"
        , "        }"
        , "    };"
        , "}"
        , ""
        , "function run() {"
        , "    // TODO: Implement tool logic"
        , "    // Access environment variables:"
        , "    // const sessionId = process.env.AGENT_SESSION_ID;"
        , "    console.log('Tool " <> toolSlug <> " executed');"
        , "}"
        , ""
        , "function main() {"
        , "    const command = process.argv[2];"
        , ""
        , "    switch (command) {"
        , "        case 'describe':"
        , "            console.log(JSON.stringify(describe()));"
        , "            process.exit(0);"
        , "        case 'run':"
        , "            run();"
        , "            process.exit(0);"
        , "        default:"
        , "            console.error('Usage: " <> toolSlug <> " <describe|run>');"
        , "            process.exit(1);"
        , "    }"
        , "}"
        , ""
        , "main();"
        ]

