{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'new' command handler.

The new command provides scaffolding for creating new agents and tools.
-}
module System.Agents.CLI.New (
    handleNew,
    NewOptions (..),
    NewCommand (..),
    NewAgentOptions (..),
    NewToolOptions (..),
    ModelPreset (..),
    ToolLanguage (..),
    -- Exported for testing
    buildAgentConfig,
    defaultPresets,
    defaultSystemPrompt,
    toolLanguageToExtension,
    makeToolTemplate,
    supportedLanguages,
) where

import Control.Monad (unless, when)
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO (stderr)
import System.Posix.Files (ownerExecuteMode, ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)

import System.Agents.Base (Agent (..), AgentDescription (..))

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
    -- ^ Optional model name override
    , newAgentPreset :: Text
    -- ^ Preset name (openai, mistral, ollama)
    }
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

-- | Subcommands for the 'new' command
data NewCommand
    = -- | Create a new agent with given options
      NewAgent NewAgentOptions
    | -- | Create a new tool with given options
      NewTool NewToolOptions
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
        ]

-- | Default system prompt based on agent slug
defaultSystemPrompt :: Text -> [Text]
defaultSystemPrompt agentSlug =
    [ "You are " <> agentSlug <> ", a helpful AI assistant."
    , "You provide clear, accurate, and concise responses."
    , "When using tools, you explain your actions to the user."
    ]

-- | Build agent configuration from options
buildAgentConfig :: NewAgentOptions -> Either String Agent
buildAgentConfig opts = do
    preset <- case Map.lookup opts.newAgentPreset defaultPresets of
        Nothing -> Left $ "Unknown preset: " ++ Text.unpack opts.newAgentPreset
        Just p -> Right p

    let selectedModelName = fromMaybe preset.presetModelName opts.newAgentModel

    pure $
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
            , builtinToolboxes = Just []
            , extraAgents = Nothing
            , skillSources = Nothing
            , autoEnableSkills = Nothing
            }

-- | Handle the new command: create agent or tool scaffolding
handleNew ::
    -- | Options for new command
    NewOptions ->
    IO ()
handleNew opts = case opts.newCommand of
    NewAgent agentOpts ->
        handleNewAgent opts.newForce agentOpts
    NewTool toolOpts ->
        handleNewTool opts.newForce toolOpts

-- | Handle the new agent command
handleNewAgent :: Bool -> NewAgentOptions -> IO ()
handleNewAgent force opts = do
    -- Check if file already exists
    unless force $ do
        exists <- doesFileExist opts.newAgentFilePath
        when exists $ do
            Text.hPutStrLn stderr $
                "Error: File already exists: " <> Text.pack opts.newAgentFilePath
            Text.hPutStrLn stderr "Use --force to overwrite"
            exitFailure

    -- Build agent config
    case buildAgentConfig opts of
        Left err -> do
            Text.hPutStrLn stderr $ "Error: " <> Text.pack err
            exitFailure
        Right agent -> do
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
            Text.putStrLn $ "Provider: " <> opts.newAgentPreset

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
