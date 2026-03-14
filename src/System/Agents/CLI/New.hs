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
) where

import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
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
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr)

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

-- | Options for new tool command
data NewToolOptions = NewToolOptions
    { newToolSlug :: Text
    , newToolFilePath :: FilePath
    , newToolLanguage :: ToolLanguage
    }
    deriving (Show, Eq)

-- | Programming language for tool scaffolding
data ToolLanguage
    = BashLang
    | PythonLang
    | HaskellLang
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
defaultSystemPrompt slug =
    [ "You are " <> slug <> ", a helpful AI assistant."
    , "You provide clear, accurate, and concise responses."
    , "When using tools, you explain your actions to the user."
    ]

-- | Build agent configuration from options
buildAgentConfig :: NewAgentOptions -> Either String Agent
buildAgentConfig opts = do
    preset <- case Map.lookup opts.newAgentPreset defaultPresets of
        Nothing -> Left $ "Unknown preset: " ++ Text.unpack opts.newAgentPreset
        Just p -> Right p

    let modelName = fromMaybe preset.presetModelName opts.newAgentModel

    pure $
        Agent
            { slug = opts.newAgentSlug
            , apiKeyId = preset.presetApiKeyId
            , flavor = preset.presetFlavor
            , modelUrl = preset.presetModelUrl
            , modelName = modelName
            , announce = "a helpful assistant powered by " <> modelName
            , systemPrompt = defaultSystemPrompt opts.newAgentSlug
            , toolDirectory = "tools"
            , mcpServers = Just []
            , openApiToolboxes = Nothing
            , postgrestToolboxes = Nothing
            , builtinToolboxes = Just []
            , extraAgents = Nothing
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
            createDirectoryIfMissing True (takeDirectory opts.newAgentFilePath </> agent.toolDirectory)

            -- Write agent file
            LByteString.writeFile opts.newAgentFilePath $
                Aeson.encodePretty (AgentDescription agent)

            Text.putStrLn $ "Created agent: " <> Text.pack opts.newAgentFilePath
            Text.putStrLn $ "Tool directory: " <> Text.pack (takeDirectory opts.newAgentFilePath </> agent.toolDirectory)
            Text.putStrLn $ "Model: " <> agent.modelName
            Text.putStrLn $ "Provider: " <> opts.newAgentPreset

-- | Handle the new tool command
handleNewTool :: Bool -> NewToolOptions -> IO ()
handleNewTool force opts = do
    unless force $ do
        exists <- doesFileExist opts.newToolFilePath
        when exists $ do
            Text.hPutStrLn stderr $
                "File already exists: "
                    <> Text.pack opts.newToolFilePath
                    <> "\nUse --force to overwrite."
            exitFailure

    createDirectoryIfMissing True (takeDirectory opts.newToolFilePath)
    let content = makeToolTemplate opts.newToolLanguage opts.newToolSlug
    Text.writeFile opts.newToolFilePath content
    -- Make executable for bash scripts
    case opts.newToolLanguage of
        BashLang -> do
            -- Set executable permissions would need unix module
            -- For now, just inform the user
            Text.putStrLn $ "Created tool: " <> Text.pack opts.newToolFilePath
            Text.putStrLn $ "  Remember to make it executable: chmod +x " <> Text.pack opts.newToolFilePath
        _ -> do
            Text.putStrLn $ "Created tool: " <> Text.pack opts.newToolFilePath
    Text.putStrLn $ "  Slug: " <> opts.newToolSlug
    Text.putStrLn $ "  Language: " <> Text.pack (show opts.newToolLanguage)

-- | Create a tool template for a given language
makeToolTemplate :: ToolLanguage -> Text -> Text
makeToolTemplate language slug = case language of
    PythonLang -> makePythonToolTemplate slug
    HaskellLang -> makeHaskellToolTemplate slug
    BashLang -> makeBashToolTemplate slug

-- | Create a bash tool template
makeBashToolTemplate :: Text -> Text
makeBashToolTemplate slug =
    Text.unlines
        [ "#!/bin/bash"
        , ""
        , "# " <> slug <> " - Tool description here"
        , ""
        , "if [ \"$1\" == \"describe\" ]; then"
        , "    cat <<'EOF'"
        , "{"
        , "  \"slug\": \"" <> slug <> "\","
        , "  \"description\": \"Description of what this tool does\","
        , "  \"args\": ["
        , "    {"
        , "      \"name\": \"example_arg\","
        , "      \"description\": \"An example argument\","
        , "      \"type\": \"string\","
        , "      \"backing_type\": \"string\","
        , "      \"arity\": \"single\","
        , "      \"mode\": \"dashdashspace\""
        , "    }"
        , "  ],"
        , "  \"empty-result\": { \"tag\": \"AddMessage\", \"contents\": \"No results\" }"
        , "}"
        , "EOF"
        , "    exit 0"
        , "fi"
        , ""
        , "# Parse arguments for 'run' command"
        , "EXAMPLE_ARG=\"\""
        , "while [[ $# -gt 0 ]]; do"
        , "    case $1 in"
        , "        --example-arg)"
        , "            EXAMPLE_ARG=\"$2\""
        , "            shift 2"
        , "            ;;"
        , "        *)"
        , "            shift"
        , "            ;;"
        , "    esac"
        , "done"
        , ""
        , "# Main logic here"
        , "echo \"Tool " <> slug <> " executed with: $EXAMPLE_ARG\""
        ]

-- | Create a Python tool template
makePythonToolTemplate :: Text -> Text
makePythonToolTemplate slug =
    Text.unlines
        [ "#!/usr/bin/env python3"
        , ""
        , "\"\"\"" <> slug <> " - Tool description here\"\"\""
        , ""
        , "import json"
        , "import sys"
        , ""
        , "DESCRIPTION = {"
        , "    \"slug\": \"" <> slug <> "\","
        , "    \"description\": \"Description of what this tool does\","
        , "    \"args\": ["
        , "        {"
        , "            \"name\": \"example_arg\","
        , "            \"description\": \"An example argument\","
        , "            \"type\": \"string\","
        , "            \"backing_type\": \"string\","
        , "            \"arity\": \"single\","
        , "            \"mode\": \"dashdashspace\""
        , "        }"
        , "    ],"
        , "    \"empty-result\": {\"tag\": \"AddMessage\", \"contents\": \"No results\"}"
        , "}"
        , ""
        , ""
        , "def main():"
        , "    if len(sys.argv) > 1 and sys.argv[1] == \"describe\":"
        , "        print(json.dumps(DESCRIPTION))"
        , "        sys.exit(0)"
        , ""
        , "    # Parse arguments"
        , "    example_arg = \"\""
        , "    i = 2  # Skip 'run' command"
        , "    while i < len(sys.argv):"
        , "        if sys.argv[i] == \"--example-arg\" and i + 1 < len(sys.argv):"
        , "            example_arg = sys.argv[i + 1]"
        , "            i += 2"
        , "        else:"
        , "            i += 1"
        , ""
        , "    # Main logic here"
        , "    print(f'Tool " <> slug <> " executed with: {example_arg}')"
        , ""
        , ""
        , "if __name__ == \"__main__\":"
        , "    main()"
        ]

-- | Create a Haskell tool template
makeHaskellToolTemplate :: Text -> Text
makeHaskellToolTemplate slug =
    Text.unlines
        [ "#!/usr/bin/env runhaskell"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , ""
        , "-- | " <> slug <> " - Tool description here"
        , ""
        , "import qualified Data.Aeson as Aeson"
        , "import qualified Data.ByteString.Lazy as LBS"
        , "import Data.Text (Text)"
        , "import qualified Data.Text as Text"
        , "import qualified Data.Text.IO as Text"
        , "import System.Environment (getArgs)"
        , ""
        , "-- | Tool description"
        , "data ScriptInfo = ScriptInfo"
        , "    { scriptSlug :: Text"
        , "    , scriptDescription :: Text"
        , "    , scriptArgs :: [ScriptArg]"
        , "    } deriving (Show)"
        , ""
        , "data ScriptArg = ScriptArg"
        , "    { argName :: Text"
        , "    , argDescription :: Text"
        , "    , argType :: Text"
        , "    , argBackingType :: Text"
        , "    , argArity :: Text"
        , "    , argMode :: Text"
        , "    } deriving (Show)"
        , ""
        , "instance Aeson.ToJSON ScriptInfo where"
        , "    toJSON s = Aeson.object"
        , "        [ \"slug\" Aeson..= scriptSlug s"
        , "        , \"description\" Aeson..= scriptDescription s"
        , "        , \"args\" Aeson..= scriptArgs s"
        , "        ]"
        , ""
        , "instance Aeson.ToJSON ScriptArg where"
        , "    toJSON a = Aeson.object"
        , "        [ \"name\" Aeson..= argName a"
        , "        , \"description\" Aeson..= argDescription a"
        , "        , \"type\" Aeson..= argType a"
        , "        , \"backing_type\" Aeson..= argBackingType a"
        , "        , \"arity\" Aeson..= argArity a"
        , "        , \"mode\" Aeson..= argMode a"
        , "        ]"
        , ""
        , "description :: ScriptInfo"
        , "description = ScriptInfo"
        , "    { scriptSlug = \"" <> slug <> "\""
        , "    , scriptDescription = \"Description of what this tool does\""
        , "    , scriptArgs ="
        , "        [ ScriptArg"
        , "            { argName = \"example_arg\""
        , "            , argDescription = \"An example argument\""
        , "            , argType = \"string\""
        , "            , argBackingType = \"string\""
        , "            , argArity = \"single\""
        , "            , argMode = \"dashdashspace\""
        , "            }"
        , "        ]"
        , "    }"
        , ""
        , "main :: IO ()"
        , "main = do"
        , "    args <- getArgs"
        , "    case args of"
        , "        (\"describe\":_) -> do"
        , "            LBS.putStr $ Aeson.encode description"
        , "        (\"run\":_) -> do"
        , "            -- Parse arguments and execute"
        , "            let exampleArg = parseArg args \"--example-arg\""
        , "            Text.putStrLn $ \"Tool " <> slug <> " executed with: \" <> exampleArg"
        , "        _ -> do"
        , "            Text.hPutStrLn stderr \"Usage: " <> slug <> " describe|run\""
        , ""
        , "parseArg :: [String] -> Text -> Text"
        , "parseArg args name = case break (== Text.unpack name) args of"
        , "    (_, _:value:_) -> Text.pack value"
        , "    _ -> \"\""
        ]
