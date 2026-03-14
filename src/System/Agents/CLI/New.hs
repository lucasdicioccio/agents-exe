{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'new' command handler.

The new command provides scaffolding for creating new agents and tools.
-}
module System.Agents.CLI.New (
    handleNew,
    NewOptions (..),
    NewCommand (..),
    AgentTemplate (..),
    ToolLanguage (..),
) where

import Control.Exception (catch)
import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr)

import System.Agents.Base (Agent (..), AgentDescription (..))

-- | Template for agent scaffolding
data AgentTemplate
    = OpenAITemplate
    | MistralTemplate
    | OllamaTemplate
    deriving (Show, Eq)

-- | Programming language for tool scaffolding
data ToolLanguage
    = BashLang
    | PythonLang
    | HaskellLang
    deriving (Show, Eq)

-- | Subcommands for the 'new' command
data NewCommand
    = NewAgent AgentTemplate Text FilePath
    -- ^ Create a new agent with given template, slug, and file path
    | NewTool ToolLanguage Text FilePath
    -- ^ Create a new tool with given language, slug, and file path
    deriving (Show, Eq)

-- | Options for the new command
data NewOptions = NewOptions
    { newCommand :: NewCommand
    , newForce :: Bool
    -- ^ Overwrite existing files
    }
    deriving (Show, Eq)

-- | Handle the new command: create agent or tool scaffolding
handleNew ::
    -- | Options for new command
    NewOptions ->
    IO ()
handleNew opts = case opts.newCommand of
    NewAgent template slug filePath ->
        handleNewAgent opts.newForce template slug filePath
    NewTool language slug filePath ->
        handleNewTool opts.newForce language slug filePath

-- | Create a new agent from a template
handleNewAgent :: Bool -> AgentTemplate -> Text -> FilePath -> IO ()
handleNewAgent force template slug filePath = do
    unless force $ do
        exists <- doesFileExist filePath
        when exists $ do
            Text.hPutStrLn stderr $
                "File already exists: "
                    <> Text.pack filePath
                    <> "\nUse --force to overwrite."
            exitFailure

    let agent = makeAgentTemplate template slug
    createDirectoryIfMissing True (takeDirectory filePath)
    LByteString.writeFile filePath $ Aeson.encodePretty (AgentDescription agent)
    Text.putStrLn $ "Created agent: " <> Text.pack filePath
    Text.putStrLn $ "  Slug: " <> agent.slug
    Text.putStrLn $ "  Model: " <> agent.modelName
    Text.putStrLn $ "  Tool directory: " <> Text.pack agent.toolDirectory

-- | Create a new tool from a template
handleNewTool :: Bool -> ToolLanguage -> Text -> FilePath -> IO ()
handleNewTool force language slug filePath = do
    unless force $ do
        exists <- doesFileExist filePath
        when exists $ do
            Text.hPutStrLn stderr $
                "File already exists: "
                    <> Text.pack filePath
                    <> "\nUse --force to overwrite."
            exitFailure

    createDirectoryIfMissing True (takeDirectory filePath)
    let content = makeToolTemplate language slug
    Text.writeFile filePath content
    -- Make executable for bash scripts
    case language of
        BashLang -> do
            -- Set executable permissions would need unix module
            -- For now, just inform the user
            Text.putStrLn $ "Created tool: " <> Text.pack filePath
            Text.putStrLn $ "  Remember to make it executable: chmod +x " <> Text.pack filePath
        _ -> do
            Text.putStrLn $ "Created tool: " <> Text.pack filePath
    Text.putStrLn $ "  Slug: " <> slug
    Text.putStrLn $ "  Language: " <> Text.pack (show language)

-- | Create an agent from a template
makeAgentTemplate :: AgentTemplate -> Text -> Agent
makeAgentTemplate template slug = case template of
    OpenAITemplate ->
        Agent
            { slug = slug
            , apiKeyId = "main-key"
            , flavor = "OpenAIv1"
            , modelUrl = "https://api.openai.com/v1"
            , modelName = "gpt-4-turbo-preview"
            , announce = "a helpful assistant powered by OpenAI"
            , systemPrompt =
                [ "You are a helpful assistant."
                , "You provide clear, accurate, and concise responses."
                , "When using tools, you explain your actions to the user."
                ]
            , toolDirectory = "tools"
            , mcpServers = Just []
            , openApiToolboxes = Nothing
            , postgrestToolboxes = Nothing
            , builtinToolboxes = Just []
            , extraAgents = Nothing
            }
    MistralTemplate ->
        Agent
            { slug = slug
            , apiKeyId = "mistral-key"
            , flavor = "OpenAIv1"
            , modelUrl = "https://api.mistral.ai/v1"
            , modelName = "mistral-large-latest"
            , announce = "a helpful assistant powered by Mistral AI"
            , systemPrompt =
                [ "You are a helpful assistant powered by Mistral AI."
                , "You provide clear, accurate, and concise responses."
                , "You excel at reasoning and following instructions precisely."
                ]
            , toolDirectory = "tools"
            , mcpServers = Just []
            , openApiToolboxes = Nothing
            , postgrestToolboxes = Nothing
            , builtinToolboxes = Just []
            , extraAgents = Nothing
            }
    OllamaTemplate ->
        Agent
            { slug = slug
            , apiKeyId = "ollama-key"
            , flavor = "OpenAIv1"
            , modelUrl = "http://localhost:11434/v1"
            , modelName = "llama3.2"
            , announce = "a helpful assistant running locally via Ollama"
            , systemPrompt =
                [ "You are a helpful assistant running locally on the user's machine."
                , "You provide clear and accurate responses while respecting privacy."
                , "Note: You are running on local hardware, which may limit capabilities."
                ]
            , toolDirectory = "tools"
            , mcpServers = Just []
            , openApiToolboxes = Nothing
            , postgrestToolboxes = Nothing
            , builtinToolboxes = Just []
            , extraAgents = Nothing
            }

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

