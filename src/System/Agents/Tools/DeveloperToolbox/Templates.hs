{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Template generation for the DeveloperToolbox.

This module provides templates for scaffolding agents and tools
in various languages and configurations.
-}
module System.Agents.Tools.DeveloperToolbox.Templates (
    -- * Agent templates
    makeAgentTemplate,
    
    -- * Tool templates
    makeToolTemplate,
    makeBashToolTemplate,
    makePythonToolTemplate,
    makeHaskellToolTemplate,
    
    -- * Config-based tool templates
    makeToolTemplateFromConfig,
    makeBashToolTemplateFromConfig,
    makePythonToolTemplateFromConfig,
    makeHaskellToolTemplateFromConfig,
    
    -- * Template helpers
    toolConfigToAeson,
    defaultDeveloperToolboxDescription,
) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

import System.Agents.Base (
    Agent (..),
    BuiltinToolboxDescription (..),
    DeveloperToolCapability (..),
    DeveloperToolboxDescription (..),
 )
import System.Agents.Tools.DeveloperToolbox.Types (
    ScriptArg (..),
    ToolConfig (..),
 )

-------------------------------------------------------------------------------
-- Agent Templates
-------------------------------------------------------------------------------

-- | Create an agent from a template
makeAgentTemplate :: Text -> Text -> Agent
makeAgentTemplate templateName agentSlug = case templateName of
    "mistral" ->
        Agent
            { slug = agentSlug
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
            , toolDirectory = Just "tools"
            , bashToolboxes = Nothing
            , mcpServers = Just []
            , openApiToolboxes = Nothing
            , postgrestToolboxes = Nothing
            , builtinToolboxes = Just [defaultDeveloperToolboxDescription]
            , extraAgents = Nothing
            , skillSources = Nothing
            , autoEnableSkills = Nothing
            }
    "ollama" ->
        Agent
            { slug = agentSlug
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
            , toolDirectory = Just "tools"
            , bashToolboxes = Nothing
            , mcpServers = Just []
            , openApiToolboxes = Nothing
            , postgrestToolboxes = Nothing
            , builtinToolboxes = Just [defaultDeveloperToolboxDescription]
            , extraAgents = Nothing
            , skillSources = Nothing
            , autoEnableSkills = Nothing
            }
    _ ->
        -- Default to OpenAI template
        Agent
            { slug = agentSlug
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
            , toolDirectory = Just "tools"
            , bashToolboxes = Nothing
            , mcpServers = Just []
            , openApiToolboxes = Nothing
            , postgrestToolboxes = Nothing
            , builtinToolboxes = Just [defaultDeveloperToolboxDescription]
            , extraAgents = Nothing
            , skillSources = Nothing
            , autoEnableSkills = Nothing
            }

-------------------------------------------------------------------------------
-- Tool Templates
-------------------------------------------------------------------------------

-- | Create a tool template for a given language
makeToolTemplate :: Text -> Text -> Text
makeToolTemplate language toolSlug = case language of
    "python" -> makePythonToolTemplate toolSlug
    "haskell" -> makeHaskellToolTemplate toolSlug
    _ -> makeBashToolTemplate toolSlug

-- | Create a bash tool template
makeBashToolTemplate :: Text -> Text
makeBashToolTemplate toolSlug =
    Text.unlines
        [ "#!/bin/bash"
        , ""
        , "# " <> toolSlug <> " - Tool description here"
        , ""
        , descLine
        , "    cat <<'EOF'"
        , "{"
        , "  \"slug\": \"" <> toolSlug <> "\","
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
        , whileLine
        , "    case " <> "$" <> "1 in"
        , "        --example-arg)"
        , "            EXAMPLE_ARG=\"" <> "$" <> "2\""
        , "            shift 2"
        , "            ;;"
        , "        *)"
        , "            shift"
        , "            ;;"
        , "    esac"
        , "done"
        , ""
        , "# Main logic here"
        , execLine
        ]
  where
    descLine = "if [ \"" <> "$" <> "1\" == \"describe\" ]; then"
    whileLine = "while [[ " <> "$" <> "# -gt 0 ]]; do"
    execLine = "echo \"Tool " <> toolSlug <> " executed with: " <> "$" <> "EXAMPLE_ARG\""

-- | Create a Python tool template
makePythonToolTemplate :: Text -> Text
makePythonToolTemplate toolSlug =
    Text.unlines
        [ "#!/usr/bin/env python3"
        , ""
        , "\"\"\"" <> toolSlug <> " - Tool description here\"\"\""
        , ""
        , "import json"
        , "import sys"
        , ""
        , "DESCRIPTION = {"
        , "    \"slug\": \"" <> toolSlug <> "\","
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
        , "    print(f'Tool " <> toolSlug <> " executed with: {example_arg}')"
        , ""
        , ""
        , "if __name__ == \"__main__\":"
        , "    main()"
        ]

-- | Create a Haskell tool template
makeHaskellToolTemplate :: Text -> Text
makeHaskellToolTemplate toolSlug =
    Text.unlines
        [ "#!/usr/bin/env runhaskell"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , ""
        , "-- | " <> toolSlug <> " - Tool description here"
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
        , "    { scriptSlug = \"" <> toolSlug <> "\""
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
        , "            Text.putStrLn $ \"Tool " <> toolSlug <> " executed with: \" <> exampleArg"
        , "        _ -> do"
        , "            Text.hPutStrLn stderr \"Usage: " <> toolSlug <> " describe|run\""
        , ""
        , "parseArg :: [String] -> Text -> Text"
        , "parseArg args name = case break (== Text.unpack name) args of"
        , "    (_, _:value:_) -> Text.pack value"
        , "    _ -> \"\""
        ]

-------------------------------------------------------------------------------
-- Config-Based Tool Templates
-------------------------------------------------------------------------------

-- | Generate tool content from config.
makeToolTemplateFromConfig :: Text -> ToolConfig -> Text
makeToolTemplateFromConfig language config = case language of
    "python" -> makePythonToolTemplateFromConfig config
    "haskell" -> makeHaskellToolTemplateFromConfig config
    _ -> makeBashToolTemplateFromConfig config

-- | Create a bash tool from config.
makeBashToolTemplateFromConfig :: ToolConfig -> Text
makeBashToolTemplateFromConfig config =
    Text.unlines $
        [ "#!/bin/bash"
        , ""
        , "# " <> toolConfigSlug config <> " - " <> toolConfigDescription config
        , ""
        , descLine
        , "    cat <<'EOF'"
        , "{"
        , "  \"slug\": \"" <> toolConfigSlug config <> "\","
        , "  \"description\": \"" <> toolConfigDescription config <> "\","
        , "  \"args\": ["
        ]
            ++ formatArgs (toolConfigArgs config)
            ++ ["  ],"]
            ++ formatEmptyResult (toolConfigEmptyResult config)
            ++ [ "}"
               , "EOF"
               , "    exit 0"
               , "fi"
               , ""
               , "# Parse arguments for 'run' command"
               ]
            ++ parseArgsCode (toolConfigArgs config)
            ++ [ ""
               , "# Main logic here"
               , execLine
               ]
  where
    -- Build the if statement line properly without escaping issues
    descLine = "if [ \"" <> "$" <> "1\" == \"describe\" ]; then"
    execLine = "echo \"Tool " <> toolConfigSlug config <> " executed\""

    formatArgs [] = ["  "]
    formatArgs args =
        let formatted = zipWith (formatArg (length args)) [0 ..] args
         in formatted

    formatArg total idx arg =
        let isLast = idx == total - 1
            comma = if isLast then "" else ","
         in "    {"
                <> "      \"name\": \""
                <> scriptArgName arg
                <> "\","
                <> "      \"description\": \""
                <> scriptArgDescription arg
                <> "\","
                <> "      \"type\": \""
                <> scriptArgType arg
                <> "\","
                <> "      \"backing_type\": \""
                <> scriptArgBackingType arg
                <> "\","
                <> "      \"arity\": \""
                <> scriptArgArity arg
                <> "\","
                <> "      \"mode\": \""
                <> scriptArgMode arg
                <> "\""
                <> "    }"
                <> comma

    formatEmptyResult Nothing = ["  \"empty-result\": { \"tag\": \"AddMessage\", \"contents\": \"No results\" }"]
    formatEmptyResult (Just val) = ["  \"empty-result\": " <> TextEncoding.decodeUtf8 (LByteString.toStrict (AesonPretty.encodePretty val))]

    parseArgsCode [] = []
    parseArgsCode args = concatMap parseOneArg args

    parseOneArg arg =
        let varName = Text.toUpper $ Text.replace "-" "_" (scriptArgName arg)
            argFlag = "--" <> scriptArgName arg
         in case scriptArgMode arg of
                "stdin" ->
                    [ varName <> "=$(cat)"
                    ]
                "positional" ->
                    [ "# Positional argument: " <> scriptArgName arg
                    ]
                "dashdashequal" ->
                    [ varName <> "=\"\""
                    , "while [[ " <> "$" <> "# -gt 0 ]]; do"
                    , "    case " <> "$" <> "1 in"
                    , "        " <> argFlag <> "=*)"
                    , "            " <> varName <> "=\"${" <> "1#*=}\""
                    , "            shift"
                    , "            ;;"
                    , "        *)"
                    , "            shift"
                    , "            ;;"
                    , "    esac"
                    , "done"
                    ]
                _ ->
                    -- dashdashspace (default)
                    [ varName <> "=\"\""
                    , "while [[ " <> "$" <> "# -gt 0 ]]; do"
                    , "    case " <> "$" <> "1 in"
                    , "        " <> argFlag <> ")"
                    , "            " <> varName <> "=\"" <> "$" <> "2\""
                    , "            shift 2"
                    , "            ;;"
                    , "        *)"
                    , "            shift"
                    , "            ;;"
                    , "    esac"
                    , "done"
                    ]

-- | Create a Python tool from config.
makePythonToolTemplateFromConfig :: ToolConfig -> Text
makePythonToolTemplateFromConfig config =
    let argsJson = TextEncoding.decodeUtf8 $ LByteString.toStrict $ AesonPretty.encodePretty (toolConfigArgs config)
        emptyResultJson = case toolConfigEmptyResult config of
            Nothing -> "{\"tag\": \"AddMessage\", \"contents\": \"No results\"}"
            Just val -> TextEncoding.decodeUtf8 $ LByteString.toStrict $ AesonPretty.encodePretty val
     in Text.unlines
            [ "#!/usr/bin/env python3"
            , ""
            , "\"\"\"" <> toolConfigSlug config <> " - " <> toolConfigDescription config <> "\"\"\""
            , ""
            , "import json"
            , "import sys"
            , ""
            , "DESCRIPTION = {"
            , "    \"slug\": \"" <> toolConfigSlug config <> "\","
            , "    \"description\": \"" <> toolConfigDescription config <> "\","
            , "    \"args\": " <> argsJson <> ","
            , "    \"empty-result\": " <> emptyResultJson
            , "}"
            , ""
            , ""
            , "def main():"
            , "    if len(sys.argv) > 1 and sys.argv[1] == \"describe\":"
            , "        print(json.dumps(DESCRIPTION))"
            , "        sys.exit(0)"
            , ""
            , "    # Parse arguments"
            , "    # TODO: Implement argument parsing"
            , ""
            , "    # Main logic here"
            , "    print(\"Tool " <> toolConfigSlug config <> " executed\")"
            , ""
            , ""
            , "if __name__ == \"__main__\":"
            , "    main()"
            ]

-- | Create a Haskell tool from config.
makeHaskellToolTemplateFromConfig :: ToolConfig -> Text
makeHaskellToolTemplateFromConfig config =
    let descValue = TextEncoding.decodeUtf8 (LByteString.toStrict (AesonPretty.encodePretty (toolConfigToAeson config)))
     in Text.unlines
            [ "#!/usr/bin/env runhaskell"
            , "{-# LANGUAGE OverloadedStrings #-}"
            , ""
            , "-- | " <> toolConfigSlug config <> " - " <> toolConfigDescription config
            , ""
            , "import qualified Data.Aeson as Aeson"
            , "import qualified Data.ByteString.Lazy as LBS"
            , "import Data.Text (Text)"
            , "import qualified Data.Text as Text"
            , "import qualified Data.Text.IO as Text"
            , "import System.Environment (getArgs)"
            , ""
            , "main :: IO ()"
            , "main = do"
            , "    args <- getArgs"
            , "    case args of"
            , "        (\"describe\":_) -> do"
            , "            LBS.putStr $ Aeson.encode description"
            , "        (\"run\":_) -> do"
            , "            -- TODO: Implement argument parsing and main logic"
            , "            Text.putStrLn \"Tool " <> toolConfigSlug config <> " executed\""
            , "        _ -> do"
            , "            Text.hPutStrLn stderr \"Usage: " <> toolConfigSlug config <> " describe|run\""
            , ""
            , "description :: Aeson.Value"
            , "description = " <> descValue
            ]

-- | Convert ToolConfig to Aeson Value for template generation.
toolConfigToAeson :: ToolConfig -> Aeson.Value
toolConfigToAeson config =
    Aeson.object
        [ "slug" .= toolConfigSlug config
        , "description" .= toolConfigDescription config
        , "args" .= toolConfigArgs config
        , "empty-result" .= toolConfigEmptyResult config
        ]

-------------------------------------------------------------------------------
-- Default Configuration
-------------------------------------------------------------------------------

-- | Default developer toolbox configuration for agent templates.
defaultDeveloperToolboxDescription :: BuiltinToolboxDescription
defaultDeveloperToolboxDescription =
    DeveloperToolbox $
        DeveloperToolboxDescription
            { developerToolboxName = "developer"
            , developerToolboxDescription = "Tools for developing agents and tools"
            , developerToolboxCapabilities =
                [ DevToolValidateTool
                , DevToolScaffoldAgent
                , DevToolScaffoldTool
                , DevToolShowSpec
                , DevToolValidateAgent
                , DevToolCreateAgent
                , DevToolCreateTool
                , DevToolReadFileRange
                , DevToolWriteFileRange
                , DevToolPatchFile
                ]
            , developerToolboxActivation = Nothing -- Uses default: AlwaysActivated
            , developerToolboxFileSandbox = Nothing -- Uses default: deny all
            }

