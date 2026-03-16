{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Provides a runtime for developer tools.

This module implements the developer toolbox functionality, providing tools
for:

* Validating tool scripts
* Generating agent scaffolding
* Generating tool scaffolding
* Accessing specification documentation

These tools help developers write and validate agents and tools.

Example usage:

@
import System.Agents.Tools.DeveloperToolbox as Dev
import System.Agents.Base (DeveloperToolCapability(..))

main :: IO ()
main = do
    let desc = DeveloperToolboxDescription
            { developerToolboxName = "developer"
            , developerToolboxDescription = "Development tools"
            , developerToolboxCapabilities = [DevToolValidateTool, DevToolScaffoldAgent]
            }
    result <- Dev.initializeToolbox tracer desc
    case result of
        Right toolbox -> do
            -- Validate a tool script
            result <- Dev.executeValidateTool toolbox "/path/to/tool.sh"
            case result of
                Right valResult -> print valResult
                Left err -> print err
        Left err -> putStrLn $ "Failed to initialize: " ++ err
@
-}
module System.Agents.Tools.DeveloperToolbox (
    -- * Core types
    Trace (..),
    Toolbox (..),
    ToolDescription (..),
    DeveloperToolError (..),
    ValidationResult (..),
    ScaffoldResult (..),

    -- * Initialization
    initializeToolbox,

    -- * Tool execution
    executeValidateTool,
    executeScaffoldAgent,
    executeScaffoldTool,
    executeShowSpec,

    -- * Capability info
    getCapabilityInfo,
    capabilityToName,
    capabilityFromName,
) where

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.FileEmbed (embedStringFile)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)

import Prod.Tracer (Tracer (..))

import System.Agents.Base (
    Agent (..),
    AgentDescription (..),
    DeveloperToolCapability (..),
    DeveloperToolboxDescription (..),
 )
import qualified System.Agents.Tools.Bash as Bash

-------------------------------------------------------------------------------
-- Core Types
-------------------------------------------------------------------------------

{- | Trace events for monitoring developer toolbox operations.

These events allow tracking of:
* Tool validation attempts
* Scaffold generation
* Spec access
-}
data Trace
    = -- | Tool validation started
      ValidateToolStartedTrace !FilePath
    | -- | Tool validation completed
      ValidateToolCompletedTrace !FilePath !Bool
    | -- | Scaffold agent started
      ScaffoldAgentStartedTrace !Text !FilePath
    | -- | Scaffold agent completed
      ScaffoldAgentCompletedTrace !Text !FilePath
    | -- | Scaffold tool started
      ScaffoldToolStartedTrace !Text !FilePath
    | -- | Scaffold tool completed
      ScaffoldToolCompletedTrace !Text !FilePath
    | -- | Show spec started
      ShowSpecStartedTrace !Text
    | -- | Show spec completed
      ShowSpecCompletedTrace !Text
    | -- | Error during operation
      DeveloperToolErrorTrace !Text !Text
    deriving (Show)

{- | Description of a developer tool.

Contains metadata about a specific developer tool.
-}
data ToolDescription = ToolDescription
    { toolDescriptionName :: Text
    -- ^ Name of the tool
    , toolDescriptionDescription :: Text
    -- ^ Human-readable description
    , toolDescriptionToolboxName :: Text
    -- ^ Name of the toolbox this tool belongs to
    }
    deriving (Show)

{- | Runtime state for a developer toolbox.

The toolbox maintains:
* Toolbox name and description
* List of enabled capabilities
-}
data Toolbox = Toolbox
    { toolboxName :: Text
    , toolboxDescription :: Text
    , toolboxCapabilities :: [DeveloperToolCapability]
    }

-- | Result of a tool validation.
data ValidationResult = ValidationResult
    { validationPath :: FilePath
    , validationValid :: Bool
    , validationSlug :: Maybe Text
    , validationError :: Maybe Text
    }
    deriving (Show)

-- | JSON serialization for ValidationResult.
instance ToJSON ValidationResult where
    toJSON result =
        Aeson.object
            [ "path" .= validationPath result
            , "valid" .= validationValid result
            , "slug" .= validationSlug result
            , "error" .= validationError result
            ]

-- | Result of a scaffolding operation.
data ScaffoldResult = ScaffoldResult
    { scaffoldSuccess :: Bool
    , scaffoldPath :: FilePath
    , scaffoldError :: Maybe Text
    }
    deriving (Show)

-- | JSON serialization for ScaffoldResult.
instance ToJSON ScaffoldResult where
    toJSON result =
        Aeson.object
            [ "success" .= scaffoldSuccess result
            , "path" .= scaffoldPath result
            , "error" .= scaffoldError result
            ]

-- | Errors that can occur during developer tool operations.
data DeveloperToolError
    = -- | The requested capability is not enabled
      CapabilityNotEnabledError !Text
    | -- | Error during tool validation
      ValidationError !Text
    | -- | Error during scaffolding
      ScaffoldError !Text
    | -- | File already exists
      FileExistsError !FilePath
    | -- | Invalid template or language
      InvalidTemplateError !Text
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

{- | Initialize a developer toolbox from a description.

This function creates a 'Toolbox' value from a 'DeveloperToolboxDescription',
validating the configuration and preparing the runtime state.

Returns an error if the configuration is invalid.
-}
initializeToolbox ::
    Tracer IO Trace ->
    DeveloperToolboxDescription ->
    IO (Either String Toolbox)
initializeToolbox _tracer desc = do
    -- Validate that we have at least one capability
    if null desc.developerToolboxCapabilities
        then pure $ Left "Developer toolbox must have at least one capability enabled"
        else do
            let toolbox =
                    Toolbox
                        { toolboxName = desc.developerToolboxName
                        , toolboxDescription = desc.developerToolboxDescription
                        , toolboxCapabilities = desc.developerToolboxCapabilities
                        }
            pure $ Right toolbox

-------------------------------------------------------------------------------
-- Capability Info
-------------------------------------------------------------------------------

-- | Get the name for a capability.
capabilityToName :: DeveloperToolCapability -> Text
capabilityToName DevToolValidateTool = "validate-tool"
capabilityToName DevToolScaffoldAgent = "scaffold-agent"
capabilityToName DevToolScaffoldTool = "scaffold-tool"
capabilityToName DevToolShowSpec = "show-spec"

-- | Convert a capability name text to the corresponding DeveloperToolCapability.
capabilityFromName :: Text -> Maybe DeveloperToolCapability
capabilityFromName name = case name of
    "validate-tool" -> Just DevToolValidateTool
    "scaffold-agent" -> Just DevToolScaffoldAgent
    "scaffold-tool" -> Just DevToolScaffoldTool
    "show-spec" -> Just DevToolShowSpec
    _ -> Nothing

-- | Get information about a capability (name and description).
getCapabilityInfo :: DeveloperToolCapability -> (Text, Text)
getCapabilityInfo DevToolValidateTool =
    ( "validate-tool"
    , "Validates a bash tool script by loading its description"
    )
getCapabilityInfo DevToolScaffoldAgent =
    ( "scaffold-agent"
    , "Generates agent scaffolding from a template (openai, mistral, ollama)"
    )
getCapabilityInfo DevToolScaffoldTool =
    ( "scaffold-tool"
    , "Generates tool scaffolding in a language (bash, python, haskell)"
    )
getCapabilityInfo DevToolShowSpec =
    ( "show-spec"
    , "Shows specification documentation (bash-tools)"
    )

-------------------------------------------------------------------------------
-- Tool Execution
-------------------------------------------------------------------------------

{- | Execute tool validation.

This function validates a bash tool script by attempting to load its
description using the standard 'describe' command.

Returns:
* 'Right ValidationResult' on successful validation (valid or invalid)
* 'Left DeveloperToolError' if capability not enabled
-}
executeValidateTool ::
    Toolbox ->
    FilePath ->
    IO (Either DeveloperToolError ValidationResult)
executeValidateTool toolbox toolPath = do
    if DevToolValidateTool `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "validate-tool"
        else do
            result <- try $ Bash.loadScript (Tracer (const (pure ()))) toolPath
            case result of
                Left (e :: SomeException) -> do
                    let errMsg = Text.pack $ show e
                    pure $
                        Right $
                            ValidationResult
                                { validationPath = toolPath
                                , validationValid = False
                                , validationSlug = Nothing
                                , validationError = Just errMsg
                                }
                Right (Left _err) -> do
                    let errMsg = "Failed to load script"
                    pure $
                        Right $
                            ValidationResult
                                { validationPath = toolPath
                                , validationValid = False
                                , validationSlug = Nothing
                                , validationError = Just errMsg
                                }
                Right (Right scriptDesc) -> do
                    pure $
                        Right $
                            ValidationResult
                                { validationPath = toolPath
                                , validationValid = True
                                , validationSlug = Just scriptDesc.scriptInfo.scriptSlug
                                , validationError = Nothing
                                }

{- | Execute agent scaffolding.

This function generates agent scaffolding from a template.

Returns:
* 'Right ScaffoldResult' on success or failure with error
* 'Left DeveloperToolError' if capability not enabled
-}
executeScaffoldAgent ::
    Toolbox ->
    -- | Template name (openai, mistral, ollama)
    Text ->
    -- | Agent slug
    Text ->
    -- | Output file path
    FilePath ->
    -- | Force overwrite
    Bool ->
    IO (Either DeveloperToolError ScaffoldResult)
executeScaffoldAgent toolbox templateName agentSlug filePath force = do
    if DevToolScaffoldAgent `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "scaffold-agent"
        else do
            unless force $ do
                exists <- doesFileExist filePath
                when exists $ do
                    error $ "File already exists: " <> filePath
            result <- try $ do
                let agent = makeAgentTemplate templateName agentSlug
                createDirectoryIfMissing True (takeDirectory filePath)
                LByteString.writeFile filePath $
                    Aeson.encodePretty (AgentDescription agent)
            case result of
                Left (e :: SomeException) ->
                    pure $
                        Right $
                            ScaffoldResult
                                { scaffoldSuccess = False
                                , scaffoldPath = filePath
                                , scaffoldError = Just $ Text.pack $ show e
                                }
                Right () ->
                    pure $
                        Right $
                            ScaffoldResult
                                { scaffoldSuccess = True
                                , scaffoldPath = filePath
                                , scaffoldError = Nothing
                                }

{- | Execute tool scaffolding.

This function generates tool scaffolding in a given language.

Returns:
* 'Right ScaffoldResult' on success or failure with error
* 'Left DeveloperToolError' if capability not enabled
-}
executeScaffoldTool ::
    Toolbox ->
    -- | Language (bash, python, haskell)
    Text ->
    -- | Tool slug
    Text ->
    -- | Output file path
    FilePath ->
    -- | Force overwrite
    Bool ->
    IO (Either DeveloperToolError ScaffoldResult)
executeScaffoldTool toolbox language toolSlug filePath force = do
    if DevToolScaffoldTool `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "scaffold-tool"
        else do
            unless force $ do
                exists <- doesFileExist filePath
                when exists $ do
                    error $ "File already exists: " <> filePath
            result <- try $ do
                let content = makeToolTemplate language toolSlug
                createDirectoryIfMissing True (takeDirectory filePath)
                Text.writeFile filePath content
            case result of
                Left (e :: SomeException) ->
                    pure $
                        Right $
                            ScaffoldResult
                                { scaffoldSuccess = False
                                , scaffoldPath = filePath
                                , scaffoldError = Just $ Text.pack $ show e
                                }
                Right () ->
                    pure $
                        Right $
                            ScaffoldResult
                                { scaffoldSuccess = True
                                , scaffoldPath = filePath
                                , scaffoldError = Nothing
                                }

-- | Embedded bash-tools documentation
bashToolsDocumentation :: Text
bashToolsDocumentation = $(embedStringFile "docs/binary-tool.md")

{- | Execute show spec.

This function returns specification documentation.

Returns:
* 'Right Text' with the spec content
* 'Left DeveloperToolError' if capability not enabled or unknown spec
-}
executeShowSpec ::
    Toolbox ->
    -- | Spec name (bash-tools)
    Text ->
    IO (Either DeveloperToolError Text)
executeShowSpec toolbox specName = do
    if DevToolShowSpec `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "show-spec"
        else case specName of
            "bash-tools" -> pure $ Right bashToolsDocumentation
            _ -> pure $ Left $ InvalidTemplateError $ "Unknown spec: " <> specName

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
            , builtinToolboxes = Just []
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
            , builtinToolboxes = Just []
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
            , builtinToolboxes = Just []
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
        , "if [ \"$1\" == \"describe\" ]; then"
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
        , "echo \"Tool " <> toolSlug <> " executed with: $EXAMPLE_ARG\""
        ]

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
