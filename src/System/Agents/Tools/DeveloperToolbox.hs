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
* Validating agent configurations
* Creating agent configurations
* Creating tool scripts

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
    AgentValidationResult (..),
    CreateResult (..),
    AgentOverrides (..),
    ToolConfig (..),
    ScriptArg (..),
    defaultAgentOverrides,

    -- * Initialization
    initializeToolbox,

    -- * Tool execution
    executeValidateTool,
    executeScaffoldAgent,
    executeScaffoldTool,
    executeShowSpec,
    executeValidateAgent,
    executeCreateAgent,
    executeCreateTool,

    -- * Capability info
    getCapabilityInfo,
    capabilityToName,
    capabilityFromName,

    -- * Template functions (exposed for testing)
    makeAgentTemplate,
    makeToolTemplate,
    makeBashToolTemplate,
    makePythonToolTemplate,
    makeHaskellToolTemplate,
    mergeAgentWithOverrides,
) where

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LByteString
import Data.FileEmbed (embedStringFile)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as Text
import System.Directory (Permissions (..), createDirectoryIfMissing, doesFileExist, getPermissions, setPermissions)
import System.FilePath (takeDirectory)

import Prod.Tracer (Tracer (..))

import System.Agents.Base (
    Agent (..),
    AgentDescription (..),
    BashToolboxDescription,
    BuiltinToolboxDescription,
    DeveloperToolCapability (..),
    DeveloperToolboxDescription (..),
    ExtraAgentRef,
    McpServerDescription,
    OpenAPIToolboxDescription,
    PostgRESTToolboxDescription,
 )
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.Tools.Bash as Bash
import System.Agents.Tools.Skills.Types (SkillName, SkillSource)

-------------------------------------------------------------------------------
-- Core Types
-------------------------------------------------------------------------------

{- | Trace events for monitoring developer toolbox operations.

These events allow tracking of:
* Tool validation attempts
* Scaffold generation
* Spec access
* Agent validation
* Agent creation
* Tool creation
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
    | -- | Agent validation started
      ValidateAgentStartedTrace !FilePath
    | -- | Agent validation completed
      ValidateAgentCompletedTrace !FilePath !Bool
    | -- | Agent creation started
      CreateAgentStartedTrace !(Maybe FilePath) !FilePath
    | -- | Agent creation completed
      CreateAgentCompletedTrace !FilePath !Bool
    | -- | Tool creation started
      CreateToolStartedTrace !Text !(Maybe FilePath) !FilePath
    | -- | Tool creation completed
      CreateToolCompletedTrace !FilePath !Bool
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

-- | Result of an agent validation.
data AgentValidationResult = AgentValidationResult
    { agentValidationPath :: FilePath
    , agentValidationValid :: Bool
    , agentValidationSlug :: Maybe Text
    , agentValidationErrors :: [Text]
    , agentValidationWarnings :: [Text]
    }
    deriving (Show)

-- | JSON serialization for AgentValidationResult.
instance ToJSON AgentValidationResult where
    toJSON result =
        Aeson.object
            [ "path" .= agentValidationPath result
            , "valid" .= agentValidationValid result
            , "slug" .= agentValidationSlug result
            , "errors" .= agentValidationErrors result
            , "warnings" .= agentValidationWarnings result
            ]

-- | Result of a create operation (for agents or tools).
data CreateResult = CreateResult
    { createSuccess :: Bool
    , createPath :: FilePath
    , createAgentSlug :: Maybe Text
    , createError :: Maybe Text
    }
    deriving (Show)

-- | JSON serialization for CreateResult.
instance ToJSON CreateResult where
    toJSON result =
        Aeson.object
            [ "success" .= createSuccess result
            , "path" .= createPath result
            , "slug" .= createAgentSlug result
            , "error" .= createError result
            ]

-- | Override parameters for creating an agent from reference or scratch.
data AgentOverrides = AgentOverrides
    { overrideSlug :: Maybe Text
    , overrideApiKeyId :: Maybe Text
    , overrideFlavor :: Maybe Text
    , overrideModelUrl :: Maybe Text
    , overrideModelName :: Maybe Text
    , overrideAnnounce :: Maybe Text
    , overrideSystemPrompt :: Maybe [Text]
    , overrideToolDirectory :: Maybe (Maybe FilePath)
    , overrideBashToolboxes :: Maybe (Maybe [BashToolboxDescription])
    , overrideMcpServers :: Maybe (Maybe [McpServerDescription])
    , overrideOpenApiToolboxes :: Maybe (Maybe [OpenAPIToolboxDescription])
    , overridePostgrestToolboxes :: Maybe (Maybe [PostgRESTToolboxDescription])
    , overrideBuiltinToolboxes :: Maybe (Maybe [BuiltinToolboxDescription])
    , overrideExtraAgents :: Maybe (Maybe [ExtraAgentRef])
    , overrideSkillSources :: Maybe (Maybe [SkillSource])
    , overrideAutoEnableSkills :: Maybe (Maybe [SkillName])
    }
    deriving (Show)

-- | Default overrides (all Nothing).
defaultAgentOverrides :: AgentOverrides
defaultAgentOverrides =
    AgentOverrides
        { overrideSlug = Nothing
        , overrideApiKeyId = Nothing
        , overrideFlavor = Nothing
        , overrideModelUrl = Nothing
        , overrideModelName = Nothing
        , overrideAnnounce = Nothing
        , overrideSystemPrompt = Nothing
        , overrideToolDirectory = Nothing
        , overrideBashToolboxes = Nothing
        , overrideMcpServers = Nothing
        , overrideOpenApiToolboxes = Nothing
        , overridePostgrestToolboxes = Nothing
        , overrideBuiltinToolboxes = Nothing
        , overrideExtraAgents = Nothing
        , overrideSkillSources = Nothing
        , overrideAutoEnableSkills = Nothing
        }

-- | Configuration for creating a tool.
data ToolConfig = ToolConfig
    { toolConfigSlug :: Text
    , toolConfigDescription :: Text
    , toolConfigArgs :: [ScriptArg]
    , toolConfigEmptyResult :: Maybe Aeson.Value
    }
    deriving (Show)

-- | Description of a script argument for tool creation.
data ScriptArg = ScriptArg
    { scriptArgName :: Text
    , scriptArgDescription :: Text
    , scriptArgType :: Text
    , scriptArgBackingType :: Text
    , scriptArgArity :: Text -- "single" | "optional"
    , scriptArgMode :: Text -- "positional" | "dashdashspace" | "dashdashequal" | "stdin"
    }
    deriving (Show)

-- | JSON serialization for ScriptArg.
instance ToJSON ScriptArg where
    toJSON arg =
        Aeson.object
            [ "name" .= scriptArgName arg
            , "description" .= scriptArgDescription arg
            , "type" .= scriptArgType arg
            , "backing_type" .= scriptArgBackingType arg
            , "arity" .= scriptArgArity arg
            , "mode" .= scriptArgMode arg
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
    | -- | Error during agent validation
      AgentValidationError !Text
    | -- | Error during agent creation
      AgentCreationError !Text
    | -- | Error during tool creation
      ToolCreationError !Text
    | -- | File not found
      FileNotFoundError !FilePath
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
capabilityToName DevToolValidateAgent = "validate-agent"
capabilityToName DevToolCreateAgent = "create-agent"
capabilityToName DevToolCreateTool = "create-tool"

-- | Convert a capability name text to the corresponding DeveloperToolCapability.
capabilityFromName :: Text -> Maybe DeveloperToolCapability
capabilityFromName name = case name of
    "validate-tool" -> Just DevToolValidateTool
    "scaffold-agent" -> Just DevToolScaffoldAgent
    "scaffold-tool" -> Just DevToolScaffoldTool
    "show-spec" -> Just DevToolShowSpec
    "validate-agent" -> Just DevToolValidateAgent
    "create-agent" -> Just DevToolCreateAgent
    "create-tool" -> Just DevToolCreateTool
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
getCapabilityInfo DevToolValidateAgent =
    ( "validate-agent"
    , "Validates an agent JSON configuration file"
    )
getCapabilityInfo DevToolCreateAgent =
    ( "create-agent"
    , "Creates a new agent configuration from scratch or from a reference"
    )
getCapabilityInfo DevToolCreateTool =
    ( "create-tool"
    , "Creates a new tool script from scratch or from a reference"
    )

-------------------------------------------------------------------------------
-- Tool Execution - Validation
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

{- | Execute agent validation.

This function validates an agent JSON configuration file by:
1. Checking if the file exists
2. Parsing the JSON
3. Validating required fields are present
4. Checking field types
5. Generating warnings for discouraged patterns

Returns:
* 'Right AgentValidationResult' with validation details
* 'Left DeveloperToolError' if capability not enabled or file not found
-}
executeValidateAgent ::
    Toolbox ->
    FilePath ->
    IO (Either DeveloperToolError AgentValidationResult)
executeValidateAgent toolbox agentPath = do
    if DevToolValidateAgent `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "validate-agent"
        else do
            fileExists <- doesFileExist agentPath
            if not fileExists
                then
                    pure $
                        Right $
                            AgentValidationResult
                                { agentValidationPath = agentPath
                                , agentValidationValid = False
                                , agentValidationSlug = Nothing
                                , agentValidationErrors = ["File not found: " <> Text.pack agentPath]
                                , agentValidationWarnings = []
                                }
                else do
                    result <- FileLoader.readJsonDescriptionFile agentPath
                    case result of
                        Left err -> do
                            let errText = Text.pack $ show err
                            pure $
                                Right $
                                    AgentValidationResult
                                        { agentValidationPath = agentPath
                                        , agentValidationValid = False
                                        , agentValidationSlug = Nothing
                                        , agentValidationErrors = [errText]
                                        , agentValidationWarnings = []
                                        }
                        Right (AgentDescription agent) -> do
                            let (errors, warnings) = validateAgentStructure agent
                            pure $
                                Right $
                                    AgentValidationResult
                                        { agentValidationPath = agentPath
                                        , agentValidationValid = null errors
                                        , agentValidationSlug = Just (slug agent)
                                        , agentValidationErrors = errors
                                        , agentValidationWarnings = warnings
                                        }

-- | Validate agent structure and return (errors, warnings).
validateAgentStructure :: Agent -> ([Text], [Text])
validateAgentStructure agent =
    let errors = concat
            [ checkRequired "slug" (slug agent)
            , checkRequired "apiKeyId" (apiKeyId agent)
            , checkRequired "flavor" (flavor agent)
            , checkRequired "modelUrl" (modelUrl agent)
            , checkRequired "modelName" (modelName agent)
            , checkRequired "announce" (announce agent)
            , checkSystemPrompt (systemPrompt agent)
            ]
        warnings = concat
            [ checkToolSources agent
            ]
    in (errors, warnings)
  where
    checkRequired :: Text -> Text -> [Text]
    checkRequired fieldName value =
        if Text.null value
            then ["Required field '" <> fieldName <> "' is empty"]
            else []

    checkSystemPrompt :: [Text] -> [Text]
    checkSystemPrompt [] = ["Required field 'systemPrompt' is empty"]
    checkSystemPrompt prompts =
        if all Text.null prompts
            then ["Required field 'systemPrompt' contains only empty strings"]
            else []

    -- Check if at least one tool source is configured
    checkToolSources :: Agent -> [Text]
    checkToolSources a =
        let hasToolDir = case toolDirectory a of
                Just path -> not (null path)
                Nothing -> False
            hasBashToolboxes = case bashToolboxes a of
                Just tb -> not (null tb)
                Nothing -> False
            hasMcpServers = case mcpServers a of
                Just servers -> not (null servers)
                Nothing -> False
            hasOpenApi = case openApiToolboxes a of
                Just tb -> not (null tb)
                Nothing -> False
            hasPostgrest = case postgrestToolboxes a of
                Just tb -> not (null tb)
                Nothing -> False
            hasBuiltin = case builtinToolboxes a of
                Just tb -> not (null tb)
                Nothing -> False
            hasExtraAgents = case extraAgents a of
                Just agents -> not (null agents)
                Nothing -> False
            hasAnySource = hasToolDir || hasBashToolboxes || hasMcpServers || hasOpenApi || hasPostgrest || hasBuiltin || hasExtraAgents
        in if hasAnySource
            then []
            else ["No tool sources configured (toolDirectory, bashToolboxes, mcpServers, openApiToolboxes, postgrestToolboxes, builtinToolboxes, or extraAgents)"]

-------------------------------------------------------------------------------
-- Tool Execution - Creation
-------------------------------------------------------------------------------

{- | Execute agent creation.

This function creates a new agent configuration file either from scratch
or based on a reference agent.

Returns:
* 'Right CreateResult' on success or failure with error
* 'Left DeveloperToolError' if capability not enabled
-}
executeCreateAgent ::
    Toolbox ->
    -- | Source mode: Nothing for scratch, Just path for reference
    Maybe FilePath ->
    -- | Override parameters (merged with reference or used as-is)
    AgentOverrides ->
    -- | Output file path
    FilePath ->
    -- | Force overwrite
    Bool ->
    IO (Either DeveloperToolError CreateResult)
executeCreateAgent toolbox mReferencePath overrides outputPath force = do
    if DevToolCreateAgent `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "create-agent"
        else do
            -- Check if output file exists
            unless force $ do
                exists <- doesFileExist outputPath
                when exists $ do
                    error $ "File already exists: " <> outputPath

            -- Load reference agent if provided
            referenceResult <- case mReferencePath of
                Nothing -> pure $ Right Nothing
                Just refPath -> do
                    refExists <- doesFileExist refPath
                    if not refExists
                        then pure $ Left $ FileNotFoundError refPath
                        else do
                            result <- FileLoader.readJsonDescriptionFile refPath
                            case result of
                                Left err -> pure $ Left $ AgentValidationError (Text.pack $ show err)
                                Right (AgentDescription agent) -> pure $ Right (Just agent)

            case referenceResult of
                Left err -> pure $ Left err
                Right mReferenceAgent -> do
                    -- Build final agent
                    let finalAgent = case mReferenceAgent of
                            Nothing -> agentFromOverrides overrides
                            Just refAgent -> mergeAgentWithOverrides refAgent overrides

                    -- Validate the resulting agent
                    let (errors, _warnings) = validateAgentStructure finalAgent
                    if not (null errors)
                        then
                            pure $
                                Right $
                                    CreateResult
                                        { createSuccess = False
                                        , createPath = outputPath
                                        , createAgentSlug = Just (slug finalAgent)
                                        , createError = Just $ "Validation failed: " <> Text.intercalate "; " errors
                                        }
                        else do
                            -- Write the agent to file
                            writeResult <- try $ do
                                createDirectoryIfMissing True (takeDirectory outputPath)
                                LByteString.writeFile outputPath $
                                    AesonPretty.encodePretty (AgentDescription finalAgent)

                            case writeResult of
                                Left (e :: SomeException) ->
                                    pure $
                                        Right $
                                            CreateResult
                                                { createSuccess = False
                                                , createPath = outputPath
                                                , createAgentSlug = Just (slug finalAgent)
                                                , createError = Just $ Text.pack $ show e
                                                }
                                Right () ->
                                    pure $
                                        Right $
                                            CreateResult
                                                { createSuccess = True
                                                , createPath = outputPath
                                                , createAgentSlug = Just (slug finalAgent)
                                                , createError = Nothing
                                                }

-- | Create an agent from overrides only (no reference).
agentFromOverrides :: AgentOverrides -> Agent
agentFromOverrides overrides =
    Agent
        { slug = fromMaybe "new-agent" (overrideSlug overrides)
        , apiKeyId = fromMaybe "main-key" (overrideApiKeyId overrides)
        , flavor = fromMaybe "OpenAIv1" (overrideFlavor overrides)
        , modelUrl = fromMaybe "https://api.openai.com/v1" (overrideModelUrl overrides)
        , modelName = fromMaybe "gpt-4" (overrideModelName overrides)
        , announce = fromMaybe "a helpful assistant" (overrideAnnounce overrides)
        , systemPrompt = fromMaybe ["You are a helpful assistant."] (overrideSystemPrompt overrides)
        , toolDirectory = fromMaybe (Just "tools") (overrideToolDirectory overrides)
        , bashToolboxes = fromMaybe Nothing (overrideBashToolboxes overrides)
        , mcpServers = fromMaybe (Just []) (overrideMcpServers overrides)
        , openApiToolboxes = fromMaybe Nothing (overrideOpenApiToolboxes overrides)
        , postgrestToolboxes = fromMaybe Nothing (overridePostgrestToolboxes overrides)
        , builtinToolboxes = fromMaybe (Just []) (overrideBuiltinToolboxes overrides)
        , extraAgents = fromMaybe Nothing (overrideExtraAgents overrides)
        , skillSources = fromMaybe Nothing (overrideSkillSources overrides)
        , autoEnableSkills = fromMaybe Nothing (overrideAutoEnableSkills overrides)
        }

-- | Merge a reference agent with overrides.
mergeAgentWithOverrides :: Agent -> AgentOverrides -> Agent
mergeAgentWithOverrides ref overrides =
    Agent
        { slug = fromMaybe (slug ref) (overrideSlug overrides)
        , apiKeyId = fromMaybe (apiKeyId ref) (overrideApiKeyId overrides)
        , flavor = fromMaybe (flavor ref) (overrideFlavor overrides)
        , modelUrl = fromMaybe (modelUrl ref) (overrideModelUrl overrides)
        , modelName = fromMaybe (modelName ref) (overrideModelName overrides)
        , announce = fromMaybe (announce ref) (overrideAnnounce overrides)
        , systemPrompt = fromMaybe (systemPrompt ref) (overrideSystemPrompt overrides)
        , toolDirectory = fromMaybe (toolDirectory ref) (overrideToolDirectory overrides)
        , bashToolboxes = fromMaybe (bashToolboxes ref) (overrideBashToolboxes overrides)
        , mcpServers = fromMaybe (mcpServers ref) (overrideMcpServers overrides)
        , openApiToolboxes = fromMaybe (openApiToolboxes ref) (overrideOpenApiToolboxes overrides)
        , postgrestToolboxes = fromMaybe (postgrestToolboxes ref) (overridePostgrestToolboxes overrides)
        , builtinToolboxes = fromMaybe (builtinToolboxes ref) (overrideBuiltinToolboxes overrides)
        , extraAgents = fromMaybe (extraAgents ref) (overrideExtraAgents overrides)
        , skillSources = fromMaybe (skillSources ref) (overrideSkillSources overrides)
        , autoEnableSkills = fromMaybe (autoEnableSkills ref) (overrideAutoEnableSkills overrides)
        }

{- | Execute tool creation.

This function creates a new tool script in the specified language,
either from scratch or based on a reference tool.

Returns:
* 'Right CreateResult' on success or failure with error
* 'Left DeveloperToolError' if capability not enabled
-}
executeCreateTool ::
    Toolbox ->
    -- | Language (bash, python, haskell)
    Text ->
    -- | Source mode: Nothing for scratch, Just path for reference
    Maybe FilePath ->
    -- | Tool configuration
    ToolConfig ->
    -- | Output file path
    FilePath ->
    -- | Force overwrite
    Bool ->
    IO (Either DeveloperToolError CreateResult)
executeCreateTool toolbox language mReferencePath config outputPath force = do
    if DevToolCreateTool `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "create-tool"
        else do
            -- Check if output file exists
            unless force $ do
                exists <- doesFileExist outputPath
                when exists $ do
                    error $ "File already exists: " <> outputPath

            -- Load reference tool if provided
            referenceResult <- case mReferencePath of
                Nothing -> pure $ Right Nothing
                Just refPath -> do
                    refExists <- doesFileExist refPath
                    if not refExists
                        then pure $ Left $ FileNotFoundError refPath
                        else do
                            -- Only bash tools can be loaded for reference
                            result <- try $ Bash.loadScript (Tracer (const (pure ()))) refPath
                            case result of
                                Left (e :: SomeException) ->
                                    pure $ Left $ ToolCreationError $ Text.pack $ show e
                                Right (Left _err) ->
                                    pure $ Left $ ToolCreationError "Failed to load reference tool"
                                Right (Right scriptDesc) ->
                                    pure $ Right (Just scriptDesc)

            case referenceResult of
                Left err -> pure $ Left err
                Right mReferenceTool -> do
                    -- Build final config
                    let finalConfig = case mReferenceTool of
                            Nothing -> config
                            Just refTool -> mergeToolConfig refTool config

                    -- Generate content
                    let content = makeToolTemplateFromConfig language finalConfig

                    -- Write the tool to file and make executable
                    writeResult <- try $ do
                        createDirectoryIfMissing True (takeDirectory outputPath)
                        Text.writeFile outputPath content
                        -- Make executable: get current permissions and set executable
                        perms <- getPermissions outputPath
                        setPermissions outputPath (perms{executable = True})

                    case writeResult of
                        Left (e :: SomeException) ->
                            pure $
                                Right $
                                    CreateResult
                                        { createSuccess = False
                                        , createPath = outputPath
                                        , createAgentSlug = Just (toolConfigSlug finalConfig)
                                        , createError = Just $ Text.pack $ show e
                                        }
                        Right () -> do
                            -- Validate the created tool by running describe
                            validationResult <- try $ Bash.loadScript (Tracer (const (pure ()))) outputPath
                            case validationResult of
                                Left (e :: SomeException) ->
                                    pure $
                                        Right $
                                            CreateResult
                                                { createSuccess = False
                                                , createPath = outputPath
                                                , createAgentSlug = Just (toolConfigSlug finalConfig)
                                                , createError = Just $ "Created tool failed validation: " <> Text.pack (show e)
                                                }
                                Right (Left _err) ->
                                    pure $
                                        Right $
                                            CreateResult
                                                { createSuccess = False
                                                , createPath = outputPath
                                                , createAgentSlug = Just (toolConfigSlug finalConfig)
                                                , createError = Just "Created tool failed validation"
                                                }
                                Right (Right _validated) ->
                                    pure $
                                        Right $
                                            CreateResult
                                                { createSuccess = True
                                                , createPath = outputPath
                                                , createAgentSlug = Just (toolConfigSlug finalConfig)
                                                , createError = Nothing
                                                }

-- | Merge a reference tool description with config overrides.
mergeToolConfig :: Bash.ScriptDescription -> ToolConfig -> ToolConfig
mergeToolConfig ref config =
    ToolConfig
        { toolConfigSlug = if toolConfigSlug config /= "" then toolConfigSlug config else Bash.scriptSlug (Bash.scriptInfo ref)
        , toolConfigDescription = if toolConfigDescription config /= "" then toolConfigDescription config else Bash.scriptDescription (Bash.scriptInfo ref)
        , toolConfigArgs = if not (null (toolConfigArgs config)) then toolConfigArgs config else scriptArgsToConfigArgs (Bash.scriptArgs (Bash.scriptInfo ref))
        , toolConfigEmptyResult = toolConfigEmptyResult config
        }
  where
    scriptArgsToConfigArgs :: [Bash.ScriptArg] -> [ScriptArg]
    scriptArgsToConfigArgs = map $ \arg ->
        ScriptArg
            { scriptArgName = Bash.argName arg
            , scriptArgDescription = Bash.argDescription arg
            , scriptArgType = Bash.argTypeString arg
            , scriptArgBackingType = Bash.argBackingTypeString arg
            , scriptArgArity = case Bash.argTypeArity arg of
                Bash.Single -> "single"
                Bash.Optional -> "optional"
            , scriptArgMode = case Bash.argCallingMode arg of
                Bash.Stdin -> "stdin"
                Bash.Positional -> "positional"
                Bash.DashDashSpace -> "dashdashspace"
                Bash.DashDashEqual -> "dashdashequal"
            }

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
        , "if [ \"$1\" == \"describe\" ]; then"
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
               , "echo \"Tool " <> toolConfigSlug config <> " executed\""
               ]
  where
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
                    , "while [[ $# -gt 0 ]]; do"
                    , "    case $1 in"
                    , "        " <> argFlag <> "=*)"
                    , "            " <> varName <> "=\"${1#*=}\""
                    , "            shift"
                    , "            ;;"
                    , "        *)"
                    , "            shift"
                    , "            ;;"
                    , "    esac"
                    , "done"
                    ]
                _ -> -- dashdashspace (default)
                    [ varName <> "=\"\""
                    , "while [[ $# -gt 0 ]]; do"
                    , "    case $1 in"
                    , "        " <> argFlag <> ")"
                    , "            " <> varName <> "=\"$2\""
                    , "            shift 2"
                    , "            ;;"
                    , "        *)"
                    , "            shift"
                    , "            ;;"
                    , "    esac"
                    , "done"
                    ]

-------------------------------------------------------------------------------
-- Tool Execution - Legacy Scaffolding
-------------------------------------------------------------------------------

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
                    AesonPretty.encodePretty (AgentDescription agent)
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

