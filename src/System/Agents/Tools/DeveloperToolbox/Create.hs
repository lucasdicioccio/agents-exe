{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Creation capabilities for the DeveloperToolbox.

This module provides functionality for creating:
- New agent configurations from scratch or from a reference
- New tool scripts in various languages
-}
module System.Agents.Tools.DeveloperToolbox.Create (
    -- * Agent creation
    executeCreateAgent,
    agentFromOverrides,
    mergeAgentWithOverrides,
    
    -- * Tool creation  
    executeCreateTool,
    mergeToolConfig,
) where

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LByteString
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (
    Permissions (..),
    createDirectoryIfMissing,
    doesFileExist,
    getPermissions,
    setPermissions,
 )
import System.FilePath (takeDirectory)

import Prod.Tracer (Tracer (..))

import System.Agents.Base (
    Agent (..),
    AgentDescription (..),
    DeveloperToolCapability (..),
 )
import qualified System.Agents.FileLoader as FileLoader
import System.Agents.Tools.Bash (LoadTrace)
import qualified System.Agents.Tools.Bash as Bash
import System.Agents.Tools.DeveloperToolbox.Types (
    AgentOverrides (..),
    CreateResult (..),
    DeveloperToolError (..),
    ScriptArg (..),
    Toolbox (..),
    ToolConfig (..),
 )
import System.Agents.Tools.DeveloperToolbox.Validate (validateAgentStructure)
import System.Agents.Tools.DeveloperToolbox.Templates (
    makeToolTemplateFromConfig,
 )

-------------------------------------------------------------------------------
-- Agent Creation
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

-------------------------------------------------------------------------------
-- Tool Creation
-------------------------------------------------------------------------------

{- | Execute tool creation.

This function creates a new tool script in the specified language,
either from scratch or based on a reference tool.

Returns:
* 'Right CreateResult' on success or failure with error
* 'Left DeveloperToolError' if capability not enabled
-}
executeCreateTool ::
    Tracer IO LoadTrace ->
    -- | Language (bash, python, haskell)
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
executeCreateTool tracer toolbox language mReferencePath config outputPath force = do
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
                            result <- try $ Bash.loadScript tracer refPath
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
                            validationResult <- try $ Bash.loadScript tracer outputPath
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

