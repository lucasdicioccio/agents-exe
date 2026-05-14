{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Validation capabilities for the DeveloperToolbox.

This module provides functionality for validating:
- Bash tool scripts
- Agent JSON configuration files
-}
module System.Agents.Tools.DeveloperToolbox.Validate (
    -- * Validation execution
    executeValidateTool,
    executeValidateAgent,
    
    -- * Validation helpers (exposed for testing)
    validateAgentStructure,
) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesFileExist)

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
    DeveloperToolError (..),
    Toolbox (..),
    ValidationResult (..),
    AgentValidationResult (..),
 )

-------------------------------------------------------------------------------
-- Tool Validation
-------------------------------------------------------------------------------

{- | Execute tool validation.

This function validates a bash tool script by attempting to load its
description using the standard 'describe' command.

Returns:
* 'Right ValidationResult' on successful validation (valid or invalid)
* 'Left DeveloperToolError' if capability not enabled
-}
executeValidateTool ::
    Tracer IO LoadTrace ->
    Toolbox ->
    FilePath ->
    IO (Either DeveloperToolError ValidationResult)
executeValidateTool tracer toolbox toolPath = do
    if DevToolValidateTool `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "validate-tool"
        else do
            result <- try $ Bash.loadScript tracer toolPath
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

-------------------------------------------------------------------------------
-- Agent Validation
-------------------------------------------------------------------------------

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
    let errors =
            concat
                [ checkRequired "slug" (slug agent)
                , checkRequired "apiKeyId" (apiKeyId agent)
                , checkRequired "flavor" (flavor agent)
                , checkRequired "modelUrl" (modelUrl agent)
                , checkRequired "modelName" (modelName agent)
                , checkRequired "announce" (announce agent)
                , checkSystemPrompt (systemPrompt agent)
                ]
        warnings =
            concat
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

