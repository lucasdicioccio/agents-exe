{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Session-aware skills toolbox with progressive disclosure.

This module provides the main integration point for skills with the agent
system. It computes available tools as a pure function of session history,
implementing progressive disclosure:

1. Initially, only metadata tools are visible:
   - skill_describe_{name}: Get skill metadata and instructions
   - skill_enable_{name}: Enable a skill's scripts
   - skill_disable_{name}: Disable a skill's scripts
   - skill_list: List all available skills

2. After skill_enable_{name} is called, script tools become available:
   - skill_{name}_{script}: Execute an enabled skill script

Tool availability is computed as a pure function of Session history,
making it deterministic, testable, and persistent.
-}
module System.Agents.Tools.Skills.Toolbox (
    -- * Trace
    Trace (..),

    -- * Tool Computation
    computeSkillTools,

    -- * Tool Registration Builders
    makeMetaTools,
    makeScriptToolsForSkill,
    makeListSkillsTool,
    makeDescribeTool,
    makeEnableTool,
    makeDisableTool,
    makeScriptTool,

    -- * Skill Store Operations
    SkillsStore,
    loadSkillsFromSources,
    emptySkillsStore,

    -- * Naming Utilities
    skill2LLMName,
) where

import Control.Exception (try)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

import Prod.Tracer (Tracer)
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Session.Types (Session (..))
import System.Agents.ToolRegistration (ToolRegistration (..))
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..))
import System.Agents.Tools.Base (CallResult (..), Tool (..), ToolDef (..), mapToolResult)
import System.Agents.Tools.Context (ToolExecutionContext)
import System.Agents.Tools.Skills.Source (loadSkillsFromSources)
import System.Agents.Tools.Skills.State (foldSession, isScriptEnabled)
import System.Agents.Tools.Skills.Types
import System.Agents.Tools.Trace (ToolTrace (..))

-------------------------------------------------------------------------------
-- Trace Events
-------------------------------------------------------------------------------

{- | Trace events for monitoring skills toolbox operations.

These events allow tracking of:
* Skill loading
* Script execution
* Enable/disable operations
-}
data Trace
    = -- | Started loading skills from sources
      SkillsLoadingTrace ![SkillSource]
    | -- | Skill loaded successfully
      SkillLoadedTrace !SkillName !FilePath
    | -- | Error loading skill
      SkillLoadErrorTrace !FilePath !Text
    | -- | Script executed successfully
      ScriptExecutedTrace !SkillName !ScriptName !Int
    | -- | Script execution failed
      ScriptExecutionErrorTrace !SkillName !ScriptName !Text
    | -- | Skill enabled
      SkillEnabledTrace !SkillName
    | -- | Skill disabled
      SkillDisabledTrace !SkillName
    deriving (Show)

-------------------------------------------------------------------------------
-- Main Tool Computation
-------------------------------------------------------------------------------

{- | Compute all available skill tools from the session state.

This is the main entry point for session-aware skill tool computation.
It returns a list of tool registrations that should be available based
on the current session state.

Progressive disclosure:
- Always returns: metadata tools (describe, enable, disable, list)
- Conditionally returns: script tools only for enabled skills
-}
computeSkillTools :: SkillsStore -> Session -> [ToolRegistration]
computeSkillTools store session =
    let state = foldSession session
        -- Always available: skill discovery and enablement
        metaTools = concatMap (makeMetaTools store) (allSkills store)
        -- Available only when enabled: script execution
        scriptTools =
            concatMap
                (makeScriptToolsForSkill state store)
                (Map.toList $ sssActiveSkills state)
     in -- Include list tool only once
        case makeListSkillsTool store of
            Just listTool -> listTool : metaTools ++ scriptTools
            Nothing -> metaTools ++ scriptTools

-- | Make script tools for a single skill based on session state.
makeScriptToolsForSkill ::
    SkillsSessionState ->
    SkillsStore ->
    (SkillName, SkillScriptsState) ->
    [ToolRegistration]
makeScriptToolsForSkill state store (skillName, _scriptsState) =
    case lookupSkill store skillName of
        Nothing -> []
        Just skill ->
            [ makeScriptTool skill script
            | script <- skill.skillScripts
            , isScriptEnabled state skillName script.siName
            ]

-------------------------------------------------------------------------------
-- Meta Tool Builders
-------------------------------------------------------------------------------

{- | Make all metadata tools for a skill.

These tools are always available regardless of enablement state.
-}
makeMetaTools :: SkillsStore -> Skill -> [ToolRegistration]
makeMetaTools _store skill =
    [ makeDescribeTool skill
    , makeEnableTool skill
    , makeDisableTool skill
    ]

{- | Make the skill_describe_{name} tool.

Returns skill metadata, instructions, and available scripts.
-}
makeDescribeTool :: Skill -> ToolRegistration
makeDescribeTool skill =
    let llmName = skill2LLMName "describe" skill.skillMetadata.smName
        llmDesc = "Get metadata and instructions for skill: " <> skill.skillMetadata.smDescription
        -- Build the response structure
        responseObj =
            Aeson.object
                [ "name" Aeson..= skill.skillMetadata.smName
                , "description" Aeson..= skill.skillMetadata.smDescription
                , "license" Aeson..= skill.skillMetadata.smLicense
                , "compatibility" Aeson..= skill.skillMetadata.smCompatibility
                , "metadata" Aeson..= skill.skillMetadata.smMetadata
                , "instructions" Aeson..= skill.skillInstructions
                , "scripts"
                    Aeson..= [ name
                             | ScriptInfo{siName = ScriptName name} <- skill.skillScripts
                             ]
                , "references"
                    Aeson..= [ name
                             | ReferenceInfo{riName = name} <- skill.skillReferences
                             ]
                ]
        responseBytes = LByteString.toStrict $ Aeson.encode responseObj

        tool :: Tool ()
        tool =
            Tool
                { toolDef = SkillTool skill.skillMetadata.smName "describe"
                , toolRun = \_tracer _ctx _args ->
                    pure $ BlobToolSuccess () responseBytes
                }

        find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
        find call =
            if call.toolCallFunction.toolCallFunctionName == llmName
                then Just $ mapToolResult (const call) tool
                else Nothing
     in ToolRegistration tool (makeToolDecl llmName llmDesc []) find

{- | Make the skill_enable_{name} tool.

Enabling a skill makes its script tools available for execution.
The actual state change is tracked by the session folding mechanism.
-}
makeEnableTool :: Skill -> ToolRegistration
makeEnableTool skill =
    let llmName = skill2LLMName "enable" skill.skillMetadata.smName
        llmDesc = "Enable skill to access its scripts: " <> skill.skillMetadata.smDescription
        responseBytes = Text.encodeUtf8 $ "Skill enabled: " <> skillNameToText skill.skillMetadata.smName

        tool :: Tool ()
        tool =
            Tool
                { toolDef = SkillTool skill.skillMetadata.smName "enable"
                , toolRun = \_tracer _ctx _args ->
                    pure $ BlobToolSuccess () responseBytes
                }

        find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
        find call =
            if call.toolCallFunction.toolCallFunctionName == llmName
                then Just $ mapToolResult (const call) tool
                else Nothing
     in ToolRegistration tool (makeToolDecl llmName llmDesc []) find

{- | Make the skill_disable_{name} tool.

Disabling a skill hides its script tools.
-}
makeDisableTool :: Skill -> ToolRegistration
makeDisableTool skill =
    let llmName = skill2LLMName "disable" skill.skillMetadata.smName
        llmDesc = "Disable skill and hide its scripts: " <> skill.skillMetadata.smDescription
        responseBytes = Text.encodeUtf8 $ "Skill disabled: " <> skillNameToText skill.skillMetadata.smName

        tool :: Tool ()
        tool =
            Tool
                { toolDef = SkillTool skill.skillMetadata.smName "disable"
                , toolRun = \_tracer _ctx _args ->
                    pure $ BlobToolSuccess () responseBytes
                }

        find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
        find call =
            if call.toolCallFunction.toolCallFunctionName == llmName
                then Just $ mapToolResult (const call) tool
                else Nothing
     in ToolRegistration tool (makeToolDecl llmName llmDesc []) find

{- | Make the skill_list tool.

Returns a list of all available skills with their metadata.
-}
makeListSkillsTool :: SkillsStore -> Maybe ToolRegistration
makeListSkillsTool store =
    if null (allSkills store)
        then Nothing
        else
            let llmName = OpenAI.ToolName "skill_list"
                llmDesc = "List all available skills and their descriptions"
                skillsList =
                    [ Aeson.object
                        [ "name" Aeson..= skill.skillMetadata.smName
                        , "description" Aeson..= skill.skillMetadata.smDescription
                        ]
                    | skill <- allSkills store
                    ]
                responseObj = Aeson.object ["skills" Aeson..= skillsList]
                responseBytes = LByteString.toStrict $ Aeson.encode responseObj

                tool :: Tool ()
                tool =
                    Tool
                        { toolDef = SkillListTool
                        , toolRun = \_tracer _ctx _args ->
                            pure $ BlobToolSuccess () responseBytes
                        }

                find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
                find call =
                    if call.toolCallFunction.toolCallFunctionName == llmName
                        then Just $ mapToolResult (const call) tool
                        else Nothing
             in Just $ ToolRegistration tool (makeToolDecl llmName llmDesc []) find

-------------------------------------------------------------------------------
-- Script Tool Builder
-------------------------------------------------------------------------------

{- | Make a script execution tool for an enabled skill.

Script tools are only available after the skill has been enabled via
skill_enable_{name}.
-}
makeScriptTool :: Skill -> ScriptInfo -> ToolRegistration
makeScriptTool skill script =
    let llmName = makeScriptToolName skill.skillMetadata.smName script.siName
        llmDesc = fromMaybe "Execute skill script" script.siDescription
        paramProps = map scriptArgToParam script.siArgs

        tool :: Tool ()
        tool =
            Tool
                { toolDef = SkillScriptTool skill.skillMetadata.smName script.siName
                , toolRun = runScriptTool script
                }

        find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
        find call =
            if call.toolCallFunction.toolCallFunctionName == llmName
                then Just $ mapToolResult (const call) tool
                else Nothing
     in ToolRegistration tool (makeToolDecl llmName llmDesc paramProps) find

{- | Execute a skill script with the provided arguments.

Implements the describe/run protocol:
1. Parse arguments from the tool call
2. Execute the script with those arguments
3. Return the result
-}
runScriptTool ::
    ScriptInfo ->
    Tracer IO ToolTrace ->
    ToolExecutionContext ->
    Aeson.Value ->
    IO (CallResult ())
runScriptTool script _tracer _ctx args = do
    -- Convert args to command line arguments
    let cmdArgs = extractArgsFromValue args
    -- Execute the script
    result <- try $ readProcessWithExitCode (siPath script) cmdArgs ""
    case result of
        Left (e :: IOError) ->
            return $ BlobToolSuccess () (Text.encodeUtf8 $ "Script execution error: " <> Text.pack (show e))
        Right (exitCode, stdout, stderr) -> case exitCode of
            ExitSuccess ->
                return $ BlobToolSuccess () (Text.encodeUtf8 $ Text.pack stdout)
            ExitFailure code ->
                return $
                    BlobToolSuccess () $
                        Text.encodeUtf8 $
                            Text.unlines
                                [ "Script failed with exit code " <> Text.pack (show code)
                                , "stdout: " <> Text.pack stdout
                                , "stderr: " <> Text.pack stderr
                                ]

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

{- | Naming policy for skill tools.

Format: skill_{action}_{skill_name}
Examples:
- skill_describe_pdf-processing
- skill_enable_pdf-processing
- skill_disable_pdf-processing
- skill_pdf-processing_extract-text
-}
skill2LLMName :: Text -> SkillName -> OpenAI.ToolName
skill2LLMName action skillName =
    OpenAI.ToolName $ "skill_" <> action <> "_" <> skillNameToText skillName

{- | Make a script tool name.

Format: skill_{skill_name}_{script_name}
-}
makeScriptToolName :: SkillName -> ScriptName -> OpenAI.ToolName
makeScriptToolName skillName scriptName =
    OpenAI.ToolName $
        "skill_"
            <> skillNameToText skillName
            <> "_"
            <> unScriptName scriptName

-- | Convert a script arg info to a ParamProperty for schema.
scriptArgToParam :: ScriptArgInfo -> ParamProperty
scriptArgToParam arg =
    ParamProperty
        { propertyKey = saName arg
        , propertyType = OpaqueParamType (saType arg)
        , propertyDescription = saDescription arg
        , propertyRequired = saRequired arg
        }

-- | Make a tool declaration for OpenAI.
makeToolDecl :: OpenAI.ToolName -> Text -> [ParamProperty] -> OpenAI.Tool
makeToolDecl name desc props =
    OpenAI.Tool
        { OpenAI.toolName = name
        , OpenAI.toolDescription = desc
        , OpenAI.toolParamProperties = props
        }

-- | Extract command line arguments from the tool call value.
extractArgsFromValue :: Aeson.Value -> [String]
extractArgsFromValue val = case val of
    Aeson.Object obj ->
        -- Convert object values to string arguments
        mapMaybe (fmap Text.unpack . valueToText) (KeyMap.elems obj)
    Aeson.Array arr ->
        mapMaybe (fmap Text.unpack . valueToText) (toList arr)
    Aeson.String txt -> [Text.unpack txt]
    _ -> []

-- | Convert an Aeson value to text.
valueToText :: Aeson.Value -> Maybe Text
valueToText val = case val of
    Aeson.String txt -> Just txt
    Aeson.Number n -> Just $ Text.pack $ show n
    Aeson.Bool b -> Just $ Text.pack $ show b
    _ -> Nothing

