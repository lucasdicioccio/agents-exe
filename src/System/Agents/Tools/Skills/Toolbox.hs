{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Skills toolbox with ProgressiveDisclosure framework integration.

This module provides skill tool registration using the generic ProgressiveDisclosure
framework. Each skill becomes a toolgroup, and skill management uses the standard
meta tools (meta_activate_tool, meta_deactivate_tool, meta_discover_tools).

Progressive disclosure:
- Skills are initially hidden (OnDemandActivated "skill:{name}")
- After meta_activate_tool("skill:{name}"), describe and script tools become available
- After meta_deactivate_tool("skill:{name}"), tools are hidden again
- meta_discover_tools lists all available skill toolgroups

This replaces the old custom skill management (skill_list, skill_enable_{name}, etc.)
with the generic framework from System.Agents.Combinators.ProgressiveDisclosure.
-}
module System.Agents.Tools.Skills.Toolbox (
    -- * Trace
    Trace (..),

    -- * Tool Registration
    skillToToolRegistrations,
    makeDescribeTool,
    makeScriptTool,

    -- * Toolgroup Utilities
    getSkillToolgroups,
    skillToToolgroupName,

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
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

import Prod.Tracer (Tracer)
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.ToolRegistration (Tool, ToolRegistration (..))
import qualified System.Agents.ToolRegistration as ToolRegistration
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..), ToolDescription (..), ToolName (..))
import System.Agents.Tools.Activation (Activation (..))
import System.Agents.Tools.Base (CallResult (..), ToolDef (..), mapToolResult)
import qualified System.Agents.Tools.Base as ToolBase
import System.Agents.Tools.Context (ToolCall (..), ToolExecutionContext)
import System.Agents.Tools.Skills.Source (loadSkillsFromSources)
import System.Agents.Tools.Skills.Types

-------------------------------------------------------------------------------
-- Trace Events
-------------------------------------------------------------------------------

{- | Trace events for monitoring skills toolbox operations.

These events allow tracking of:
* Skill loading
* Script execution
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
    deriving (Show)

-------------------------------------------------------------------------------
-- Tool Registration
-------------------------------------------------------------------------------

{- | Convert a skill to its tool registrations with ProgressiveDisclosure activation.

Each skill becomes a toolgroup "skill:{name}" with:
- A describe tool (shows skill metadata and instructions)
- Script tools for each script in the skill

All tools are marked with OnDemandActivated so they only appear after
meta_activate_tool("skill:{name}") is called.
-}
skillToToolRegistrations :: Skill -> [ToolRegistration]
skillToToolRegistrations skill =
    let skillToolgroup = skillToToolgroupName (skillMetadata skill).smName
        -- Describe tool for the skill
        describeTool = (makeDescribeTool skill){toolActivation = Just $ OnDemandActivated skillToolgroup}
        -- Script tools (all share same toolgroup - enabling skill enables all scripts)
        scriptTools = map (\s -> (makeScriptTool skill s){toolActivation = Just $ OnDemandActivated skillToolgroup}) (skillScripts skill)
     in describeTool : scriptTools

{- | Get all skill toolgroup names for use with meta_discover_tools.

Returns a Set of toolgroup names in the format "skill:{name}" for all skills
in the store.
-}
getSkillToolgroups :: SkillsStore -> Set Text
getSkillToolgroups store =
    Set.fromList $ map (skillToToolgroupName . smName . skillMetadata) (allSkills store)

{- | Convert a skill name to its toolgroup name.

Format: "skill:{skill_name}"
Example: skillToToolgroupName (SkillName "pdf-processing") = "skill:pdf-processing"
-}
skillToToolgroupName :: SkillName -> Text
skillToToolgroupName skillName = "skill:" <> skillNameToText skillName

-------------------------------------------------------------------------------
-- Tool Builders
-------------------------------------------------------------------------------

{- | Make the skill_describe_{name} tool.

Returns skill metadata, instructions, and available scripts.
This tool is activated on-demand via the ProgressiveDisclosure framework.
-}
makeDescribeTool :: Skill -> ToolRegistration
makeDescribeTool skill =
    let llmName = skill2LLMName "describe" (skillMetadata skill).smName
        llmDesc = "Get metadata and instructions for skill: " <> (skillMetadata skill).smDescription
        -- Build the response structure
        responseObj =
            Aeson.object
                [ "name" Aeson..= (skillMetadata skill).smName
                , "description" Aeson..= (skillMetadata skill).smDescription
                , "license" Aeson..= (skillMetadata skill).smLicense
                , "compatibility" Aeson..= (skillMetadata skill).smCompatibility
                , "metadata" Aeson..= (skillMetadata skill).smMetadata
                , "instructions" Aeson..= skillInstructions skill
                , "scripts"
                    Aeson..= [ name
                             | ScriptInfo{siName = ScriptName name} <- skillScripts skill
                             ]
                , "references"
                    Aeson..= [ name
                             | ReferenceInfo{riName = name} <- skillReferences skill
                             ]
                ]
        responseBytes = LByteString.toStrict $ Aeson.encode responseObj

        tool :: Tool ()
        tool =
            ToolBase.Tool
                { ToolBase.toolDef = MetaTool "skill-describe"
                , ToolBase.toolRun = \_tracer _ctx _args ->
                    pure $ BlobToolSuccess () responseBytes
                }

        find :: ToolCall -> Maybe (Tool ToolCall)
        find call =
            if call.callToolName == getToolName llmName
                then Just $ mapToolResult (const call) tool
                else Nothing
     in ToolRegistration tool (makeToolDecl llmName llmDesc []) find Nothing

{- | Make a script execution tool for a skill.

Script tools are activated on-demand along with their parent skill via
the ProgressiveDisclosure framework.
-}
makeScriptTool :: Skill -> ScriptInfo -> ToolRegistration
makeScriptTool skill script =
    let llmName = makeScriptToolName (skillMetadata skill).smName script.siName
        llmDesc = fromMaybe "Execute skill script" script.siDescription
        paramProps = map scriptArgToParam script.siArgs

        tool :: Tool ()
        tool =
            ToolBase.Tool
                { ToolBase.toolDef = MetaTool "skill-script"
                , ToolBase.toolRun = runScriptTool script
                }

        find :: ToolCall -> Maybe (Tool ToolCall)
        find call =
            if call.callToolName == getToolName llmName
                then Just $ mapToolResult (const call) tool
                else Nothing
     in ToolRegistration tool (makeToolDecl llmName llmDesc paramProps) find Nothing

{- | Execute a skill script with the provided arguments.

Implements the describe/run protocol:
1. Parse arguments from the tool call
2. Execute the script with those arguments
3. Return the result
-}
runScriptTool ::
    ScriptInfo ->
    Tracer IO ToolRegistration.Trace ->
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
makeToolDecl :: OpenAI.ToolName -> Text -> [ParamProperty] -> ToolDescription
makeToolDecl name desc props =
    ToolDescription
        { toolDescriptionName = name
        , toolDescriptionText = desc
        , toolDescriptionParamProperties = props
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
