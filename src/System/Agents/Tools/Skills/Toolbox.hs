{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

    -- * Script Name
    ScriptName (..),

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
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import Prod.Tracer (Tracer)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName)
import System.Process (proc)
import System.Process.ByteString (readCreateProcessWithExitCode)

import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.ToolRegistration (Tool, ToolRegistration (..))
import qualified System.Agents.ToolRegistration as ToolRegistration
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..), ToolDescription (..), ToolName (..))
import System.Agents.Tools.Activation (Activation (..))
import System.Agents.Tools.Base (CallResult (..), ToolDef (..), mapToolResult)
import qualified System.Agents.Tools.Base as ToolBase
import System.Agents.Tools.Context (ToolCall (..), ToolExecutionContext)
import System.Agents.Tools.ParamTier (defaultParamTier)
import qualified System.Agents.Tools.ScriptTypes as ST
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

newtype ScriptName = ScriptName {unScriptName :: FilePath}
    deriving (Show, Aeson.ToJSON, Aeson.FromJSON)

scriptName :: ST.ScriptDescription -> ScriptName
scriptName sd = ScriptName $ takeBaseName sd.scriptPath

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
                    Aeson..= [ scriptName sd
                             | sd <- skillScripts skill
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
                    pure $ BlobToolSuccess () responseBytes Nothing
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
makeScriptTool :: Skill -> ST.ScriptDescription -> ToolRegistration
makeScriptTool skill script =
    let llmName = makeScriptToolName (skillMetadata skill).smName (scriptName script)
        -- Use the description from describe output, with fallback
        llmDesc = script.scriptInfo.scriptDescription
        -- Build parameter properties from the script's describe output (siArgs)
        paramProps = map scriptArgToParam script.scriptInfo.scriptArgs

        tool :: Tool ()
        tool =
            ToolBase.Tool
                { -- Store the ScriptDescription (describe result) in toolDef, like BashTool does
                  ToolBase.toolDef = SkillScriptTool script.scriptInfo
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
1. Parse arguments from the tool call using the script's argument metadata (siArgs from describe)
2. Execute the script with those arguments
3. Return the result

Arguments are parsed from the JSON object using the ScriptArg metadata,
similar to how Bash.runValue handles arguments. This ensures that:
- Arguments are matched by name from the JSON object
- Only provided arguments are passed to the script
- Arguments are passed as positional parameters in order
-}
runScriptTool ::
    ST.ScriptDescription ->
    Tracer IO ToolRegistration.Trace ->
    ToolExecutionContext ->
    Aeson.Value ->
    IO (CallResult ())
runScriptTool script _tracer _ctx args = do
    -- Parse arguments using the script's argument metadata from describe (like Bash.runValue)
    case parseArgsForScript script args of
        Left err ->
            return $ BlobToolSuccess () (Text.encodeUtf8 $ "Argument parsing error: " <> Text.pack err) Nothing
        Right (argz, stdin) -> do
            let cmdArgs = "run" : [Text.unpack arg | arg <- argz]
            -- Execute the script with parsed arguments
            {-
            baseEnv <- getEnvironment
            let toolEnv = buildToolEnvironment mCtx baseEnv
            -}
            let process = (proc script.scriptPath cmdArgs)
            result <- try $ readCreateProcessWithExitCode process (Text.encodeUtf8 stdin)
            case result of
                Left (e :: IOError) ->
                    return $ BlobToolSuccess () (Text.encodeUtf8 $ "Script execution error: " <> Text.pack (show e)) Nothing
                Right (exitCode, stdout, stderr) -> case exitCode of
                    ExitSuccess ->
                        return $ BlobToolSuccess () stdout Nothing
                    ExitFailure code ->
                        let errorOutput =
                                Text.encodeUtf8 $
                                    Text.unlines
                                        [ "Script failed with exit code " <> Text.pack (show code)
                                        , "stdout: " <> Text.decodeUtf8With lenientDecode stdout
                                        , "stderr: " <> Text.decodeUtf8With lenientDecode stderr
                                        ]
                         in return $ BlobToolSuccess () errorOutput Nothing

-------------------------------------------------------------------------------
-- Argument Parsing (reuses translateArguments from ScriptTypes)
-------------------------------------------------------------------------------

flattenArguments :: [(ScriptArg, Maybe Text)] -> [Text]
flattenArguments = mconcat . fmap flatten1
  where
    flatten1 :: (ScriptArg, Maybe Text) -> [Text]
    flatten1 (_, Nothing) = []
    flatten1 (arg, Just txt) =
        case arg.argCallingMode of
            Stdin -> []
            Positional -> [txt]
            DashDashEqual -> [mconcat ["--", arg.argName, "=", txt]]
            DashDashSpace -> [mconcat ["--", arg.argName], txt]

flattenInput :: [(ScriptArg, Maybe Text)] -> Text
flattenInput = Text.unlines . mconcat . fmap flatten1
  where
    flatten1 :: (ScriptArg, Maybe Text) -> [Text]
    flatten1 (_, Nothing) = []
    flatten1 (arg, Just txt) =
        case arg.argCallingMode of
            Stdin -> [txt]
            Positional -> []
            DashDashEqual -> []
            DashDashSpace -> []

-- | Parse arguments from JSON value using script argument metadata.
parseArgsForScript :: ST.ScriptDescription -> Aeson.Value -> Either String ([Text], Text)
parseArgsForScript script val = do
    args <- Aeson.parseEither (ST.translateArguments script.scriptInfo) val
    pure (flattenArguments args, flattenInput args)

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
makeScriptToolName skillName name =
    OpenAI.ToolName $
        "skill_"
            <> skillNameToText skillName
            <> "_"
            <> (Text.pack $ unScriptName name)

-- | Convert a script arg to a ParamProperty for schema.
scriptArgToParam :: ST.ScriptArg -> ParamProperty
scriptArgToParam arg =
    ParamProperty
        { propertyKey = ST.argName arg
        , propertyType = OpaqueParamType (ST.argTypeString arg)
        , propertyDescription = ST.argDescription arg
        , propertyRequired = ST.argTypeArity arg == ST.Single
        , propertyTier = defaultParamTier
        }

-- | Make a tool declaration for OpenAI.
makeToolDecl :: OpenAI.ToolName -> Text -> [ParamProperty] -> ToolDescription
makeToolDecl name desc props =
    ToolDescription
        { toolDescriptionName = name
        , toolDescriptionText = desc
        , toolDescriptionParamProperties = props
        }
