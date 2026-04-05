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
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Maybe (fromMaybe)
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

-- Import shared script types and translateArguments from ScriptTypes
-- Note: We import ScriptInfo with a qualified name to avoid conflict with Skills.Types.ScriptInfo
import qualified System.Agents.Tools.ScriptTypes as ST

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

The ScriptInfo (from the script's describe output) is stored in the toolDef
as SkillScriptTool, similar to how BashTools stores ScriptDescription in BashTool.
-}
makeScriptTool :: Skill -> ScriptInfo -> ToolRegistration
makeScriptTool skill script =
    let llmName = makeScriptToolName (skillMetadata skill).smName script.siName
        -- Use the description from describe output, with fallback
        llmDesc = fromMaybe "Execute skill script" script.siDescription
        -- Build parameter properties from the script's describe output (siArgs)
        paramProps = map scriptArgToParam script.siArgs

        tool :: Tool ()
        tool =
            ToolBase.Tool
                { -- Store the ScriptInfo (describe result) in toolDef, like BashTool does
                  ToolBase.toolDef = SkillScriptTool script
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
    ScriptInfo ->
    Tracer IO ToolRegistration.Trace ->
    ToolExecutionContext ->
    Aeson.Value ->
    IO (CallResult ())
runScriptTool script _tracer _ctx args = do
    -- Parse arguments using the script's argument metadata from describe (like Bash.runValue)
    case parseArgsForScript script args of
        Left err ->
            return $ BlobToolSuccess () (Text.encodeUtf8 $ "Argument parsing error: " <> Text.pack err)
        Right cmdArgs -> do
            -- Execute the script with parsed arguments
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
-- Argument Parsing (reuses translateArguments from ScriptTypes)
-------------------------------------------------------------------------------

{- | Parse arguments from JSON value using script argument metadata.

This uses translateArguments from ScriptTypes which only requires the argName field:
1. Extract argument values from the JSON object by name
2. Build command line arguments from provided values

Unlike the Bash implementation, skill scripts use simpler positional argument
passing without calling modes (stdin, dashdash, etc.).
-}
parseArgsForScript :: ScriptInfo -> Aeson.Value -> Either String [String]
parseArgsForScript script val = do
    -- Use translateArguments from ScriptTypes
    -- We create a dummy ScriptInfo for ScriptTypes since translateArguments only uses scriptArgs
    let scriptInfo = ST.ScriptInfo
            { ST.scriptArgs = siArgs script
            , ST.scriptSlug = ""  -- Not used by translateArguments
            , ST.scriptDescription = ""  -- Not used by translateArguments
            , ST.scriptEmptyResultBehavior = Nothing  -- Not used by translateArguments
            }
    argValues <- Aeson.parseEither (ST.translateArguments scriptInfo) val
    -- Flatten to command line arguments (only include provided args)
    pure $ concatMap argValueToString argValues

{- | Convert an argument value pair to command line strings.

For skill scripts, arguments are passed positionally in the order defined
by the script's argument metadata. Only provided (Just) values are included.
-}
argValueToString :: (ST.ScriptArg, Maybe Text) -> [String]
argValueToString (_, Nothing) = []
argValueToString (_, Just txt) = [Text.unpack txt]

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

-- | Convert a script arg to a ParamProperty for schema.
scriptArgToParam :: ST.ScriptArg -> ParamProperty
scriptArgToParam arg =
    ParamProperty
        { propertyKey = ST.argName arg
        , propertyType = OpaqueParamType (ST.argTypeString arg)
        , propertyDescription = ST.argDescription arg
        , propertyRequired = ST.argTypeArity arg == ST.Single
        }

-- | Make a tool declaration for OpenAI.
makeToolDecl :: OpenAI.ToolName -> Text -> [ParamProperty] -> ToolDescription
makeToolDecl name desc props =
    ToolDescription
        { toolDescriptionName = name
        , toolDescriptionText = desc
        , toolDescriptionParamProperties = props
        }

