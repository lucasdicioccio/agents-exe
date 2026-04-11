{-# LANGUAGE OverloadedRecordDot #-}

{- | Session state management for toolbox activation.

This module provides the pure monoid-based state tracking for toolgroup activation.
The state is computed by folding over session turns, enabling deterministic,
testable, and persistent tool availability.

Meta-tools for activation:
- meta_activate_tool(toolgroup) -> activates a toolgroup
- meta_deactivate_tool(toolgroup) -> deactivates a toolgroup
- meta_discover_tools() -> lists available toolgroups (no state change)

Progressive disclosure:
- Tools with 'OnDemandActivated' activation are initially hidden
- After meta_activate_tool is called, they become available
- After meta_deactivate_tool is called, they become hidden again

Backward Compatibility:
- Old skill_enable_{name} calls are treated as meta_activate_tool("skill:{name}")
- Old skill_disable_{name} calls are treated as meta_deactivate_tool("skill:{name}")
- Old skill_list calls are ignored (no state change)
-}
module System.Agents.Tools.Activation.Session (
    -- * Pure Session Folding
    foldSession,
    extractFromTurn,
    extractFromToolCall,

    -- * State Queries
    isToolgroupActive,
    getActiveToolgroups,

    -- * State Construction Helpers
    activateToolgroup,
    deactivateToolgroup,

    -- * Meta Tool Builders
    makeActivateTool,
    makeDeactivateTool,
    makeDiscoverTools,
    extractToolgroups,

    -- * Re-exports from Activation
    ToolgroupName,
    ActivationState (..),
    ToolboxSessionState (..),
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Session.Types (LlmToolCall (..), LlmTurnContent (..), Session (..), Turn (..))
import System.Agents.ToolRegistration (Tool, ToolRegistration (..))
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..), ToolDescription (..), ToolName (..))
import System.Agents.Tools.Activation
import System.Agents.Tools.Base (CallResult (..), ToolDef (..), mapToolResult)
import qualified System.Agents.Tools.Base as ToolBase
import System.Agents.Tools.Context (ToolCall (..))

-------------------------------------------------------------------------------
-- Pure Session Folding
-------------------------------------------------------------------------------

{- | Fold a complete session into the toolbox session state.

This is a pure function that computes the current activation state
of all toolgroups by folding over all turns in the session.
-}
foldSession :: Session -> ToolboxSessionState
foldSession session = foldl' (\acc turn -> acc <> extractFromTurn turn) mempty session.turns

{- | Extract toolbox activation state changes from a single turn.

User turns don't modify activation state.
LLM turns may contain meta_activate_tool or meta_deactivate_tool calls.
-}
extractFromTurn :: Turn -> ToolboxSessionState
extractFromTurn (UserTurn _ _) = mempty
extractFromTurn (LlmTurn llmTurn _) =
    foldl' (\acc tc -> acc <> extractFromToolCall tc) mempty llmTurn.llmToolCalls

{- | Extract activation state changes from a single tool call.

Recognizes:
- meta_activate_tool(toolgroup) -> activates the toolgroup
- meta_deactivate_tool(toolgroup) -> deactivates the toolgroup
- skill_enable_{name} (backward compat) -> activates "skill:{name}"
- skill_disable_{name} (backward compat) -> deactivates "skill:{name}"
- meta_discover_tools() -> no state change
- skill_list -> no state change (backward compat)
-}
extractFromToolCall :: LlmToolCall -> ToolboxSessionState
extractFromToolCall (LlmToolCall val) =
    case extractFunctionName val of
        Nothing -> mempty
        Just funcName -> parseMetaToolCall funcName val

{- | Extract the function name from a tool call JSON value.

Expects: {"function": {"name": "meta_activate_tool", ...}, ...}
-}
extractFunctionName :: Aeson.Value -> Maybe Text
extractFunctionName val = case val of
    Aeson.Object obj -> do
        func <- KeyMap.lookup "function" obj
        case func of
            Aeson.Object funcObj -> do
                name <- KeyMap.lookup "name" funcObj
                case name of
                    Aeson.String txt -> Just txt
                    _ -> Nothing
            _ -> Nothing
    _ -> Nothing

{- | Parse a meta-tool call and return the appropriate state change.

Handles:
- meta_activate_tool with 'toolgroup' argument -> activates the toolgroup
- meta_deactivate_tool with 'toolgroup' argument -> deactivates the toolgroup
- skill_enable_{name} -> activates "skill:{name}" (backward compatibility)
- skill_disable_{name} -> deactivates "skill:{name}" (backward compatibility)
- meta_discover_tools -> no state change
- skill_list -> no state change (backward compatibility)
- other tool calls -> no state change
-}
parseMetaToolCall :: Text -> Aeson.Value -> ToolboxSessionState
parseMetaToolCall funcName val
    | funcName == "meta_activate_tool" =
        case extractToolgroupArg val of
            Just toolgroup -> activateToolgroup toolgroup
            Nothing -> mempty
    | funcName == "meta_deactivate_tool" =
        case extractToolgroupArg val of
            Just toolgroup -> deactivateToolgroup toolgroup
            Nothing -> mempty
    -- Backward compatibility: skill_enable_{name} -> meta_activate_tool("skill:{name}")
    | Just skillName <- Text.stripPrefix "skill_enable_" funcName =
        activateToolgroup ("skill:" <> skillName)
    -- Backward compatibility: skill_disable_{name} -> meta_deactivate_tool("skill:{name}")
    | Just skillName <- Text.stripPrefix "skill_disable_" funcName =
        deactivateToolgroup ("skill:" <> skillName)
    -- No state change for discovery tools
    | funcName == "meta_discover_tools" = mempty
    | funcName == "skill_list" = mempty
    | otherwise = mempty

{- | Extract the 'toolgroup' argument from a tool call.

The LLM API returns tool call arguments as a JSON-encoded string.
This function handles both:
1. JSON-encoded string arguments: {"arguments": "{\\"toolgroup\\":\\"name\\"}"}
2. Direct object arguments: {"arguments": {"toolgroup": "name"}}

It attempts to parse string arguments as JSON first, then extracts
the "toolgroup" field from the resulting object.
-}
extractToolgroupArg :: Aeson.Value -> Maybe ToolgroupName
extractToolgroupArg val = case val of
    Aeson.Object obj -> do
        func <- KeyMap.lookup "function" obj
        case func of
            Aeson.Object funcObj -> do
                args <- KeyMap.lookup "arguments" funcObj
                extractToolgroupFromArgsValue args
            _ -> Nothing
    _ -> Nothing

{- | Extract the toolgroup name from the arguments value.

Handles two cases:
1. String containing JSON: parse it and extract "toolgroup" field
2. Direct object: extract "toolgroup" field directly
3. Plain string: use as toolgroup name (backward compatibility)
-}
extractToolgroupFromArgsValue :: Aeson.Value -> Maybe ToolgroupName
extractToolgroupFromArgsValue args =
    case args of
        Aeson.String txt ->
            -- Try to parse as JSON first (LLM API encodes args as JSON string)
            case Aeson.decode (LByteString.fromStrict $ Text.encodeUtf8 txt) of
                Just (Aeson.Object obj) ->
                    -- Parsed as JSON object, extract toolgroup field
                    case KeyMap.lookup "toolgroup" obj of
                        Just (Aeson.String tg) -> Just tg
                        _ -> Nothing
                Just (Aeson.String tg) ->
                    -- Parsed as JSON string, use directly
                    Just tg
                _ ->
                    -- Not valid JSON or not an object/string, use raw string
                    Just txt
        Aeson.Object argsObj ->
            -- Direct object, extract toolgroup field
            case KeyMap.lookup "toolgroup" argsObj of
                Just (Aeson.String tg) -> Just tg
                _ -> Nothing
        _ -> Nothing

-------------------------------------------------------------------------------
-- State Queries
-------------------------------------------------------------------------------

{- | Check if a specific toolgroup is currently active.

A toolgroup is active if:
1. It's in the active toolgroups map with Active state

Returns False for toolgroups not explicitly activated.
-}
isToolgroupActive :: ToolboxSessionState -> ToolgroupName -> Bool
isToolgroupActive (ToolboxSessionState stateMap) toolgroup =
    case Map.lookup toolgroup stateMap of
        Just Active -> True
        _ -> False

{- | Get all currently active toolgroups.

Returns a list of toolgroup names that are in Active state.
-}
getActiveToolgroups :: ToolboxSessionState -> [ToolgroupName]
getActiveToolgroups (ToolboxSessionState stateMap) =
    Map.keys $ Map.filter (== Active) stateMap

-------------------------------------------------------------------------------
-- State Construction Helpers
-------------------------------------------------------------------------------

-- | Create a state that activates a toolgroup.
activateToolgroup :: ToolgroupName -> ToolboxSessionState
activateToolgroup toolgroup =
    ToolboxSessionState $ Map.singleton toolgroup Active

-- | Create a state that deactivates a toolgroup.
deactivateToolgroup :: ToolgroupName -> ToolboxSessionState
deactivateToolgroup toolgroup =
    ToolboxSessionState $ Map.singleton toolgroup Inactive

-------------------------------------------------------------------------------
-- Meta Tool Builders
-------------------------------------------------------------------------------

{- | Get all unique toolgroup names from a list of ToolRegistrations.

Extracts toolgroups from the 'OnDemandActivated' activation mode.
-}
extractToolgroups :: [ToolRegistration] -> Set ToolgroupName
extractToolgroups = Set.fromList . mapMaybe extractToolgroup
  where
    extractToolgroup :: ToolRegistration -> Maybe ToolgroupName
    extractToolgroup reg = case reg.toolActivation of
        Just (OnDemandActivated tg) -> Just tg
        _ -> Nothing

{- | Make the meta_activate_tool tool.

Activates a toolgroup, making its associated tools available.
The actual state change is tracked by the session folding mechanism.
-}
makeActivateTool :: Set ToolgroupName -> ToolRegistration
makeActivateTool toolgroups =
    let llmName = OpenAI.ToolName "meta_activate_tool"
        availableGroups = Text.intercalate ", " (Set.toAscList toolgroups)
        llmDesc = "Activate a toolgroup to access its tools. Available toolgroups: " <> availableGroups
        paramProps =
            [ ParamProperty
                { propertyKey = "toolgroup"
                , propertyType = StringParamType
                , propertyDescription = "Name of the toolgroup to activate. Available: " <> availableGroups
                , propertyRequired = True
                }
            ]

        -- The run function checks if the toolgroup exists and returns "ok" or an error
        tool :: Tool ()
        tool =
            ToolBase.Tool
                { ToolBase.toolDef = MetaTool "activate"
                , ToolBase.toolRun = \_tracer _ctx args ->
                    case extractToolgroupFromArgs args of
                        Just tg
                            | tg `Set.member` toolgroups ->
                                pure $ BlobToolSuccess () (Text.encodeUtf8 $ "Toolgroup '" <> tg <> "' activated") Nothing
                        Just tg ->
                            pure $ BlobToolSuccess () (Text.encodeUtf8 $ "Error: Unknown toolgroup '" <> tg <> "'. Available: " <> availableGroups) Nothing
                        Nothing ->
                            pure $ BlobToolSuccess () (Text.encodeUtf8 $ "Error: Missing 'toolgroup' parameter. Available: " <> availableGroups) Nothing
                }

        find :: ToolCall -> Maybe (Tool ToolCall)
        find call =
            if call.callToolName == getToolName llmName
                then Just $ mapToolResult (const call) tool
                else Nothing
     in ToolRegistration tool (makeToolDecl llmName llmDesc paramProps) find Nothing

{- | Make the meta_deactivate_tool tool.

Deactivates a toolgroup, hiding its associated tools.
-}
makeDeactivateTool :: Set ToolgroupName -> ToolRegistration
makeDeactivateTool toolgroups =
    let llmName = OpenAI.ToolName "meta_deactivate_tool"
        availableGroups = Text.intercalate ", " (Set.toAscList toolgroups)
        llmDesc = "Deactivate a toolgroup to hide its tools. Available toolgroups: " <> availableGroups
        paramProps =
            [ ParamProperty
                { propertyKey = "toolgroup"
                , propertyType = StringParamType
                , propertyDescription = "Name of the toolgroup to deactivate. Available: " <> availableGroups
                , propertyRequired = True
                }
            ]

        -- The run function checks if the toolgroup exists and returns "ok" or an error
        tool :: Tool ()
        tool =
            ToolBase.Tool
                { ToolBase.toolDef = MetaTool "deactivate"
                , ToolBase.toolRun = \_tracer _ctx args ->
                    case extractToolgroupFromArgs args of
                        Just tg
                            | tg `Set.member` toolgroups ->
                                pure $ BlobToolSuccess () (Text.encodeUtf8 $ "Toolgroup '" <> tg <> "' deactivated") Nothing
                        Just tg ->
                            pure $ BlobToolSuccess () (Text.encodeUtf8 $ "Error: Unknown toolgroup '" <> tg <> "'. Available: " <> availableGroups) Nothing
                        Nothing ->
                            pure $ BlobToolSuccess () (Text.encodeUtf8 $ "Error: Missing 'toolgroup' parameter. Available: " <> availableGroups) Nothing
                }

        find :: ToolCall -> Maybe (Tool ToolCall)
        find call =
            if call.callToolName == getToolName llmName
                then Just $ mapToolResult (const call) tool
                else Nothing
     in ToolRegistration tool (makeToolDecl llmName llmDesc paramProps) find Nothing

{- | Make the meta_discover_tools tool.

Returns a list of all available toolgroups without modifying state.
-}
makeDiscoverTools :: Set ToolgroupName -> ToolRegistration
makeDiscoverTools toolgroups =
    let llmName = OpenAI.ToolName "meta_discover_tools"
        availableGroups = Text.intercalate ", " (Set.toAscList toolgroups)
        llmDesc = "List all available toolgroups. Toolgroups: " <> availableGroups
        responseObj = Aeson.object ["toolgroups" Aeson..= Set.toAscList toolgroups]
        responseBytes = LByteString.toStrict $ Aeson.encode responseObj

        tool :: Tool ()
        tool =
            ToolBase.Tool
                { ToolBase.toolDef = MetaTool "discover"
                , ToolBase.toolRun = \_tracer _ctx _args ->
                    pure $ BlobToolSuccess () responseBytes Nothing
                }

        find :: ToolCall -> Maybe (Tool ToolCall)
        find call =
            if call.callToolName == getToolName llmName
                then Just $ mapToolResult (const call) tool
                else Nothing
     in ToolRegistration tool (makeToolDecl llmName llmDesc []) find Nothing

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

{- | Extract the toolgroup name from tool call arguments.

Expects: {"toolgroup": "name"} or just "name"
-}
extractToolgroupFromArgs :: Aeson.Value -> Maybe ToolgroupName
extractToolgroupFromArgs val = case val of
    Aeson.Object obj -> do
        tg <- KeyMap.lookup "toolgroup" obj
        case tg of
            Aeson.String txt -> Just txt
            _ -> Nothing
    Aeson.String txt -> Just txt
    _ -> Nothing

-- | Make a tool declaration for OpenAI.
makeToolDecl :: OpenAI.ToolName -> Text -> [ParamProperty] -> ToolDescription
makeToolDecl name desc props =
    ToolDescription
        { toolDescriptionName = name
        , toolDescriptionText = desc
        , toolDescriptionParamProperties = props
        }

