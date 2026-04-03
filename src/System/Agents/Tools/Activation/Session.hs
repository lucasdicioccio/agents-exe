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

    -- * Re-exports from Activation
    ToolgroupName,
    ActivationState (..),
    ToolboxSessionState (..),
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import System.Agents.Session.Types (LlmToolCall (..), LlmTurnContent (..), Session (..), Turn (..))
import System.Agents.Tools.Activation

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
- meta_discover_tools() -> no state change (metadata only)
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
- meta_discover_tools -> no state change
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
    | funcName == "meta_discover_tools" = mempty
    | otherwise = mempty

{- | Extract the 'toolgroup' argument from a tool call.

Expects: {"function": {"arguments": {"toolgroup": "name"}, ...}, ...}
Or: {"function": {"arguments": "name"}, ...} (direct string argument)
-}
extractToolgroupArg :: Aeson.Value -> Maybe ToolgroupName
extractToolgroupArg val = case val of
    Aeson.Object obj -> do
        func <- KeyMap.lookup "function" obj
        case func of
            Aeson.Object funcObj -> do
                args <- KeyMap.lookup "arguments" funcObj
                case args of
                    Aeson.Object argsObj -> do
                        -- Look for "toolgroup" key first
                        tg <- KeyMap.lookup "toolgroup" argsObj
                        case tg of
                            Aeson.String txt -> Just txt
                            _ -> Nothing
                    Aeson.String txt -> Just txt
                    _ -> Nothing
            _ -> Nothing
    _ -> Nothing

-------------------------------------------------------------------------------
-- State Queries
-------------------------------------------------------------------------------

{- | Check if a specific toolgroup is currently active.

A toolgroup is active if:
1. It's in the active toolgroups map, OR
2. The map doesn't contain it (default for backward compatibility)

Returns False only if the toolgroup was explicitly deactivated.
-}
isToolgroupActive :: ToolboxSessionState -> ToolgroupName -> Bool
isToolgroupActive (ToolboxSessionState stateMap) toolgroup =
    case Map.lookup toolgroup stateMap of
        Just Active -> True
        Just Inactive -> False
        Nothing -> True -- Default: active if not explicitly tracked

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

