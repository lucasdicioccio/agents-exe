{-# LANGUAGE OverloadedRecordDot #-}

{- | Session state management for skills with progressive disclosure.

This module provides the pure monoid-based state tracking for skills.
The state is computed by folding over session turns, enabling deterministic,
testable, and persistent tool availability.

Progressive disclosure:
- Initially, only metadata tools are visible (skill_describe_{name}, skill_enable_{name})
- After skill_enable_{name} is called, script tools become available
- After skill_disable_{name} is called, script tools are hidden again
-}
module System.Agents.Tools.Skills.State (
    -- * Session State Types
    ScriptState (..),
    SkillScriptsState,
    SkillsSessionState (..),

    -- * Pure Session Folding
    foldSession,
    extractFromTurn,
    extractFromToolCall,

    -- * State Construction Helpers
    enableSkill,
    disableSkill,
    isScriptEnabled,
    getEnabledScripts,
) where

import qualified Data.Aeson
import qualified Data.Aeson.KeyMap
import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import System.Agents.Session.Types (LlmToolCall (..), LlmTurnContent (..), Session (..), Turn (..))
import System.Agents.Tools.Skills.Types

-------------------------------------------------------------------------------
-- Pure Session Folding
-------------------------------------------------------------------------------

{- | Fold a complete session into the skills session state.

This is a pure function that computes the current state of enabled skills
by folding over all turns in the session.
-}
foldSession :: Session -> SkillsSessionState
foldSession session = foldl' (\acc turn -> acc <> extractFromTurn turn) mempty (turns session)

{- | Extract skills state changes from a single turn.

User turns don't modify skills state.
LLM turns may contain skill enable/disable tool calls.
-}
extractFromTurn :: Turn -> SkillsSessionState
extractFromTurn (UserTurn _ _) = mempty
extractFromTurn (LlmTurn llmTurn _) =
    foldl' (\acc tc -> acc <> extractFromToolCall tc) mempty (llmToolCalls llmTurn)

{- | Extract skills state changes from a single tool call.

Recognizes:
- skill_enable_{name} -> enables all scripts in the skill
- skill_disable_{name} -> disables all scripts in the skill
-}
extractFromToolCall :: LlmToolCall -> SkillsSessionState
extractFromToolCall (LlmToolCall val) =
    -- Extract function name from the tool call
    case extractFunctionName val of
        Nothing -> mempty
        Just funcName -> parseSkillToolCall funcName

{- | Extract the function name from a tool call JSON value.

Expects: {"function": {"name": "skill_enable_foo", ...}, ...}
-}
extractFunctionName :: Data.Aeson.Value -> Maybe Text
extractFunctionName val = case val of
    Data.Aeson.Object obj -> do
        func <- Data.Aeson.KeyMap.lookup "function" obj
        case func of
            Data.Aeson.Object funcObj -> do
                name <- Data.Aeson.KeyMap.lookup "name" funcObj
                case name of
                    Data.Aeson.String txt -> Just txt
                    _ -> Nothing
            _ -> Nothing
    _ -> Nothing

{- | Parse a skill tool call name and return the appropriate state change.

Handles:
- skill_enable_{skill_name} -> enables the skill
- skill_disable_{skill_name} -> disables the skill
- skill_{skill_name}_{script_name} -> no state change (execution only)
- skill_describe_{skill_name} -> no state change (metadata only)
- skill_list -> no state change (metadata only)
-}
parseSkillToolCall :: Text -> SkillsSessionState
parseSkillToolCall funcName
    | Just skillName <- Text.stripPrefix "skill_enable_" funcName =
        enableSkillFromText skillName
    | Just skillName <- Text.stripPrefix "skill_disable_" funcName =
        disableSkillFromText skillName
    | otherwise = mempty

{- | Enable a skill by name (from text).

Returns empty state if the name is invalid.
-}
enableSkillFromText :: Text -> SkillsSessionState
enableSkillFromText txt =
    case validateSkillName txt of
        Left _ -> mempty
        Right name -> enableSkill name

{- | Disable a skill by name (from text).

Returns empty state if the name is invalid.
-}
disableSkillFromText :: Text -> SkillsSessionState
disableSkillFromText txt =
    case validateSkillName txt of
        Left _ -> mempty
        Right name -> disableSkill name

-------------------------------------------------------------------------------
-- State Construction Helpers
-------------------------------------------------------------------------------

{- | Create a state that enables all scripts in a skill.

Since we don't know the script names yet, we create an empty
SkillScriptsState which will be populated with Enabled status
when the skill is looked up.
-}
enableSkill :: SkillName -> SkillsSessionState
enableSkill skillName =
    SkillsSessionState $
        Map.singleton skillName Map.empty

-- | Create a state that disables all scripts in a skill.
disableSkill :: SkillName -> SkillsSessionState
disableSkill skillName =
    SkillsSessionState $
        Map.singleton skillName Map.empty

-- An empty map here signals "disabled" - the skill is tracked
-- but with no scripts enabled

{- | Check if a specific script is enabled in the session state.

A script is enabled if:
1. Its skill is in the active skills map
2. The script either:
   - Has an explicit Enabled state, OR
   - The skill was enabled without explicit script list (all scripts enabled)
-}
isScriptEnabled :: SkillsSessionState -> SkillName -> ScriptName -> Bool
isScriptEnabled (SkillsSessionState activeSkills) skillName scriptName =
    case Map.lookup skillName activeSkills of
        Nothing -> False
        Just scriptsState ->
            case Map.lookup scriptName scriptsState of
                Just Enabled -> True
                Just Disabled -> False
                Nothing -> True -- Skill enabled without explicit script list = all enabled

{- | Get all enabled scripts for a skill.

If the skill has no explicit script list, returns Nothing to indicate
all scripts should be considered enabled.
-}
getEnabledScripts :: SkillsSessionState -> SkillName -> Maybe [ScriptName]
getEnabledScripts (SkillsSessionState activeSkills) skillName =
    case Map.lookup skillName activeSkills of
        Nothing -> Just [] -- Skill not enabled, no scripts
        Just scriptsState ->
            if Map.null scriptsState
                then Nothing -- All scripts enabled (no explicit list)
                else Just $ Map.keys $ Map.filter (== Enabled) scriptsState

