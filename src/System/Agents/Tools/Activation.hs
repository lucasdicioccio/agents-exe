{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Unified progressive disclosure system for all toolboxes.

This module provides lifetime and activation configuration for all toolbox types,
generalizing the skills system's progressive disclosure pattern.

== Lifetime

Controls how long a toolbox instance lives:

* 'ProgramLifetime': One instance for entire program execution
* 'RootConversationLifetime': One per root conversation (shared across extra-agents)
* 'ConversationLifetime': One per conversation (default)
* 'ToolCallLifetime': Fresh instance per tool call (not valid for MCP servers)

== Activation

Controls when tools are visible to the LLM:

* 'AlwaysActivated': Always visible (current default behavior)
* 'OnDemandActivated': Hidden until explicitly activated via meta-tool
* 'FirstNStepsActivated': Visible for first N steps, then conditionally hidden

== Example Configuration

@
{
  "bashToolboxes": [
    {
      "tag": "FileSystemDirectory",
      "contents": {
        "path": "./tools",
        "lifetime": "program",
        "activation": "always"
      }
    },
    {
      "tag": "FileSystemDirectory",
      "contents": {
        "path": "./rarely-used-tools",
        "lifetime": "conversation",
        "activation": {"tag": "on-demand", "toolgroup": "rare-tools"}
      }
    }
  ]
}
@
-}
module System.Agents.Tools.Activation (
    -- * Lifetime Configuration
    Lifetime (..),
    defaultLifetime,

    -- * Activation Configuration
    Activation (..),
    StickyMode (..),
    defaultActivation,

    -- * Session State Management
    ToolgroupName,
    ActivationState (..),
    ToolboxSessionState (..),

    -- * Pure Session Folding
    foldSession,
    extractFromTurn,
    extractFromToolCall,

    -- * State Construction Helpers
    activateToolgroup,
    deactivateToolgroup,
    isToolgroupActive,

    -- * Tool Visibility Computation
    ToolRegistration (..),
    ToolboxMetadata (..),
    computeVisibleTools,
    shouldShowTool,

    -- * Meta-Tool Names
    metaActivateToolName,
    metaDeactivateToolName,
    metaDiscoverToolsName,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

import System.Agents.Session.Types (LlmToolCall (..), LlmTurnContent (..), Session (..), Turn (..))

-------------------------------------------------------------------------------
-- Lifetime Configuration
-------------------------------------------------------------------------------

{- | Lifetime scope for toolbox instances.

Controls how long a toolbox instance lives and how it's shared across
conversations and tool calls.

* 'ProgramLifetime': Global singleton, lives for entire program
* 'RootConversationLifetime': Shared across a root conversation and its extra-agents
* 'ConversationLifetime': Per-conversation instance (default)
* 'ToolCallLifetime': Fresh instance per tool call (NOT valid for MCP servers)
-}
data Lifetime
    = ProgramLifetime
    | RootConversationLifetime
    | ConversationLifetime
    | ToolCallLifetime
    deriving (Show, Eq, Ord, Generic)

instance Aeson.FromJSON Lifetime where
    parseJSON = Aeson.withText "Lifetime" $ \txt ->
        case txt of
            "program" -> return ProgramLifetime
            "root-conversation" -> return RootConversationLifetime
            "conversation" -> return ConversationLifetime
            "tool-call" -> return ToolCallLifetime
            other -> fail $ "Invalid Lifetime: " ++ Text.unpack other ++ ". Expected 'program', 'root-conversation', 'conversation', or 'tool-call'."

instance Aeson.ToJSON Lifetime where
    toJSON ProgramLifetime = Aeson.String "program"
    toJSON RootConversationLifetime = Aeson.String "root-conversation"
    toJSON ConversationLifetime = Aeson.String "conversation"
    toJSON ToolCallLifetime = Aeson.String "tool-call"

-- | Default lifetime for toolboxes (ConversationLifetime).
defaultLifetime :: Lifetime
defaultLifetime = ConversationLifetime

-------------------------------------------------------------------------------
-- Activation Configuration
-------------------------------------------------------------------------------

{- | Sticky mode for FirstNSteps activation.

Controls whether tools remain visible after the initial N steps.
-}
data StickyMode
    = StickyIfUsed
    | NonSticky
    deriving (Show, Eq, Ord, Generic)

instance Aeson.FromJSON StickyMode where
    parseJSON = Aeson.withText "StickyMode" $ \txt ->
        case txt of
            "sticky-if-used" -> return StickyIfUsed
            "non-sticky" -> return NonSticky
            _other -> fail $ "Invalid StickyMode: " ++ Text.unpack txt ++ ". Expected 'sticky-if-used' or 'non-sticky'."

instance Aeson.ToJSON StickyMode where
    toJSON StickyIfUsed = Aeson.String "sticky-if-used"
    toJSON NonSticky = Aeson.String "non-sticky"

{- | Activation strategy for toolbox visibility.

Controls when tools from a toolbox are visible to the LLM.

* 'AlwaysActivated': Always visible (current default behavior)
* 'OnDemandActivated': Hidden until activated via meta_activate_tool
* 'FirstNStepsActivated': Visible for first N steps, then conditionally hidden
-}
data Activation
    = AlwaysActivated
    | OnDemandActivated Text
    | FirstNStepsActivated Int StickyMode
    deriving (Show, Eq, Ord, Generic)

instance Aeson.FromJSON Activation where
    parseJSON (Aeson.String "always") = return AlwaysActivated
    parseJSON val =
        Aeson.withObject
            "Activation"
            ( \obj -> do
                tag <- obj Aeson..: "tag"
                case (tag :: Text) of
                    "always" -> return AlwaysActivated
                    "on-demand" -> OnDemandActivated <$> obj Aeson..: "toolgroup"
                    "first-n-steps" -> do
                        steps <- obj Aeson..: "steps"
                        sticky <- obj Aeson..: "sticky"
                        return $ FirstNStepsActivated steps sticky
                    _other -> fail $ "Unknown Activation tag: " ++ Text.unpack tag
            )
            val

instance Aeson.ToJSON Activation where
    toJSON AlwaysActivated = Aeson.String "always"
    toJSON (OnDemandActivated toolgroup) =
        Aeson.object
            [ "tag" Aeson..= ("on-demand" :: Text)
            , "toolgroup" Aeson..= toolgroup
            ]
    toJSON (FirstNStepsActivated steps sticky) =
        Aeson.object
            [ "tag" Aeson..= ("first-n-steps" :: Text)
            , "steps" Aeson..= steps
            , "sticky" Aeson..= sticky
            ]

-- | Default activation for toolboxes (AlwaysActivated).
defaultActivation :: Activation
defaultActivation = AlwaysActivated

-------------------------------------------------------------------------------
-- Session State Management
-------------------------------------------------------------------------------

-- | Name of a toolgroup for on-demand activation.
type ToolgroupName = Text

-- | Activation state for a toolgroup.
data ActivationState = Active | Inactive
    deriving (Show, Eq, Ord, Generic, Aeson.FromJSON, Aeson.ToJSON)

{- | Session state tracking activation for all on-demand toolboxes.

This is a monoid that can be built by folding over session turns.
Later state overrides earlier state (last activate/deactivate wins).

Note: This replaces SkillsSessionState and provides a generic mechanism
for all toolbox types.
-}
newtype ToolboxSessionState = ToolboxSessionState
    { tssActiveToolgroups :: Map ToolgroupName ActivationState
    }
    deriving stock (Show, Eq, Generic)
    deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup ToolboxSessionState where
    (ToolboxSessionState m1) <> (ToolboxSessionState m2) =
        -- Union with later values overriding earlier ones
        ToolboxSessionState (Map.unionWith (\_ earlier -> earlier) m2 m1)

instance Monoid ToolboxSessionState where
    mempty = ToolboxSessionState Map.empty

-------------------------------------------------------------------------------
-- Pure Session Folding
-------------------------------------------------------------------------------

{- | Fold a complete session into the toolbox session state.

This is a pure function that computes the current activation state
by folding over all turns in the session.
-}
foldSession :: Session -> ToolboxSessionState
foldSession session = foldl' (\acc turn -> acc <> extractFromTurn turn) mempty (turns session)

{- | Extract toolbox state changes from a single turn.

User turns don't modify toolbox state.
LLM turns may contain meta activation/deactivation tool calls.
-}
extractFromTurn :: Turn -> ToolboxSessionState
extractFromTurn (UserTurn _ _) = mempty
extractFromTurn (LlmTurn llmTurn _) =
    foldl' (\acc tc -> acc <> extractFromToolCall tc) mempty (llmToolCalls llmTurn)

{- | Extract toolbox state changes from a single tool call.

Recognizes:
- meta_activate_tool(toolgroup) -> activates the toolgroup
- meta_deactivate_tool(toolgroup) -> deactivates the toolgroup
-}
extractFromToolCall :: LlmToolCall -> ToolboxSessionState
extractFromToolCall (LlmToolCall val) =
    -- Extract function name and arguments from the tool call
    case extractFunctionCall val of
        Nothing -> mempty
        Just (funcName, args) -> parseMetaToolCall funcName args

{- | Extract the function name and arguments from a tool call JSON value.

Expects: {"function": {"name": "meta_activate_tool", "arguments": "..."}, ...}
-}
extractFunctionCall :: Aeson.Value -> Maybe (Text, Maybe Aeson.Value)
extractFunctionCall val = case val of
    Aeson.Object obj -> do
        func <- KeyMap.lookup "function" obj
        case func of
            Aeson.Object funcObj -> do
                name <- KeyMap.lookup "name" funcObj
                case name of
                    Aeson.String txt -> do
                        let args = KeyMap.lookup "arguments" funcObj
                        Just (txt, args)
                    _ -> Nothing
            _ -> Nothing
    _ -> Nothing

{- | Parse a meta tool call name and arguments, returning appropriate state change.

Handles:
- meta_activate_tool(skill_name) -> activates the skill
- meta_deactivate_tool(skill_name) -> deactivates the skill
- meta_discover_tools() -> no state change (metadata only)
-}
parseMetaToolCall :: Text -> Maybe Aeson.Value -> ToolboxSessionState
parseMetaToolCall funcName mArgs
    | funcName == metaActivateToolName =
        case extractToolgroupFromArgs mArgs of
            Just toolgroup -> activateToolgroup toolgroup
            Nothing -> mempty
    | funcName == metaDeactivateToolName =
        case extractToolgroupFromArgs mArgs of
            Just toolgroup -> deactivateToolgroup toolgroup
            Nothing -> mempty
    | otherwise = mempty

{- | Extract toolgroup name from tool call arguments.

Arguments may be passed as a JSON object or as a JSON string.
-}
extractToolgroupFromArgs :: Maybe Aeson.Value -> Maybe ToolgroupName
extractToolgroupFromArgs Nothing = Nothing
extractToolgroupFromArgs (Just (Aeson.Object obj)) = do
    tg <- KeyMap.lookup "toolgroup" obj
    case tg of
        Aeson.String txt -> Just txt
        _ -> Nothing
extractToolgroupFromArgs (Just (Aeson.String txt)) = Just txt
extractToolgroupFromArgs _ = Nothing

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

-- | Check if a toolgroup is currently active.
isToolgroupActive :: ToolboxSessionState -> ToolgroupName -> Bool
isToolgroupActive (ToolboxSessionState activeToolgroups) toolgroup =
    case Map.lookup toolgroup activeToolgroups of
        Just Active -> True
        _ -> False

-------------------------------------------------------------------------------
-- Tool Visibility Computation
-------------------------------------------------------------------------------

{- | Minimal interface for tool registration needed by visibility computation.
This avoids circular dependencies with System.Agents.ToolRegistration
-}
data ToolRegistration = ToolRegistration
    { trToolgroupName :: Maybe ToolgroupName
    -- ^ Optional toolgroup name (for on-demand activation)
    , trActivation :: Activation
    -- ^ Activation strategy for this tool
    }
    deriving (Show, Eq)

-- | Metadata about a toolbox needed for visibility computation.
data ToolboxMetadata = ToolboxMetadata
    { tbToolgroupName :: Maybe ToolgroupName
    -- ^ Toolgroup name for on-demand activation
    , tbActivation :: Activation
    -- ^ Activation strategy
    , tbLifetime :: Lifetime
    -- ^ Lifetime scope
    }
    deriving (Show, Eq)

{- | Compute visible tools based on session state and current step.

This filters a list of tool registrations based on their activation
configuration and the current session state.

Logic:
1. AlwaysActivated: always include
2. OnDemandActivated group: include if group is Active in session state
3. FirstNStepsActivated k sticky:
   - If currentStep <= k: include
   - If currentStep > k:
     - StickyIfUsed: include if tool was used in previous steps
     - NonSticky: exclude

Note: Tool usage tracking for sticky mode requires additional session
data that should be computed separately and passed in.
-}
computeVisibleTools ::
    -- | Current activation state
    ToolboxSessionState ->
    -- | Current step number (0-indexed)
    Int ->
    -- | All registered tools
    [ToolRegistration] ->
    -- | Filtered visible tools
    [ToolRegistration]
computeVisibleTools sessionState currentStep =
    filter (shouldShowTool sessionState currentStep)

{- | Determine if a tool should be shown based on its activation config.

This is a pure function that makes the visibility decision.
-}
shouldShowTool :: ToolboxSessionState -> Int -> ToolRegistration -> Bool
shouldShowTool _ _ (ToolRegistration _ AlwaysActivated) = True
shouldShowTool sessionState _ (ToolRegistration (Just toolgroup) (OnDemandActivated _)) =
    isToolgroupActive sessionState toolgroup
shouldShowTool _ _ (ToolRegistration Nothing (OnDemandActivated _)) =
    -- On-demand without a toolgroup name is invalid - don't show
    False
shouldShowTool _ currentStep (ToolRegistration _ (FirstNStepsActivated n _stickyMode)) =
    -- For now, we only implement the basic first-n-steps logic
    -- Sticky mode would require tracking tool usage across steps
    currentStep <= n

-------------------------------------------------------------------------------
-- Meta-Tool Names
-------------------------------------------------------------------------------

-- | Name of the meta tool for activating a toolgroup.
metaActivateToolName :: Text
metaActivateToolName = "meta_activate_tool"

-- | Name of the meta tool for deactivating a toolgroup.
metaDeactivateToolName :: Text
metaDeactivateToolName = "meta_deactivate_tool"

-- | Name of the meta tool for discovering available toolgroups.
metaDiscoverToolsName :: Text
metaDiscoverToolsName = "meta_discover_tools"
