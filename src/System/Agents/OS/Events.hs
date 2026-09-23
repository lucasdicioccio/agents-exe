{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | OS Event types for subcall visibility and TUI integration.

This module provides event types that are used for communication between
the OS layer and the TUI, particularly for tracking subcall lifecycle
(started, progress, completed, failed).

By placing these types in a separate module, we avoid circular dependencies
between OS.Interfaces, Session.Base, and Tools.Context.
-}
module System.Agents.OS.Events (
    -- * OS Event Types
    OSEvent (..),

    -- * Tool-call activity
    ToolCallActivity (..),
    ToolCallPhase (..),
    isFinalToolCallPhase,

    -- * Phase 2c emission (ctxEmit)
    OSEmission (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)

import System.Agents.Base (ConversationId)
import System.Agents.Session.Types (Session, SessionId, ToolCallId)

-- | Events that can be emitted by the OS.
data OSEvent
    = OSEvent_Error Text
    | -- \** Subcall Events for TUI visibility

      -- | Emitted when a subcall (helper agent invocation) starts
      OSEvent_SubcallStarted
        { subcallParentConversationId :: ConversationId
        -- ^ The parent conversation that initiated the subcall
        , subcallConversationId :: ConversationId
        -- ^ The new conversation ID for the subcall
        , subcallAgentSlug :: Text
        -- ^ The slug of the agent being called
        , subcallDepth :: Int
        -- ^ The recursion depth of this subcall (0 = root)
        }
    | -- | Emitted when a subcall makes progress
      OSEvent_SubcallProgress
        { subcallProgressConversationId :: ConversationId
        -- ^ The subcall conversation ID
        , subcallProgressSession :: Session
        -- ^ The current session state
        }
    | -- | Emitted when a subcall completes successfully
      OSEvent_SubcallCompleted
        { subcallCompletedConversationId :: ConversationId
        -- ^ The subcall conversation ID
        , subcallCompletedResult :: Text
        -- ^ The result/response text from the subcall
        }
    | -- | Emitted when a subcall fails
      OSEvent_SubcallFailed
        { subcallFailedConversationId :: ConversationId
        -- ^ The subcall conversation ID
        , subcallFailedError :: Text
        -- ^ The error message
        }
    | -- | Emitted when a background (async) tool call starts, reports
      -- progress, or finishes
      OSEvent_ToolCallActivity ToolCallActivity
    deriving (Show)

-- | Lifecycle phase of a background tool call.
data ToolCallPhase
    = ToolCallStarted
    | -- | A progress payload reported by the tool
      ToolCallProgressed Value
    | ToolCallCompleted
    | -- | The call failed with the given error
      ToolCallFailed Text
    | ToolCallCancelled
    deriving (Show, Eq)

-- | Whether the phase is final (the call no longer runs).
isFinalToolCallPhase :: ToolCallPhase -> Bool
isFinalToolCallPhase = \case
    ToolCallStarted -> False
    ToolCallProgressed _ -> False
    ToolCallCompleted -> True
    ToolCallFailed _ -> True
    ToolCallCancelled -> True

-- | An update about a background tool call.
data ToolCallActivity = ToolCallActivity
    { tcaSessionId :: SessionId
    -- ^ Session holding the call
    , tcaConversationId :: ConversationId
    -- ^ Conversation holding the call
    , tcaToolCallId :: ToolCallId
    -- ^ Session-level id of the call
    , tcaProviderCallId :: Maybe Text
    -- ^ Id the LLM provider gave the call, when known
    , tcaToolName :: Text
    -- ^ Name of the called tool
    , tcaPhase :: ToolCallPhase
    , tcaAt :: UTCTime
    }
    deriving (Show, Eq)

instance ToJSON ToolCallPhase where
    toJSON = \case
        ToolCallStarted -> Aeson.object ["phase" .= ("started" :: Text)]
        ToolCallProgressed payload -> Aeson.object ["phase" .= ("progressed" :: Text), "payload" .= payload]
        ToolCallCompleted -> Aeson.object ["phase" .= ("completed" :: Text)]
        ToolCallFailed err -> Aeson.object ["phase" .= ("failed" :: Text), "error" .= err]
        ToolCallCancelled -> Aeson.object ["phase" .= ("cancelled" :: Text)]

instance FromJSON ToolCallPhase where
    parseJSON = Aeson.withObject "ToolCallPhase" $ \o -> do
        p <- o .: "phase"
        case (p :: Text) of
            "started" -> pure ToolCallStarted
            "progressed" -> ToolCallProgressed <$> o .: "payload"
            "completed" -> pure ToolCallCompleted
            "failed" -> ToolCallFailed <$> o .: "error"
            "cancelled" -> pure ToolCallCancelled
            other -> fail ("unknown tool call phase: " <> Text.unpack other)

{- | Wire shape for a 'ToolCallActivity': @{tool_call_id, tool, session_id,
conversation_id, provider_call_id?, phase, payload?, error?, at}@ -- the
"..." in the spec's @tool.progressed@ payload sketch. Used both standalone
and as the body of 'System.Agents.Protocol.ToolCallProgressed'.
-}
instance ToJSON ToolCallActivity where
    toJSON act =
        withPhaseFields $
            Aeson.object
                [ "tool_call_id" .= act.tcaToolCallId
                , "tool" .= act.tcaToolName
                , "session_id" .= act.tcaSessionId
                , "conversation_id" .= act.tcaConversationId
                , "provider_call_id" .= act.tcaProviderCallId
                , "at" .= act.tcaAt
                ]
      where
        withPhaseFields (Aeson.Object o) = case toJSON act.tcaPhase of
            Aeson.Object phaseFields -> Aeson.Object (phaseFields <> o)
            _ -> Aeson.Object o
        withPhaseFields other = other

instance FromJSON ToolCallActivity where
    parseJSON = Aeson.withObject "ToolCallActivity" $ \o ->
        ToolCallActivity
            <$> o .: "session_id"
            <*> o .: "conversation_id"
            <*> o .: "tool_call_id"
            <*> o .:? "provider_call_id"
            <*> o .: "tool"
            <*> parseJSON (Aeson.Object o)
            <*> o .: "at"

{- | The subcall lifecycle and tool-activity events forwarded through
'System.Agents.Session.Base.ctxEmit' \/ 'System.Agents.Tools.Context.ctxEmit'
(@todos/os-as-standalone-server.md@, Phase 2c). Kept as a small type here,
independent of 'System.Agents.Protocol.EventBody', so that 'Session.Base'
and 'Tools.Context' do not have to depend on 'Protocol' (which itself
depends on 'Session.Base' for 'SessionId' and friends -- a cycle).
'System.Agents.Host.Runner' turns each constructor into the matching
'EventBody' (@subcall.started@\/@subcall.completed@\/@subcall.failed@\/
@tool.progressed@) and 'emit's it on the owning session's stream.
-}
data OSEmission
    = -- | Parent session id, child session id, the helper's slug, call depth.
      EmitSubcallStarted SessionId SessionId Text Int
    | -- | Child session id, the subcall's result text (when it succeeded).
      EmitSubcallCompleted SessionId (Maybe Text)
    | -- | Child session id, the failure message.
      EmitSubcallFailed SessionId Text
    | EmitToolCallActivity ToolCallActivity
    deriving (Show)
