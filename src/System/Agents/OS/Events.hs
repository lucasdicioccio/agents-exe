{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Event types for subcall visibility and TUI\/runner integration.

This module provides event types used for communication between the OS
layer, the session runner and its clients, particularly for tracking
subcall lifecycle (started, progress, completed, failed) and tool-call
activity.

By placing these types in a separate module, we avoid circular dependencies
between Session.Base, Tools.Context and Protocol.
-}
module System.Agents.OS.Events (
    -- * Tool-call activity
    ToolCallActivity (..),
    ToolCallPhase (..),
    isFinalToolCallPhase,

    -- * Emission (ctxEmit)
    OSEmission (..),
    newQueueEmitter,
    queueEmitter,
) where

import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, writeTQueue)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)

import System.Agents.Base (ConversationId)
import System.Agents.Session.Types (SessionId, ToolCallId)

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

{- | The single in-library event emission type: subcall lifecycle,
tool-activity and hook-failure events forwarded through
'System.Agents.Session.Base.ctxEmit' \/ 'System.Agents.Tools.Context.ctxEmit'
(@todos/os-as-standalone-server.md@, Phase 2c\/3c). It is the one emission
mechanism now that 'OSEvent' and @ctxEventQueue@ are retired (Phase 3c).
Kept as a small type here, independent of 'System.Agents.Protocol.EventBody',
so that 'Session.Base' and 'Tools.Context' do not have to depend on
'Protocol' (which itself depends on 'Session.Base' for 'SessionId' and
friends -- a cycle). 'System.Agents.Host.Runner' turns each constructor into
the matching 'EventBody' (@subcall.started@\/@subcall.completed@\/
@subcall.failed@\/@tool.progressed@\/@hook.failed@) and 'emit's it on the
owning session's stream. A local consumer that is not a runner (a CLI path,
a test) installs 'System.Agents.OS.Events.queueEmitter' instead.
-}
data OSEmission
    = -- | Parent session id, child session id, the helper's slug, call depth.
      EmitSubcallStarted SessionId SessionId Text Int
    | -- | Child session id, the subcall's result text (when it succeeded).
      EmitSubcallCompleted SessionId (Maybe Text)
    | -- | Child session id, the failure message.
      EmitSubcallFailed SessionId Text
    | EmitToolCallActivity ToolCallActivity
    | -- | A hook (e.g. a tool-call middleware) failed outside of the normal
      -- tool-call result path. Not a session failure: the run continues.
      EmitError Text
    deriving (Show)

{- | Create an in-memory queue and an emitter function that writes to it, for
local (non-runner) consumers that used to drain @ctxEventQueue@: install the
returned function as 'System.Agents.Session.Base.ctxEmit' \/
'System.Agents.Tools.Context.ctxEmit' and read emissions back off the queue.
-}
newQueueEmitter :: IO (TQueue OSEmission, OSEmission -> IO ())
newQueueEmitter = do
    q <- newTQueueIO
    pure (q, queueEmitter q)

-- | An emitter that writes every emission to the given queue.
queueEmitter :: TQueue OSEmission -> (OSEmission -> IO ())
queueEmitter q emission = atomically $ writeTQueue q emission
