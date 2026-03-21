{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.Session.Base (
    -- * Re-exported from Session.Types
    SessionId (..),
    TurnId (..),
    newSessionId,
    newTurnId,
    Session (..),
    Turn (..),
    UserTurnContent (..),
    LlmTurnContent (..),
    SystemPrompt (..),
    LlmResponse (..),
    LlmToolCall (..),
    UserQuery (..),
    UserToolResponse (..),
    SystemTool (..),
    SystemToolDefinition (..),
    SystemToolDefinitionV1 (..),

    -- * Defined in this module
    MissingUserPrompt (..),
    LlmCompletion (..),
    Action (..),
    ContextConfig (..),
    defaultContextConfig,
    Agent (..),
    SessionProgress (..),
    OnSessionProgress,
    ignoreSessionProgress,

    -- * Session construction helpers
    mkChildSession,
    isChildSession,
    getRootConversationId,
) where

import Data.Maybe (isJust)
import Data.Text (Text)

import System.Agents.Base (ConversationId)
import System.Agents.Tools.Context (ToolExecutionContext)

-- Re-export all session types from Session.Types for backward compatibility
import System.Agents.Session.Types

-------------------------------------------------------------------------------
-- Action and Agent types
-------------------------------------------------------------------------------

data MissingUserPrompt = MissingUserPrompt
    { missingQuery :: Bool
    , missingToolCalls :: [LlmToolCall]
    }
    deriving (Show, Eq, Ord)

-- todo: medias
data LlmCompletion = LlmCompletion
    { completePrompt :: SystemPrompt
    , completeTools :: [SystemTool]
    , completeQuery :: Maybe UserQuery
    , completeToolResponses :: [(LlmToolCall, UserToolResponse)]
    , completeConversationHistory :: [Turn]
    }
    deriving (Show, Eq, Ord)

data Action r
    = Stop r
    | AskUserPrompt MissingUserPrompt
    | AskLlmCompletion LlmCompletion
    | -- comfort/note fully-motivated below, is to evolve the agent so that th runner logic has a primitive to do so
      -- \* one advantage is it allows "pure" agents (i.e., dropping the need for an IO in usrQuery et al.)
      -- \* could consider forking but that would require a joining function (r -> r -> r) to combine results, which prevents the functorial aspects
      -- \* could consider extensiblility so that agents come with their set of decisions, but the runloop then has to account for these
      Evolve (Agent r)
    deriving (Functor)

{- | Configuration for what to include in the tool execution context.

This allows fine-grained control over the context passed to tools,
enabling performance optimizations and privacy controls.

By default, minimal context is provided to avoid performance overhead.
-}
data ContextConfig = ContextConfig
    { includeFullSession :: Bool
    -- ^ Whether to include 'ctxFullSession'
    , includeAgentId :: Bool
    -- ^ Whether to include 'ctxAgentId'
    }
    deriving (Show, Eq)

{- | Default context configuration with minimal inclusion.

* 'includeFullSession' = False (sessions can grow large)
* 'includeAgentId' = True (lightweight and commonly useful)
-}
defaultContextConfig :: ContextConfig
defaultContextConfig = ContextConfig False True

{- | An agent is a decorated step function from a session step to an action that
may yield a result r or some delay.
Functions in its body.
-}
data Agent r = Agent
    { step :: Session -> IO (Action r)
    , --
      sysPrompt :: IO SystemPrompt
    , sysTools :: IO [SystemTool]
    , usrQuery :: IO (Maybe UserQuery)
    , toolCall :: ToolExecutionContext -> LlmToolCall -> IO UserToolResponse
    , --
      complete :: LlmCompletion -> IO (LlmResponse, [LlmToolCall])
    , --
      contextConfig :: ContextConfig
    -- ^ Configuration for what to include in tool execution context
    }
    deriving (Functor)

-------------------------------------------------------------------------------
-- Session Progress Tracking
-------------------------------------------------------------------------------

{- | Represents the progress of a session through its lifecycle.
This type is used with 'OnSessionProgress' callbacks to track
session state changes in a decoupled manner.
-}
data SessionProgress
    = -- | Emitted when a new session is started
      SessionStarted Session
    | -- | Emitted after each step when the session is updated
      SessionUpdated Session
    | -- | Emitted when the session completes successfully
      SessionCompleted Session
    | -- | Emitted when the session fails with an error message
      SessionFailed Session Text
    deriving (Show, Eq)

{- | Callback type for receiving session progress updates.
This decouples the session storage mechanism from the agent loop logic.
-}
type OnSessionProgress = SessionProgress -> IO ()

-- | A no-op session progress handler for when tracking is not needed.
ignoreSessionProgress :: OnSessionProgress
ignoreSessionProgress = const (pure ())

-------------------------------------------------------------------------------
-- Session Construction Helpers
-------------------------------------------------------------------------------

{- | Create a child session linked to a parent session.

This is used when a parent agent calls a sub-agent, creating a new
session that tracks its relationship to the parent. The child session
stores references to the parent's session ID, conversation ID, and
the agent slug that initiated the call.

Example:

@
childSess <- mkChildSession parentSessId parentConvId "helper-agent"
-- Use childSess for the sub-agent call
@
-}
mkChildSession ::
    -- | Parent session ID
    SessionId ->
    -- | Parent conversation ID
    ConversationId ->
    -- | Parent agent slug (the agent that initiated this sub-agent call)
    Text ->
    IO Session
mkChildSession parentSessId parentConvId parentSlug = do
    sessId <- newSessionId
    tId <- newTurnId
    pure $
        Session
            { turns = []
            , sessionId = sessId
            , forkedFromSessionId = Nothing
            , turnId = tId
            , parentSessionId = Just parentSessId
            , parentConversationId = Just parentConvId
            , parentAgentSlug = Just parentSlug
            }

{- | Check if a session is a child session (was created by a parent agent).

Returns True if the session has a 'parentSessionId', indicating it was
created as part of a nested agent call.

Example:

@
if isChildSession sess
    then displayInHierarchy sess
    else displayAsRoot sess
@
-}
isChildSession :: Session -> Bool
isChildSession = isJust . parentSessionId

{- | Get the root conversation ID for a session.

For child sessions, this returns the parent's conversation ID (the conversation
that initiated the nested call chain). For root sessions (those without a parent),
this returns the provided conversation ID.

This function is useful for tracing the full call chain back to its origin,
enabling features like conversation hierarchy display in the TUI.

Note: Currently this returns the immediate parent's conversation ID. In the
future, this could recursively follow the parent chain if deeper ancestry
tracking is needed.

Example:

@
rootConvId <- getRootConversationId store sess
-- Use rootConvId to group related sessions in the UI
@
-}
getRootConversationId ::
    -- | The conversation ID to use if this is a root session (no parent)
    ConversationId ->
    -- | The session to get the root conversation ID for
    Session ->
    IO ConversationId
getRootConversationId rootConvId sess =
    case parentConversationId sess of
        Nothing -> pure rootConvId
        Just parentConvId -> pure parentConvId

