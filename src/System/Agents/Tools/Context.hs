{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Tool execution context for passing session metadata to tools.

This module provides a structured record that encapsulates all session-related
metadata that tools might need access to during execution. The context is
intentionally kept separate from LLM-facing types to maintain a clean
separation of concerns.

Design decisions:

* Structured record (not generic Aeson.Value) for type safety and clarity
* Optional 'Session' allows performance optimization when full context isn't needed
* Separate module to maintain clean import hierarchy
* Recursion tracking via 'CallStackEntry' for nested agent calls
* Tool portal for inter-toolbox communication
-}
module System.Agents.Tools.Context (
    -- * Core types
    ToolExecutionContext (..),
    CallStackEntry (..),
    RecursionError (..),
    ToolExecutionContextSnapshot (..),
    contextSnapshot,
    hydrateContextSnapshot,

    -- * Tool Portal types
    TraceId,
    ToolCall (..),
    ToolResult (..),
    ToolPortal,

    -- * Construction
    mkToolExecutionContext,
    mkMinimalContext,
    mkRootContext,
    mkPortalContext,
    mkSubcallContext,
    pushAgentContext,

    -- * Access helpers
    hasFullSession,
    hasAgentId,

    -- * Recursion tracking helpers
    currentRecursionDepth,
    isAtDepth,
    callChain,
    isAgentInCallStack,

    -- * Security helpers
    isToolAllowed,

    -- * Subcall helpers
    getSubcallDepth,
    isSubcallContext,
) where

import Control.Concurrent.STM (STM)
import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import GHC.Generics (Generic)

import qualified Data.Map.Strict as Map

import System.Agents.Base (AgentId, ConversationId)
import System.Agents.OS.Core.World (World)
import System.Agents.OS.Events (OSEmission)
import System.Agents.Session.Mailbox (MailRouter, SpawnSession, UnwatchSession, WatchSession)
import System.Agents.Session.Types (Envelope, Session, SessionId, ToolCallId, TrackedToolCall, TurnId)
import System.Agents.Tools.Bindings.Types (DerivedNarrowing (..), ScopedBinding (..))
import System.Agents.Tools.Params.Types (ParamValue (..), Params)

-------------------------------------------------------------------------------
-- Call Stack Entry
-------------------------------------------------------------------------------

{- | Information about a single level in the agent call stack.

Each entry tracks which agent is being called at a specific recursion
depth, along with the conversation ID for that level. The call stack
enables:

* Recursion depth tracking for safety limits
* Call chain observability for debugging
* Context-aware behavior based on position in stack

=== Fields

* 'callAgentSlug' - The slug identifying the agent at this level
* 'callConversationId' - The conversation ID for this level
* 'callDepth' - The recursion depth (0 = root)
-}
data CallStackEntry = CallStackEntry
    { callAgentSlug :: Text
    {- ^ The slug of the agent at this level.
    Use "root" for the initial context before any agent calls.
    -}
    , callConversationId :: ConversationId
    -- ^ The conversation ID for this level in the call stack.
    , callDepth :: Int
    -- ^ The recursion depth (0 = root, 1 = first nested call, etc.)
    }
    deriving (Show, Eq, Generic)

{- | JSON serialization support for 'CallStackEntry'.
Enables persistence and transmission of call stack information.
-}
instance ToJSON CallStackEntry

instance FromJSON CallStackEntry

-------------------------------------------------------------------------------
-- Recursion Error
-------------------------------------------------------------------------------

{- | Error type for recursion limit violations.

This type represents errors that occur when recursion limits are exceeded,
providing the full call stack for debugging purposes.
-}
data RecursionError
    = {- | The maximum configured recursion depth was exceeded.
      The call stack at the time of failure is provided for debugging.
      -}
      MaxRecursionDepthExceeded [CallStackEntry]
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Tool Portal Types
-------------------------------------------------------------------------------

{- | Unique identifier for tool invocation traces.

Using Text for simplicity. Can be strengthened to a newtype later if needed.
-}
type TraceId = Text

{- | A tool call from Lua (or another tool) to another tool through the portal.

This represents a synchronous call where the caller waits for the result.
-}
data ToolCall = ToolCall
    { callToolName :: Text
    -- ^ The name of the tool to invoke (e.g., "bash", "sqlite_query")
    , callArgs :: Aeson.Value
    -- ^ JSON arguments for the tool
    }
    deriving (Show, Eq, Generic)

instance ToJSON ToolCall
instance FromJSON ToolCall

-- | Result of a tool invocation through the portal.
data ToolResult = ToolResult
    { resultData :: Aeson.Value
    -- ^ The result data from the tool
    , resultDuration :: NominalDiffTime
    -- ^ How long the invocation took
    , resultTraceId :: TraceId
    -- ^ Trace ID for book-keeping and call tree visualization
    }
    deriving (Show, Eq, Generic)

instance ToJSON ToolResult
instance FromJSON ToolResult

{- | The tool portal: a callback function that allows tools to invoke other tools.

This is the core inter-toolbox communication mechanism. When Lua (or another
tool) wants to call another tool, it uses this portal.

The portal accepts an optional parent context to propagate OS integration
fields (World, EventQueue) to nested tool calls. This enables TUI visibility
for subcalls initiated from Lua scripts.

Note: The portal is synchronous (IO ToolResult) for simplicity.
Async support can be added later if needed.
-}
type ToolPortal = Maybe ToolExecutionContext -> ToolCall -> IO ToolResult

dummyPortal :: ToolPortal
dummyPortal _parentCtx _call =
    pure $
        ToolResult
            { resultData = Aeson.object [("error", Aeson.String "Tool portal not available during initialization")]
            , resultDuration = 0
            , resultTraceId = "dummy"
            }

-------------------------------------------------------------------------------
-- Tool Execution Context
-------------------------------------------------------------------------------

{- | Context containing session metadata passed to tools during execution.

This record encapsulates identifiers and optional full session data that
tools may need to access without exposing these details to the LLM.

The context is designed to be:

* Serializable (ToJSON/FromJSON instances) for potential persistence or
  transmission
* Optional where appropriate (e.g., 'ctxFullSession', 'ctxAgentId') to
  support different use cases
* LLM-agnostic - no types from LLM interfaces are included
* Recursion-aware - tracks call stack for nested agent invocations
* Portal-enabled - supports inter-toolbox communication
* OS-integrated - supports OS World and event emission for TUI visibility

=== Fields

* 'ctxSessionId' - Unique identifier for the current session. Always present.
* 'ctxConversationId' - Identifier for the conversation this session belongs to.
* 'ctxTurnId' - Identifier for the current turn within the session.
* 'ctxAgentId' - Optional identifier for the agent executing the tool.
  May be 'Nothing' for anonymous or system-level tool executions.
* 'ctxFullSession' - Optional complete serialized session. Tools that need
  access to conversation history or other session state can use this.
  Left as 'Nothing' for performance when only identifiers are needed.
* 'ctxCallStack' - The chain of call stack entries tracking nested agent calls.
  Root entry is at the end of the list (appended with ':').
* 'ctxMaxDepth' - Optional maximum recursion depth limit. When 'Just n',
  agent calls beyond depth n will fail with 'MaxRecursionDepthExceeded'.
* 'ctxToolPortal' - Optional tool portal for inter-toolbox communication.
  When present, tools can invoke other tools through this callback.
* 'ctxAllowedTools' - Whitelist of tool names allowed in this context.
  Empty list means all tools are allowed (backward compatibility).
* 'ctxWorld' - Optional OS World for ECS operations. When present, subcalls
  can insert conversations into the OS for TUI visibility.
* 'ctxEmit' - Optional emission hook. When present, subcalls emit events to
  notify a runner (or other local consumer) of their lifecycle.
* 'ctxParentConversation' - Optional parent conversation ID for subcalls.
  When present, indicates this context is for a nested agent invocation.
* 'ctxProgressCallback' - Optional callback a tool can use to emit
  structured progress updates while it is running. The callback accepts
  a JSON value and writes it into the OS entity for this tool call.
  When absent, tools should not attempt to stream partial results.
-}
data ToolExecutionContext = ToolExecutionContext
    { ctxSessionId :: SessionId
    {- ^ The current session identifier. This uniquely identifies the
    session within which the tool is being executed.
    -}
    , ctxConversationId :: ConversationId
    {- ^ The conversation identifier that groups related sessions together.
    Multiple sessions may belong to the same conversation (e.g., forked sessions).
    -}
    , ctxTurnId :: TurnId
    {- ^ The current turn identifier within the session. This allows tools
    to understand their position in the conversation flow.
    -}
    , ctxAgentId :: Maybe AgentId
    {- ^ Optional agent identifier. Present when a specific agent is executing
    the tool, 'Nothing' for system-level or anonymous executions.
    -}
    , ctxFullSession :: Maybe Session
    {- ^ Optional full serialized session. When present, tools can access
    complete conversation history and session state. This is made optional
    to avoid performance overhead when not needed and to prevent circular
    dependencies in the module hierarchy.
    -}
    , ctxCallStack :: [CallStackEntry]
    {- ^ The call chain for recursion tracking. The list is ordered with
    the most recent call first (head) and root context last (tail).
    -}
    , ctxMaxDepth :: Maybe Int
    {- ^ Optional maximum recursion depth. When specified, nested agent
    calls that would exceed this depth will fail with a 'RecursionError'.
    -}
    , ctxToolPortal :: ToolPortal
    {- ^ Tool portal for inter-toolbox communication. When present,
    tools can use this callback to invoke other tools. This enables Lua
    scripts and other tools to orchestrate multiple tool calls.

    The portal accepts an optional parent context to propagate OS integration
    fields to nested calls, enabling TUI visibility for subcalls.
    -}
    , ctxAllowedTools :: [Text]
    {- ^ Whitelist of tool names allowed in this context. An empty list
    means all tools are allowed (for backward compatibility). This is
    checked before invoking tools through the portal.
    -}
    , ctxWorld :: Maybe World
    {- ^ Optional OS World for ECS operations. When present, tools can
    insert entities and components into the OS. This enables subcall
    conversations to be visible in the TUI.
    -}
    , ctxEmit :: Maybe (OSEmission -> IO ())
    {- ^ Optional hook (@todos/os-as-standalone-server.md@, Phase 2c\/3c):
    subcall lifecycle \/ tool-call activity \/ hook-failure events, reported
    so a runner can broadcast them on its event stream. Copied straight
    from 'System.Agents.Session.Base.Agent.ctxEmit'.
    -}
    , ctxParentConversation :: Maybe ConversationId
    {- ^ Optional parent conversation ID for subcalls. When present,
    indicates this context is for a nested agent invocation, enabling
    proper lineage tracking in the OS.
    -}
    , ctxProgressCallback :: Maybe (Aeson.Value -> IO ())
    {- ^ Optional progress callback for streaming tool-call updates.
    When present, a running tool can emit structured JSON progress
    payloads; the async engine persists them on the OS entity.
    -}
    , ctxCancelToolCall :: Maybe (ToolCallId -> IO Bool)
    {- ^ Optional hook to cancel a call running in the async engine. It
    interrupts the background thread and returns 'False' when the engine
    does not own a running call with that id.
    -}
    , ctxRecordChildSession :: Maybe (SessionId -> IO ())
    {- ^ Phase 4 (@todos/session-mailbox.md@ §5): optional hook a
    @prompt_agent_\<slug\>@ call uses to report its own child session id
    back to its tracking entity, once the child session exists. Built by
    the async engine per call (it alone knows the call's own 'EntityId')
    and written onto the call's 'System.Agents.OS.Conversation.Types.ToolCallConfig'
    component; 'System.Agents.Session.Step.pollRunningCall' copies it onto
    'TrackedToolCall.tcChildSessionId' so a detached placeholder can show it
    (see 'System.Agents.Session.Types.partialToolMessages'). 'Nothing' for
    calls with no OS entity (inline calls) or outside the async engine.
    -}
    , ctxAwaitMail :: Maybe (STM [Envelope])
    {- ^ Phase 2 (@todos/session-mailbox.md@ §3): optional hook the @wait@
    capability uses to block (in the same 'Control.Concurrent.STM.orElse'
    transaction as its other arms) until mail arrives past the session's
    current 'Session.Types.mailCursor'. Built once per turn from the
    agent's mailbox and that cursor (see 'System.Agents.Session.Step.buildContext'),
    so every call in the turn shares the same view of "what's new".
    'Nothing' when the agent has no mailbox.
    -}
    , ctxMailRouter :: Maybe MailRouter
    {- ^ Phase 4 (@todos/session-mailbox.md@ §5): optional process-wide
    table of live mailboxes, used by @send-message@ \/ @spawn-session@ to
    address another session by id. Copied straight from 'Agent.ctxMailRouter'.
    'Nothing' when the agent has no router.
    -}
    , ctxSpawnSession :: Maybe SpawnSession
    {- ^ Phase 4 (@todos/session-mailbox.md@ §5): optional @spawn-session@
    hook. Copied straight from 'Agent.ctxSpawnSession'. 'Nothing' when the
    front-end has not installed one.
    -}
    , ctxWatchSession :: Maybe WatchSession
    {- ^ Phase 6 (@todos/session-mailbox.md@ §7): optional @watch-session@
    hook. Copied straight from 'Agent.ctxWatchSession'.
    -}
    , ctxUnwatchSession :: Maybe UnwatchSession
    {- ^ Phase 6 (@todos/session-mailbox.md@ §7): optional @unwatch-session@
    hook, paired with 'ctxWatchSession'.
    -}
    , ctxSessionToolCalls :: [TrackedToolCall]
    {- ^ Tracked calls of the session's unfinished turns. Lets the
    tool-call capabilities answer about calls that have no OS entity —
    typically calls started by a process that is gone — without carrying
    the whole session in 'ctxFullSession'.
    -}
    , ctxParams :: Params
    {- ^ Resolved parameter values available for tool argument bindings
    (see @todos/tool-partial-application.md@). Never includes anything the
    LLM sent; only what the process, session or message supplied.
    -}
    , ctxInheritedBindings :: [ScopedBinding]
    {- ^ Bindings an ancestor placed on a helper reachable below this
    agent, already re-rooted at this agent's position
    (@todos/tool-partial-application.md@, §8.3). 'runSubAgent' filters and
    re-roots this list again when it prompts one of this agent's own
    helpers.
    -}
    , ctxDerivedNarrowings :: Map.Map (Text, Text) DerivedNarrowing
    {- ^ The session's narrowings named by @derive_agent@ (@todos/tool-
    partial-application.md@, §8.4), keyed by @(the helper's slug, the
    model-chosen name)@: a fold over the session's own history, nothing
    else. Like 'ctxSessionToolCalls', recomputed fresh wherever this
    context is built from a live session; empty when built without one.
    -}
    }
    deriving (Generic)

-- | Custom Eq instance for ToolExecutionContext that handles non-comparable fields
instance Eq ToolExecutionContext where
    (==) a b =
        ctxSessionId a == ctxSessionId b
            && ctxConversationId a == ctxConversationId b
            && ctxTurnId a == ctxTurnId b
            && ctxAgentId a == ctxAgentId b
            && ctxFullSession a == ctxFullSession b
            && ctxCallStack a == ctxCallStack b
            && ctxMaxDepth a == ctxMaxDepth b
            && ctxAllowedTools a == ctxAllowedTools b
            && ctxParentConversation a == ctxParentConversation b
            && ctxParams a == ctxParams b
            && ctxInheritedBindings a == ctxInheritedBindings b
            && ctxDerivedNarrowings a == ctxDerivedNarrowings b

-- Note: ctxToolPortal, ctxWorld, ctxEmit, and ctxProgressCallback are not compared
-- (functions and TVars can't be compared)

-- | Custom Show instance for ToolExecutionContext that handles non-showable fields
instance Show ToolExecutionContext where
    show ctx =
        "ToolExecutionContext {"
            ++ " ctxSessionId = "
            ++ show (ctxSessionId ctx)
            ++ ", ctxConversationId = "
            ++ show (ctxConversationId ctx)
            ++ ", ctxTurnId = "
            ++ show (ctxTurnId ctx)
            ++ ", ctxAgentId = "
            ++ show (ctxAgentId ctx)
            ++ ", ctxFullSession = "
            ++ show (ctxFullSession ctx)
            ++ ", ctxCallStack = "
            ++ show (ctxCallStack ctx)
            ++ ", ctxMaxDepth = "
            ++ show (ctxMaxDepth ctx)
            ++ ", ctxToolPortal = "
            ++ portalStr
            ++ ", ctxAllowedTools = "
            ++ show (ctxAllowedTools ctx)
            ++ ", ctxWorld = "
            ++ worldStr
            ++ ", ctxEmit = "
            ++ emitStr
            ++ ", ctxParentConversation = "
            ++ show (ctxParentConversation ctx)
            ++ ", ctxProgressCallback = "
            ++ progressCallbackStr
            ++ ", ctxCancelToolCall = "
            ++ cancelHookStr
            ++ ", ctxRecordChildSession = "
            ++ recordChildSessionStr
            ++ ", ctxSessionToolCalls = "
            ++ show (length (ctxSessionToolCalls ctx))
            ++ ", ctxParams = "
            ++ show (ctxParams ctx)
            ++ ", ctxInheritedBindings = "
            ++ show (ctxInheritedBindings ctx)
            ++ ", ctxDerivedNarrowings = "
            ++ show (ctxDerivedNarrowings ctx)
            ++ " }"
      where
        portalStr = "<portal>"
        worldStr = "<world>"
        emitStr = "<emit>"
        progressCallbackStr = "<progressCallback>"
        cancelHookStr = "<cancelToolCall>"
        recordChildSessionStr = "<recordChildSession>"

{- | JSON serialization support for 'ToolExecutionContext'.
Note: The tool portal function, world, and event queue are not serialized.
-}
instance ToJSON ToolExecutionContext where
    toJSON ctx =
        Aeson.object
            [ "sessionId" .= ctxSessionId ctx
            , "conversationId" .= ctxConversationId ctx
            , "turnId" .= ctxTurnId ctx
            , "agentId" .= ctxAgentId ctx
            , "fullSession" .= ctxFullSession ctx
            , "callStack" .= ctxCallStack ctx
            , "maxDepth" .= ctxMaxDepth ctx
            , "allowedTools" .= ctxAllowedTools ctx
            , "parentConversation" .= ctxParentConversation ctx
            , "params" .= ctxParams ctx
            , "inheritedBindings" .= filter (not . sbSecret) (ctxInheritedBindings ctx)
            , "derivedNarrowings" .= Map.toList (ctxDerivedNarrowings ctx)
            -- Note: ctxToolPortal, ctxWorld, ctxEmit, and
            -- ctxProgressCallback are intentionally omitted (not serializable).
            -- ctxParams serializes through ParamValue's redacting ToJSON, and
            -- a secret-valued inherited binding is dropped outright, so
            -- secret values never appear here either.
            ]

instance FromJSON ToolExecutionContext where
    parseJSON = Aeson.withObject "ToolExecutionContext" $ \v ->
        ToolExecutionContext
            <$> v .: "sessionId"
            <*> v .: "conversationId"
            <*> v .: "turnId"
            <*> v .: "agentId"
            <*> v .: "fullSession"
            <*> v .: "callStack"
            <*> v .: "maxDepth"
            <*> pure dummyPortal
            <*> v .: "allowedTools"
            <*> pure Nothing
            <*> pure Nothing
            <*> v .: "parentConversation"
            <*> pure Nothing
            <*> pure Nothing
            <*> pure Nothing
            <*> pure Nothing
            <*> pure Nothing
            <*> pure Nothing
            <*> pure Nothing
            <*> pure Nothing
            <*> pure []
            <*> pure Map.empty
            <*> pure []
            <*> pure Map.empty


{- | Serializable subset of 'ToolExecutionContext' suitable for durable
continuation snapshots.

Omits the non-serializable runtime fields ('ctxToolPortal', 'ctxWorld',
'ctxEmit') so that a paused call can be persisted and later re-hydrated in
a different process.
-}
data ToolExecutionContextSnapshot = ToolExecutionContextSnapshot
    { tecsSessionId :: SessionId
    , tecsConversationId :: ConversationId
    , tecsTurnId :: TurnId
    , tecsCallStack :: [CallStackEntry]
    , tecsAllowedTools :: [Text]
    , tecsParentConversation :: Maybe ConversationId
    , tecsParams :: Params
    {- ^ Non-secret parameter values only: secrets are memory-only and never
    survive a snapshot (see @todos/tool-partial-application.md@, §5/§6). A
    re-hydrated call whose tool needs a secret parameter fails at call time.
    -}
    , tecsInheritedBindings :: [ScopedBinding]
    -- ^ Non-secret inherited bindings only, for the same reason as 'tecsParams'.
    }
    deriving (Show, Eq, Generic)

instance ToJSON ToolExecutionContextSnapshot
instance FromJSON ToolExecutionContextSnapshot

-- | Extract the serializable snapshot from a full execution context.
contextSnapshot :: ToolExecutionContext -> ToolExecutionContextSnapshot
contextSnapshot ctx =
    ToolExecutionContextSnapshot
        { tecsSessionId = ctxSessionId ctx
        , tecsConversationId = ctxConversationId ctx
        , tecsTurnId = ctxTurnId ctx
        , tecsCallStack = ctxCallStack ctx
        , tecsAllowedTools = ctxAllowedTools ctx
        , tecsParentConversation = ctxParentConversation ctx
        , tecsParams = Map.filter (not . pvSecret) (ctxParams ctx)
        , tecsInheritedBindings = filter (not . sbSecret) (ctxInheritedBindings ctx)
        }

{- | Re-hydrate a full execution context from a snapshot.

The runtime fields ('ctxToolPortal', 'ctxWorld', 'ctxEmit') are supplied by
the caller; the remaining fields are restored from the snapshot.
-}
hydrateContextSnapshot ::
    ToolPortal ->
    Maybe World ->
    ToolExecutionContextSnapshot ->
    ToolExecutionContext
hydrateContextSnapshot portal mWorld snap =
    ToolExecutionContext
        { ctxSessionId = tecsSessionId snap
        , ctxConversationId = tecsConversationId snap
        , ctxTurnId = tecsTurnId snap
        , ctxAgentId = Nothing
        , ctxFullSession = Nothing
        , ctxCallStack = tecsCallStack snap
        , ctxMaxDepth = Nothing
        , ctxToolPortal = portal
        , ctxAllowedTools = tecsAllowedTools snap
        , ctxWorld = mWorld
        , ctxEmit = Nothing
        , ctxParentConversation = tecsParentConversation snap
        , ctxProgressCallback = Nothing
        , ctxCancelToolCall = Nothing
        , ctxRecordChildSession = Nothing
        , ctxAwaitMail = Nothing
        , ctxMailRouter = Nothing
        , ctxSpawnSession = Nothing
        , ctxWatchSession = Nothing
        , ctxUnwatchSession = Nothing
        , ctxSessionToolCalls = []
        , ctxParams = tecsParams snap
        , ctxInheritedBindings = tecsInheritedBindings snap
        , ctxDerivedNarrowings = Map.empty
        }
-------------------------------------------------------------------------------
-- Construction Helpers
-------------------------------------------------------------------------------

mkToolExecutionContext ::
    SessionId ->
    ConversationId ->
    TurnId ->
    Maybe AgentId ->
    Maybe Session ->
    ToolPortal ->
    [CallStackEntry] ->
    Maybe Int ->
    ToolExecutionContext
mkToolExecutionContext sessId convId tId mAgentId mSession portal stack maxDepth =
    ToolExecutionContext
        { ctxSessionId = sessId
        , ctxConversationId = convId
        , ctxTurnId = tId
        , ctxAgentId = mAgentId
        , ctxFullSession = mSession
        , ctxCallStack = stack
        , ctxMaxDepth = maxDepth
        , ctxToolPortal = portal
        , ctxAllowedTools = []
        , ctxWorld = Nothing
        , ctxEmit = Nothing
        , ctxParentConversation = Nothing
        , ctxProgressCallback = Nothing
        , ctxCancelToolCall = Nothing
        , ctxRecordChildSession = Nothing
        , ctxAwaitMail = Nothing
        , ctxMailRouter = Nothing
        , ctxSpawnSession = Nothing
        , ctxWatchSession = Nothing
        , ctxUnwatchSession = Nothing
        , ctxSessionToolCalls = []
        , ctxParams = Map.empty
        , ctxInheritedBindings = []
        , ctxDerivedNarrowings = Map.empty
        }

{- | Create a minimal 'ToolExecutionContext' with only required identifiers.

Use this when you don't have (or don't need) agent identification or
full session data. This is appropriate for simple tools that only need
to log or track their execution context.

Note: This creates a root-level context with an empty call stack.
For proper recursion tracking, use 'mkRootContext' instead.

Example:

@
context <- mkMinimalContext sessionId conversationId turnId portal
@
-}
mkMinimalContext ::
    SessionId ->
    ConversationId ->
    TurnId ->
    ToolPortal ->
    ToolExecutionContext
mkMinimalContext sessId convId tId portal =
    ToolExecutionContext
        { ctxSessionId = sessId
        , ctxConversationId = convId
        , ctxTurnId = tId
        , ctxAgentId = Nothing
        , ctxFullSession = Nothing
        , ctxCallStack = []
        , ctxMaxDepth = Nothing
        , ctxToolPortal = portal
        , ctxAllowedTools = []
        , ctxWorld = Nothing
        , ctxEmit = Nothing
        , ctxParentConversation = Nothing
        , ctxProgressCallback = Nothing
        , ctxCancelToolCall = Nothing
        , ctxRecordChildSession = Nothing
        , ctxAwaitMail = Nothing
        , ctxMailRouter = Nothing
        , ctxSpawnSession = Nothing
        , ctxWatchSession = Nothing
        , ctxUnwatchSession = Nothing
        , ctxSessionToolCalls = []
        , ctxParams = Map.empty
        , ctxInheritedBindings = []
        , ctxDerivedNarrowings = Map.empty
        }

{- | Create a root-level context for the start of agent execution (depth 0).

This constructor should be used when initializing the first context
in an agent execution flow. It creates a call stack with a single
"root" entry at depth 0.

Example:

@
context <- mkRootContext
    sessionId
    conversationId
    turnId
    (Just agentId)
    (Just fullSession)
    portal
    (Just 5)  -- max depth of 5
@
-}
mkRootContext ::
    SessionId ->
    ConversationId ->
    TurnId ->
    Maybe AgentId ->
    Maybe Session ->
    ToolPortal ->
    -- | Optional maximum recursion depth limit
    Maybe Int ->
    ToolExecutionContext
mkRootContext sessId convId tId mAgentId mSession portal maxDepth =
    ToolExecutionContext
        { ctxSessionId = sessId
        , ctxConversationId = convId
        , ctxTurnId = tId
        , ctxAgentId = mAgentId
        , ctxFullSession = mSession
        , ctxCallStack = [CallStackEntry "root" convId 0]
        , ctxMaxDepth = maxDepth
        , ctxToolPortal = portal
        , ctxAllowedTools = []
        , ctxWorld = Nothing
        , ctxEmit = Nothing
        , ctxParentConversation = Nothing
        , ctxProgressCallback = Nothing
        , ctxCancelToolCall = Nothing
        , ctxRecordChildSession = Nothing
        , ctxAwaitMail = Nothing
        , ctxMailRouter = Nothing
        , ctxSpawnSession = Nothing
        , ctxWatchSession = Nothing
        , ctxUnwatchSession = Nothing
        , ctxSessionToolCalls = []
        , ctxParams = Map.empty
        , ctxInheritedBindings = []
        , ctxDerivedNarrowings = Map.empty
        }

{- | Create a context with tool portal support.

This constructor is used when setting up a context that supports
inter-toolbox communication through the tool portal mechanism.

Example:

@
context <- mkPortalContext
    sessionId
    conversationId
    turnId
    (Just agentId)
    (Just fullSession)
    [CallStackEntry "root" conversationId 0]
    (Just 5)
    portal  -- the portal function
    ["bash", "sqlite"]  -- allowed tools
@
-}
mkPortalContext ::
    SessionId ->
    ConversationId ->
    TurnId ->
    Maybe AgentId ->
    Maybe Session ->
    [CallStackEntry] ->
    Maybe Int ->
    ToolPortal ->
    [Text] -> -- allowed tools
    ToolExecutionContext
mkPortalContext sessId convId tId mAgentId mSession stack maxDepth portal allowed =
    ToolExecutionContext
        { ctxSessionId = sessId
        , ctxConversationId = convId
        , ctxTurnId = tId
        , ctxAgentId = mAgentId
        , ctxFullSession = mSession
        , ctxCallStack = stack
        , ctxMaxDepth = maxDepth
        , ctxToolPortal = portal
        , ctxAllowedTools = allowed
        , ctxWorld = Nothing
        , ctxEmit = Nothing
        , ctxParentConversation = Nothing
        , ctxProgressCallback = Nothing
        , ctxCancelToolCall = Nothing
        , ctxRecordChildSession = Nothing
        , ctxAwaitMail = Nothing
        , ctxMailRouter = Nothing
        , ctxSpawnSession = Nothing
        , ctxWatchSession = Nothing
        , ctxUnwatchSession = Nothing
        , ctxSessionToolCalls = []
        , ctxParams = Map.empty
        , ctxInheritedBindings = []
        , ctxDerivedNarrowings = Map.empty
        }

{- | Create a nested context for subcall execution with OS integration.

This constructor is used when setting up a context for a nested agent
call (subcall) that needs OS integration for TUI visibility. It includes
all the fields needed for the subcall to insert itself into the OS World
and emit events.

Example:

@
case pushAgentContext "helper-agent" newConvId parentCtx of
    Left err -> handleRecursionError err
    Right baseCtx -> do
        let subcallCtx = mkSubcallContext
                baseCtx
                (Just world)        -- OS World
                parentConvId        -- Parent conversation
        runSubAgent subcallCtx query
@
-}
mkSubcallContext ::
    -- | Base context from pushAgentContext
    ToolExecutionContext ->
    -- | Optional OS World for ECS operations
    Maybe World ->
    -- | Parent conversation ID (required for subcalls)
    ConversationId ->
    ToolExecutionContext
mkSubcallContext baseCtx mWorld parentConvId =
    baseCtx
        { ctxWorld = mWorld
        , ctxParentConversation = Just parentConvId
        , -- The parent's hooks target the parent's tool call and engine; the
          -- sub-agent's own steps install hooks for its calls.
          ctxProgressCallback = Nothing
        , ctxCancelToolCall = Nothing
        , ctxRecordChildSession = Nothing
        , ctxAwaitMail = Nothing
        , ctxMailRouter = Nothing
        , ctxSpawnSession = Nothing
        , ctxWatchSession = Nothing
        , ctxUnwatchSession = Nothing
        , ctxSessionToolCalls = []
        }

{- | Create a nested context when calling a sub-agent.

This function pushes a new entry onto the call stack and checks
against the configured maximum recursion depth. If the depth limit
would be exceeded, returns a 'RecursionError'.

The call stack grows with the most recent entry at the head of the list.
The depth of the new entry is the length of the current call stack.

Example:

@
case pushAgentContext "helper-agent" newConvId parentCtx of
    Left err -> handleRecursionError err
    Right newCtx -> continueWithSubAgent newCtx
@
-}
pushAgentContext ::
    -- | The slug of the agent being called
    Text ->
    -- | The new conversation ID for this sub-agent call
    ConversationId ->
    -- | The parent context
    ToolExecutionContext ->
    -- | Either an error (if depth exceeded) or the new context
    Either RecursionError ToolExecutionContext
pushAgentContext slug newConvId parentCtx =
    let newDepth = length parentCtx.ctxCallStack
        newEntry = CallStackEntry slug newConvId newDepth
     in case parentCtx.ctxMaxDepth of
            Just maxD
                | newDepth >= maxD ->
                    Left $ MaxRecursionDepthExceeded parentCtx.ctxCallStack
            _ ->
                Right $
                    parentCtx
                        { ctxConversationId = newConvId
                        , ctxCallStack = newEntry : parentCtx.ctxCallStack
                        }

-------------------------------------------------------------------------------
-- Query Helpers
-------------------------------------------------------------------------------

{- | Check if the context contains a full session.

Useful for tools that need to decide whether they can access conversation
history or other session state.

@
if hasFullSession ctx
    then processWithHistory (fromJust $ ctxFullSession ctx)
    else processWithoutHistory
@
-}
hasFullSession :: ToolExecutionContext -> Bool
hasFullSession = maybe False (const True) . ctxFullSession

{- | Check if the context has an associated agent identifier.

Tools can use this to determine if they're running in an agent context
or as a system-level operation.
-}
hasAgentId :: ToolExecutionContext -> Bool
hasAgentId = maybe False (const True) . ctxAgentId

{- | Get current recursion depth.

Returns the number of entries in the call stack. A root context
created with 'mkRootContext' has depth 1 (the "root" entry).

@
depth = currentRecursionDepth ctx
-- depth >= 1 always (for properly initialized contexts)
@
-}
currentRecursionDepth :: ToolExecutionContext -> Int
currentRecursionDepth = length . ctxCallStack

{- | Check if we're at or beyond a specific depth.

Useful for tools that need to behave differently at different
recursion depths (e.g., limiting behavior in deeply nested calls).

@
if isAtDepth 3 ctx
    then useSimpleMode
    else useFullMode
@
-}
isAtDepth :: Int -> ToolExecutionContext -> Bool
isAtDepth n ctx = currentRecursionDepth ctx >= n

{- | Get the full call chain as a list (root first).

Returns the call stack entries in order from root to current.
This is useful for debugging and displaying the full call chain.

@
chain = callChain ctx
-- chain !! 0 is the root entry
-- last chain is the current entry
@
-}
callChain :: ToolExecutionContext -> [CallStackEntry]
callChain = reverse . ctxCallStack

{- | Check if a specific agent is already in the call stack (cycle detection).

This can be used to detect potential infinite loops caused by
circular agent references before they occur.

@
if isAgentInCallStack "agent-a" ctx
    then preventCycle  -- agent-a is already in the call chain
    else continueWithCall
@
-}
isAgentInCallStack :: Text -> ToolExecutionContext -> Bool
isAgentInCallStack slug ctx =
    any ((== slug) . callAgentSlug) ctx.ctxCallStack

-------------------------------------------------------------------------------
-- Security Helpers
-------------------------------------------------------------------------------

{- | Check if a tool is allowed in this context.

An empty allowed list means all tools are allowed (backward compatibility).
This check should be performed before invoking tools through the portal.

@
if isToolAllowed "bash" ctx
    then invokeTool "bash" args
    else return $ ToolError "Tool not allowed"
@
-}
isToolAllowed :: Text -> ToolExecutionContext -> Bool
isToolAllowed toolName ctx =
    -- Empty allowed list means all tools allowed (backward compatibility)
    null (ctxAllowedTools ctx) || toolName `elem` ctxAllowedTools ctx

-------------------------------------------------------------------------------
-- Subcall Helpers
-------------------------------------------------------------------------------

{- | Get the subcall depth (0 if not a subcall).

Returns 0 for root conversations, 1+ for nested subcalls.
This is calculated from the ctxParentConversation field.

@
depth = getSubcallDepth ctx
-- depth == 0: root conversation
-- depth > 0: nested subcall
@
-}
getSubcallDepth :: ToolExecutionContext -> Int
getSubcallDepth ctx = case ctxParentConversation ctx of
    Nothing -> 0
    Just _ -> length (ctxCallStack ctx)

{- | Check if this context represents a subcall (nested agent invocation).

Returns True if ctxParentConversation is Just, indicating this
context is for a nested agent call.

@
if isSubcallContext ctx
    then handleSubcallBehavior ctx
    else handleRootBehavior ctx
@
-}
isSubcallContext :: ToolExecutionContext -> Bool
isSubcallContext = maybe False (const True) . ctxParentConversation
