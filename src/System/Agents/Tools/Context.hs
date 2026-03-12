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
-}
module System.Agents.Tools.Context (
    -- * Core types
    ToolExecutionContext (..),
    CallStackEntry (..),
    RecursionError (..),

    -- * Construction
    mkToolExecutionContext,
    mkMinimalContext,
    mkRootContext,
    pushAgentContext,

    -- * Access helpers
    hasFullSession,
    hasAgentId,

    -- * Recursion tracking helpers
    currentRecursionDepth,
    isAtDepth,
    callChain,
    isAgentInCallStack,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import System.Agents.Base (AgentId, ConversationId)
import System.Agents.Session.Types (Session, SessionId, TurnId)

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
    }
    deriving (Show, Eq, Generic)

{- | JSON serialization support for 'ToolExecutionContext'.
Enables persistence and transmission of context across boundaries.
-}
instance ToJSON ToolExecutionContext

instance FromJSON ToolExecutionContext

-------------------------------------------------------------------------------
-- Construction Helpers
-------------------------------------------------------------------------------

{- | Create a complete 'ToolExecutionContext' with all fields populated.

This is the preferred constructor when you have all context information
available and want to provide tools with full session access.

Example:

@
context <- mkToolExecutionContext
    sessionId
    conversationId
    turnId
    (Just agentId)
    (Just fullSession)
    [CallStackEntry "root" conversationId 0]
    Nothing
@
-}
mkToolExecutionContext ::
    SessionId ->
    ConversationId ->
    TurnId ->
    Maybe AgentId ->
    Maybe Session ->
    [CallStackEntry] ->
    Maybe Int ->
    ToolExecutionContext
mkToolExecutionContext = ToolExecutionContext

{- | Create a minimal 'ToolExecutionContext' with only required identifiers.

Use this when you don't have (or don't need) agent identification or
full session data. This is appropriate for simple tools that only need
to log or track their execution context.

Note: This creates a root-level context with an empty call stack.
For proper recursion tracking, use 'mkRootContext' instead.

Example:

@
context <- mkMinimalContext sessionId conversationId turnId
@
-}
mkMinimalContext ::
    SessionId ->
    ConversationId ->
    TurnId ->
    ToolExecutionContext
mkMinimalContext sessId convId tId =
    ToolExecutionContext
        { ctxSessionId = sessId
        , ctxConversationId = convId
        , ctxTurnId = tId
        , ctxAgentId = Nothing
        , ctxFullSession = Nothing
        , ctxCallStack = []
        , ctxMaxDepth = Nothing
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
    (Just 5)  -- max depth of 5
@
-}
mkRootContext ::
    SessionId ->
    ConversationId ->
    TurnId ->
    Maybe AgentId ->
    Maybe Session ->
    -- | Optional maximum recursion depth limit
    Maybe Int ->
    ToolExecutionContext
mkRootContext sessId convId tId mAgentId mSession maxDepth =
    ToolExecutionContext
        { ctxSessionId = sessId
        , ctxConversationId = convId
        , ctxTurnId = tId
        , ctxAgentId = mAgentId
        , ctxFullSession = mSession
        , ctxCallStack = [CallStackEntry "root" convId 0]
        , ctxMaxDepth = maxDepth
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
