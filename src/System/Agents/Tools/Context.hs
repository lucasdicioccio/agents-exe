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
) where

import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time (NominalDiffTime)
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
    , callCallerId :: Text
    -- ^ Identifier for the calling tool (for tracing: who called whom)
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

Note: The portal is synchronous (IO ToolResult) for simplicity.
Async support can be added later if needed.
-}
type ToolPortal = ToolCall -> IO ToolResult

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
    , ctxToolPortal :: Maybe ToolPortal
    {- ^ Optional tool portal for inter-toolbox communication. When present,
    tools can use this callback to invoke other tools. This enables Lua
    scripts and other tools to orchestrate multiple tool calls.
    -}
    , ctxAllowedTools :: [Text]
    {- ^ Whitelist of tool names allowed in this context. An empty list
    means all tools are allowed (for backward compatibility). This is
    checked before invoking tools through the portal.
    -}
    }
    deriving (Generic)

-- | Custom Eq instance for ToolExecutionContext that handles the function field
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

-- Note: ctxToolPortal is not compared (functions can't be compared)

-- | Custom Show instance for ToolExecutionContext that handles the function field
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
            ++ " }"
      where
        portalStr = case ctxToolPortal ctx of
            Nothing -> "Nothing"
            Just _ -> "Just <portal>"

{- | JSON serialization support for 'ToolExecutionContext'.
Note: The tool portal function is not serialized (functions can't be serialized).
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
            -- Note: ctxToolPortal is intentionally omitted (not serializable)
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
            <*> pure Nothing -- ToolPortal can't be deserialized
            <*> v .: "allowedTools"

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
mkToolExecutionContext sessId convId tId mAgentId mSession stack maxDepth =
    ToolExecutionContext
        { ctxSessionId = sessId
        , ctxConversationId = convId
        , ctxTurnId = tId
        , ctxAgentId = mAgentId
        , ctxFullSession = mSession
        , ctxCallStack = stack
        , ctxMaxDepth = maxDepth
        , ctxToolPortal = Nothing
        , ctxAllowedTools = []
        }

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
        , ctxToolPortal = Nothing
        , ctxAllowedTools = []
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
        , ctxToolPortal = Nothing
        , ctxAllowedTools = []
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
    (Just toolPortal)  -- the portal function
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
    Maybe ToolPortal ->
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
