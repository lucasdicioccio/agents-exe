# Session Management

Session management provides persistent storage and retrieval of agent conversations, enabling conversation resumption and history analysis.

## Overview

Sessions represent complete conversations between users and agents, including:

- **Session metadata**: IDs, timestamps, agent references
- **Turn history**: Complete conversation turns
- **Tool calls**: Records of tool invocations and results
- **Context**: Full conversation state for resumption
- **Parent-child relationships**: For tracking nested agent calls (new in this version)

```
┌─────────────────────────────────────────────────────────────┐
│                       Session                                │
├─────────────────────────────────────────────────────────────┤
│  SessionId: "uuid"                                          │
│  ConversationId: "uuid"                                     │
│  AgentSlug: "my-agent"                                      │
├─────────────────────────────────────────────────────────────┤
│  Parent Links (for sub-agent calls):                        │
│  - parentSessionId: Maybe SessionId                         │
│  - parentConversationId: Maybe ConversationId               │
│  - parentAgentSlug: Maybe Text                              │
├─────────────────────────────────────────────────────────────┤
│  Turns:                                                      │
│  ┌─────────────────────────────────────────────────────┐   │
│  │ Turn 1                                              │   │
│  │   User: "Hello!"                                    │   │
│  │   Assistant: "Hi there!"                            │   │
│  └─────────────────────────────────────────────────────┘   │
│  ┌─────────────────────────────────────────────────────┐   │
│  │ Turn 2                                              │   │
│  │   User: "List files"                                │   │
│  │   Tool: list_files -> ["a.txt", "b.txt"]            │   │
│  │   Assistant: "Found 2 files..."                     │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

## Nested Sessions (Sub-Agent Calls)

When an agent calls another agent as a tool, a child session is created with links to the parent. This enables:

- **Conversation hierarchy**: Track which agent called which sub-agent
- **Correlation tracing**: Link sub-agent traces to parent conversations
- **TUI tree view**: Display conversations hierarchically
- **Audit trail**: Full call chain reconstruction

### Creating Child Sessions

```haskell
import System.Agents.Session.Base

-- Create a child session linked to parent
childSess <- mkChildSession
    parentSessionId      -- Parent's session ID
    parentConversationId -- Parent's conversation ID
    "parent-agent"       -- Slug of agent initiating the call

-- Check if a session is a child session
if isChildSession childSess
    then putStrLn "This is a sub-agent session"
    else putStrLn "This is a root session"
```

### Session Type with Parent Links

```haskell
data Session = Session
    { turns :: [Turn]
    , sessionId :: SessionId
    , forkedFromSessionId :: Maybe SessionId
    , turnId :: TurnId
    -- NEW: Parent tracking fields for sub-agent calls
    , parentSessionId :: Maybe SessionId
    , parentConversationId :: Maybe ConversationId
    , parentAgentSlug :: Maybe Text
    }
```

### Sub-Agent Session Configuration

```haskell
import System.Agents.AgentTree.OneShotTool

-- Configuration for sub-agent callbacks and storage
data SubAgentSessionConfig = SubAgentSessionConfig
    { subAgentOnProgress :: Maybe OnSessionProgress
    -- ^ Optional callback for session progress events
    , subAgentStore :: Maybe SessionStore
    -- ^ Optional session store for persistence
    }

-- Default configuration (no callbacks, no storage)
defaultSubAgentConfig :: SubAgentSessionConfig
defaultSubAgentConfig = SubAgentSessionConfig Nothing Nothing

-- Create config with callbacks
let config = defaultSubAgentConfig
        { subAgentOnProgress = Just myCallback
        , subAgentStore = Just sessionStore
        }
```

### Converting Runtimes to Tools with Callbacks

```haskell
import System.Agents.AgentTree.OneShotTool

-- Legacy function (backward compatible)
turnAgentRuntimeIntoIOTool
    :: SessionStore -> Runtime -> AgentSlug -> AgentId -> ToolRegistration

-- New function with callback support
turnAgentRuntimeIntoIOToolWithCallbacks
    :: SubAgentSessionConfig
    -> Tracer IO Trace      -- Parent tracer for correlation
    -> Runtime              -- Sub-agent runtime
    -> AgentSlug            -- Parent agent slug
    -> AgentId              -- Parent agent ID
    -> ToolRegistration

-- Example usage
let toolReg = turnAgentRuntimeIntoIOToolWithCallbacks
        (SubAgentSessionConfig (Just onProgress) (Just store))
        parentTracer
        subAgentRuntime
        parentSlug
        parentAgentId
```

### Session Progress Callbacks

```haskell
data SessionProgress
    = SessionStarted Session
    | SessionUpdated Session
    | SessionCompleted Session
    | SessionFailed Session Text

type OnSessionProgress = SessionProgress -> IO ()

-- Example callback
myCallback :: OnSessionProgress
myCallback progress = case progress of
    SessionStarted sess ->
        putStrLn $ "Sub-agent started: " ++ show (sessionId sess)
    SessionUpdated sess ->
        putStrLn $ "Sub-agent progress: " ++ show (length (turns sess)) ++ " turns"
    SessionCompleted sess ->
        putStrLn $ "Sub-agent completed: " ++ show (sessionId sess)
    SessionFailed sess err ->
        putStrLn $ "Sub-agent failed: " ++ err
```

## Core Types

### Session Types (`System.Agents.Session.Types`)

```haskell
newtype SessionId = SessionId UUID
newtype TurnId = TurnId UUID

data Session = Session
    { sessionId :: SessionId
    , conversationId :: ConversationId
    , agentSlug :: AgentSlug
    , turns :: [Turn]
    -- NEW: Parent tracking fields
    , parentSessionId :: Maybe SessionId
    , parentConversationId :: Maybe ConversationId
    , parentAgentSlug :: Maybe Text
    }

data Turn = Turn
    { turnId :: TurnId
    , userMessage :: Message
    , assistantMessage :: Message
    , toolCalls :: [ToolCallRecord]
    , timestamp :: UTCTime
    }
```

### Message Types

```haskell
data Message = Message
    { role :: Role
    , content :: Text
    }

data Role = System | User | Assistant

data ToolCallRecord = ToolCallRecord
    { toolCallId :: Text
    , toolName :: Text
    , toolInput :: Value
    , toolOutput :: Either Text Value
    , toolDuration :: NominalDiffTime
    }
```

## Session Store

The `SessionStore` module provides persistent storage:

```haskell
data SessionStore = SessionStore
    { readSession :: SessionId -> IO (Maybe Session)
    , writeSession :: Session -> IO ()
    , listSessions :: IO [SessionId]
    , deleteSession :: SessionId -> IO ()
    }

-- Default file-based store
mkSessionStore :: FilePath -> SessionStore
mkSessionStore prefix = SessionStore
    { readSession = \sid -> do
        let path = sessionPath prefix sid
        decodeFileStrict' path
    , writeSession = \sess -> do
        let path = sessionPath prefix (sessionId sess)
        encodeFile path sess
    , ...
    }
```

### File Naming

```
sessions/
├── session-550e8400-e29b-41d4-a716-446655440000.json
├── session-6ba7b810-9dad-11d1-80b4-00c04fd430c8.json
└── session-6ba7b811-9dad-11d1-80b4-00c04fd430c8.json
```

## Session Lifecycle

### Creating a Session

```haskell
newSession :: AgentSlug -> IO Session
newSession slug = do
    sid <- SessionId <$> UUID.nextRandom
    cid <- ConversationId <$> UUID.nextRandom
    return Session
        { sessionId = sid
        , conversationId = cid
        , agentSlug = slug
        , turns = []
        -- NEW: Parent fields default to Nothing for root sessions
        , parentSessionId = Nothing
        , parentConversationId = Nothing
        , parentAgentSlug = Nothing
        }
```

### Creating a Child Session (Sub-Agent Call)

```haskell
import System.Agents.Session.Base

createChildSession :: SessionId -> ConversationId -> Text -> IO Session
createChildSession parentSessId parentConvId parentSlug = do
    sessId <- newSessionId
    tId <- newTurnId
    pure $ Session
        { turns = []
        , sessionId = sessId
        , forkedFromSessionId = Nothing
        , turnId = tId
        -- NEW: Link to parent
        , parentSessionId = Just parentSessId
        , parentConversationId = Just parentConvId
        , parentAgentSlug = Just parentSlug
        }
```

### Adding a Turn

```haskell
addTurn :: Session -> Message -> Message -> [ToolCallRecord] -> IO Session
addTurn session userMsg assistantMsg calls = do
    tid <- TurnId <$> UUID.nextRandom
    now <- getCurrentTime
    let turn = Turn
            { turnId = tid
            , userMessage = userMsg
            , assistantMessage = assistantMsg
            , toolCalls = calls
            , timestamp = now
            }
    return session { turns = turns session ++ [turn] }
```

### Conversation Loop

```haskell
conversationLoop :: 
    SessionStore -> 
    Runtime -> 
    Session -> 
    IO ()
conversationLoop store runtime session = do
    -- Get user input
    input <- getUserInput
    
    -- Call LLM with session context
    let messages = sessionToMessages session
    response <- callLLM runtime messages input
    
    -- Execute any tool calls
    toolResults <- executeToolCalls runtime (toolCalls response)
    
    -- Update session
    let userMsg = Message User input
    let assistantMsg = Message Assistant (responseText response)
    let callRecords = makeToolRecords toolResults
    newSession <- addTurn session userMsg assistantMsg callRecords
    
    -- Persist
    writeSession store newSession
    
    -- Continue
    conversationLoop store runtime newSession
```

## Session Serialization

### JSON Format

```json
{
  "sessionId": "550e8400-e29b-41d4-a716-446655440000",
  "conversationId": "6ba7b810-9dad-11d1-80b4-00c04fd430c8",
  "agentSlug": "file-assistant",
  "turns": [
    {
      "turnId": "6ba7b811-9dad-11d1-80b4-00c04fd430c8",
      "userMessage": {
        "role": "user",
        "content": "List all files"
      },
      "assistantMessage": {
        "role": "assistant",
        "content": "I found 3 files..."
      },
      "toolCalls": [
        {
          "toolCallId": "call_abc123",
          "toolName": "list_files",
          "toolInput": {"directory": "."},
          "toolOutput": {"right": ["a.txt", "b.txt", "c.txt"]},
          "toolDuration": 0.123
        }
      ],
      "timestamp": "2024-01-15T10:30:00Z"
    }
  ],
  "parentSessionId": null,
  "parentConversationId": null,
  "parentAgentSlug": null
}
```

### Backward Compatibility

Sessions created before the parent tracking feature will load correctly:

```haskell
instance FromJSON Session where
    parseJSON v = parseNew v <|> parseOld v
      where
        parseNew = -- Parse with parent fields
        parseOld = -- Parse old format, default parent fields to Nothing
```

## Session Operations

### Resuming a Session

```bash
# Start with existing session
agents-exe run --agent-file agent.json --session-file session-xxx.json
```

```haskell
mainOneShot :: 
    SessionStore ->
    Maybe FilePath,  -- Session file path
    Maybe Session,   -- Pre-loaded session
    Props -> 
    Text ->            -- Prompt
    IO ()
mainOneShot store mPath mSession props prompt = do
    -- Load or create session
    session <- case mSession of
        Just s -> return s
        Nothing -> case mPath of
            Just path -> fromMaybe (newSession props.agentSlug) 
                                   <$> readSessionFromFile path
            Nothing -> newSession props.agentSlug
    
    -- Run conversation
    ...
    
    -- Save session
    writeSession store updatedSession
```

### Session Printing

The `session-print` command displays session history:

```bash
# Print full session
agents-exe session-print session.json

# Print with tool call results
agents-exe session-print --show-tool-call-results session.json

# Limit to N turns
agents-exe session-print --n-turns 5 session.json

# Reverse chronological order
agents-exe session-print --antichronological session.json

# Show system prompts and tools each turn
agents-exe session-print --repeat-system-prompt --repeat-tools session.json
```

### Session Edit

Sessions can be edited for:

- Removing problematic turns
- Modifying system prompts
- Injecting context

```haskell
-- Remove last N turns
pruneTurns :: Int -> Session -> Session
pruneTurns n session = session 
    { turns = take (length (turns session) - n) (turns session) }

-- Inject context
injectContext :: Text -> Session -> Session
injectContext context session = 
    -- Add as first system message
```

## Context Window Management

Sessions track context size for LLM limits:

```haskell
estimateTokens :: Session -> Int
estimateTokens session = 
    sum [estimateMessageTokens m | turn <- turns session
                                 , m <- [userMessage turn, assistantMessage turn]]

manageContext :: Int -> Session -> Session
manageContext maxTokens session
    | estimateTokens session <= maxTokens = session
    | otherwise = manageContext maxTokens (pruneOldestTurn session)
```

## Tool Execution Context

Sessions provide context for tool execution:

```haskell
data ToolExecutionContext = ToolExecutionContext
    { ctxSessionId :: SessionId
    , ctxConversationId :: ConversationId
    , ctxTurnId :: TurnId
    , ctxFullSession :: Maybe Session
    , ...
    }
```

This allows tools to:
- Access conversation history
- Store tool-specific state
- Make context-aware decisions
- Create child sessions for sub-agent calls

## Session Store Backends

### File Store (Default)

```haskell
fileSessionStore :: FilePath -> SessionStore
fileSessionStore prefix = SessionStore
    { readSession = \sid -> 
        decodeFileStrict' (prefix ++ show sid ++ ".json")
    , writeSession = \sess ->
        encodeFile (prefix ++ show (sessionId sess) ++ ".json") sess
    , ...
    }
```

### Memory Store (Testing)

```haskell
memorySessionStore :: IO SessionStore
memorySessionStore = do
    ref <- newIORef Map.empty
    return SessionStore
        { readSession = \sid -> Map.lookup sid <$> readIORef ref
        , writeSession = \sess -> 
            modifyIORef ref (Map.insert (sessionId sess) sess)
        , ...
        }
```

## Best Practices

### Session Hygiene

1. **Regular cleanup**: Delete old sessions to save space
2. **Sensitive data**: Be careful with sessions containing secrets
3. **Backup**: Sessions are JSON files - back them up
4. **Versioning**: Handle schema migrations for old sessions

### Nested Session Design

1. **Clear parent links**: Always set parent fields for sub-agent sessions
2. **Progress callbacks**: Use callbacks to track sub-agent lifecycle
3. **Trace correlation**: Pass parent tracer for correlation IDs
4. **Tree visualization**: Use parent links for hierarchical display

### Conversation Design

1. **Clear turn boundaries**: Each user input = one turn
2. **Tool call recording**: Always record for reproducibility
3. **Error capture**: Record tool failures in the session
4. **Timestamps**: Useful for debugging and auditing

### Performance

1. **Lazy loading**: Don't load full history unless needed
2. **Pagination**: For long sessions, load turns in chunks
3. **Compression**: Consider gzip for large session files

## Session Print Options

```haskell
data SessionPrintOptions = SessionPrintOptions
    { sessionFile :: FilePath
    , showToolCallResults :: Bool
    , nTurns :: Maybe Int           -- Limit number of turns
    , repeatSystemPrompt :: Bool    -- Show system prompt each turn
    , repeatTools :: Bool           -- Show available tools each turn
    , order :: PrintOrder
    }

data PrintOrder = Chronological | Antichronological
```

## Example: Session Analysis

```haskell
-- Count tool usage
toolUsageStats :: Session -> Map Text Int
toolUsageStats session = 
    Map.fromListWith (+) 
        [(toolName tc, 1) | turn <- turns session
                          , tc <- toolCalls turn]

-- Average response time
averageResponseTime :: Session -> NominalDiffTime
averageResponseTime session =
    sum [toolDuration tc | turn <- turns session
                         , tc <- toolCalls turn] 
    / fromIntegral (length $ concatMap toolCalls $ turns session)

-- Find turns with errors
errorTurns :: Session -> [Turn]
errorTurns session =
    [turn | turn <- turns session
          , any (isLeft . toolOutput) (toolCalls turn)]

-- Get conversation hierarchy
getRootConversationId :: Session -> Maybe ConversationId
getRootConversationId session =
    case parentConversationId session of
        Nothing -> Just (conversationId session)  -- Root
        Just parentId -> Just parentId            -- Return parent
```

