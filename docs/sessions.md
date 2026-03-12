# Session Management

Session management provides persistent storage and retrieval of agent conversations, enabling conversation resumption and history analysis.

## Overview

Sessions represent complete conversations between users and agents, including:

- **Session metadata**: IDs, timestamps, agent references
- **Turn history**: Complete conversation turns
- **Tool calls**: Records of tool invocations and results
- **Context**: Full conversation state for resumption

```
┌─────────────────────────────────────────────────────────────┐
│                       Session                                │
├─────────────────────────────────────────────────────────────┤
│  SessionId: "uuid"                                          │
│  ConversationId: "uuid"                                     │
│  AgentSlug: "my-agent"                                      │
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
  ]
}
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
    Maybe FilePath ->  -- Session file path
    Maybe Session ->   -- Pre-loaded session
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
```

