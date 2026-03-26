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

## Session Printing (`System.Agents.SessionPrint`)

The `SessionPrint` module provides rich markdown formatting for session files, including statistics visualization and configurable content display.

### Session Print Types

```haskell
-- | Preference for ordering session steps.
data OrderPreference
    = Chronological      -- Oldest first
    | Antichronological  -- Newest first

-- | Amount of content to print (lines or characters).
data PrintAmount
    = Lines Int
    | Chars Int

-- | Visibility preference for displaying content.
data PrintVisibility
    = Hidden                    -- Don't show content
    | Elided PrintAmount PrintAmount  -- Show leading/trailing, elide middle
    | ShownFull                 -- Show complete content

-- | Options for controlling session print output.
data SessionPrintOptions = SessionPrintOptions
    { sessionPrintFile :: FilePath
    , showToolCallResults :: PrintVisibility
    , showToolCallArguments :: PrintVisibility
    , nTurns :: Maybe Int
    , repeatSystemPrompt :: Bool
    , repeatTools :: Bool
    , orderPreference :: OrderPreference
    , noFunnyStamp :: Bool
    }

-- | Statistics about a session.
data SessionStatistics = SessionStatistics
    { statTotalTurns :: Int
    , statUserTurns :: Int
    , statLlmTurns :: Int
    , statTotalToolCalls :: Int
    , statToolCallsByName :: Map Text Int
    , statInputBytes :: Int
    , statOutputBytes :: Int
    , statReasoningBytes :: Int
    , statTotalBytes :: Int
    }
```

### CLI: session-print Command

```bash
# Print full session
agents-exe session-print session.json

# Print with tool call results visible
agents-exe session-print --show-tool-call-results shown session.json

# Show tool call arguments too
agents-exe session-print \
    --show-tool-call-results shown \
    --show-tool-call-arguments shown \
    session.json

# Elide long outputs (show first/last 10 lines)
agents-exe session-print --show-tool-call-results elided session.json

# Limit to N turns
agents-exe session-print --n-turns 5 session.json

# Reverse chronological order
agents-exe session-print --antichronological session.json

# Show system prompts and tools each turn
agents-exe session-print --repeat-system-prompt --repeat-tools session.json

# Skip the ASCII art logo
agents-exe session-print --no-funny-stamp session.json
```

### Content Elision

The `elideDocument` function intelligently handles content that's too long:

```haskell
-- | Elide a document by keeping leading and trailing portions.
elideDocument :: PrintAmount -> PrintAmount -> Text -> Text

-- Examples:
elideDocument (Lines 3) (Lines 3) "line1\nline2\n...\nline7"
-- Shows all 7 lines (no overlap)

elideDocument (Lines 2) (Lines 2) "line1\nline2\nline3\nline4\nline5"
-- "line1\nline2\n... (1 line elided) ...\nline4\nline5"
```

### Statistics Visualization

Session print includes visual bar charts for:

1. **Tool usage**: Bar chart showing which tools were called most
2. **Byte usage**: Input, output, and reasoning token breakdown

```
📊 Statistics

### 🔧 Tool Calls

Total Tool Calls: 15

`read-file`         8   ████████████████████████████████████████
`write-file`        4   ████████████████████
`grep-files`        3   ███████████████

### 💾 Byte Usage

`Input    `      2 KiB   ████████████████████████
`Output   `      5 KiB   ████████████████████████████████████████████████
`Reasoning`      1 KiB   ████████████

Total: 8 KiB
```

## Session Content Injection (`System.Agents.SessionPrint.Inject`)

The `SessionInject` module allows injecting session content into prompts with various verbosity levels.

### Injection Verbosity Levels

```haskell
data SessionInjectMode
    = SessionXS   -- Minimal: queries/responses only, skips tool-only turns
    | SessionS    -- Low: +thinking, +tool names
    | SessionM    -- Medium: +statistics
    | SessionL    -- High: +tool call results
    | SessionXL   -- Maximum: complete session
```

### CLI: Session Injection Options

```bash
# Inject session at minimal verbosity
agents-exe run --session-xs previous-session.json --prompt "Continue..."

# Inject at low verbosity
agents-exe run --session-s previous-session.json --prompt "Continue..."

# Inject at medium verbosity
agents-exe run --session-m previous-session.json --prompt "Continue..."

# Inject at high verbosity (includes tool results)
agents-exe run --session-l previous-session.json --prompt "Continue..."

# Maximum verbosity
agents-exe run --session-xl previous-session.json --prompt "Continue..."
```

### Programmatic Usage

```haskell
import System.Agents.SessionPrint.Inject

-- Load session content at specific verbosity
loadSessionForPrompt :: SessionInjectMode -> Session -> Text
loadSessionForPrompt mode session = case mode of
    SessionXS -> formatMinimal session   -- Just user queries and LLM responses
    SessionS  -> formatLow session       -- + thinking process
    SessionM  -> formatMedium session    -- + statistics
    SessionL  -> formatHigh session      -- + tool call results
    SessionXL -> formatComplete session  -- Everything
```

## Session Edit

The `SessionEdit` module provides operations for modifying session files.

### CLI: session-edit Command

```bash
# Take first N turns
agents-exe session-edit --take --count 10 session.json < input.json > output.json

# Take last N turns
agents-exe session-edit --take-tail --count 5 session.json < input.json > output.json

# Drop first N turns
agents-exe session-edit --drop --count 2 session.json < input.json > output.json

# Drop last N turns
agents-exe session-edit --drop-tail --count 1 session.json < input.json > output.json

# Remove all tool calls
agents-exe session-edit --censor-tool-calls session.json < input.json > output.json

# Remove all thinking content
agents-exe session-edit --censor-thinking session.json < input.json > output.json
```

### Edit Operations

```haskell
data SessionEditOp
    = SessionEditTake Int       -- Take first N turns
    | SessionEditTakeTail Int   -- Take last N turns
    | SessionEditDrop Int       -- Drop first N turns
    | SessionEditDropTail Int   -- Drop last N turns
    | SessionEditCensorToolCalls  -- Remove tool calls
    | SessionEditCensorThinking   -- Remove thinking content

applyEdit :: SessionEditOp -> Session -> Session
applyEdit (SessionEditTake n) session = 
    session { turns = take n (turns session) }
applyEdit (SessionEditDrop n) session = 
    session { turns = drop n (turns session) }
applyEdit SessionEditCensorToolCalls session =
    session { turns = map removeToolCalls (turns session) }
-- etc.
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

## Related Modules

| Module | Purpose |
|--------|---------|
| `System.Agents.Session.Types` | Core session types |
| `System.Agents.Session.Base` | Session operations |
| `System.Agents.Session.Loop` | Conversation loop |
| `System.Agents.Session.Step` | Single turn execution |
| `System.Agents.Session.Edit` | Session editing operations |
| `System.Agents.Session.OpenAI` | OpenAI-specific session handling |
| `System.Agents.SessionStore` | Persistent storage |
| `System.Agents.SessionPrint` | Markdown printing and statistics |
| `System.Agents.SessionPrint.Inject` | Session content injection |

