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

# Remove thinking content
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

## Session Search (`System.Agents.Session.Search`)

The Session Search subsystem provides fast fuzzy text search across session files using SQLite FTS5. It enables searching through conversation history with trigram-based fuzzy matching and metadata filtering.

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Session Search                            │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │    Types    │  │    Index    │  │    Query    │         │
│  │  (config)   │  │  (SQLite)   │  │  (search)   │         │
│  └─────────────┘  └─────────────┘  └─────────────┘         │
│        ▲                ▲                ▲                  │
│        └────────────────┴────────────────┘                  │
│                    CLI Handlers                              │
│              (session-index, session-search)                 │
└─────────────────────────────────────────────────────────────┘
```

### Core Types (`System.Agents.Session.Search.Types`)

```haskell
-- | Configuration for the search index.
data SearchIndexConfig = SearchIndexConfig
    { indexDbPath :: FilePath
    -- ^ Path to the SQLite index database (default: .agents-search.db)
    , indexSessionStore :: SessionStore
    -- ^ Session store to index
    , indexIncludeToolOutputs :: Bool
    -- ^ Whether to include tool outputs in the index
    }

-- | Options for session search queries.
data SearchOptions = SearchOptions
    { searchQuery :: Text
    -- ^ Fuzzy search query text
    , searchDateFilter :: Maybe DateFilter
    -- ^ Optional date filter
    , searchTools :: [Text]
    -- ^ Filter by tools used (any of these)
    , searchAgent :: Maybe Text
    -- ^ Filter by agent slug
    , searchIncludeToolOutputs :: Bool
    -- ^ Include tool outputs in search
    , searchJsonOutput :: Bool
    -- ^ Output results as JSON
    , searchPreviewLines :: Int
    -- ^ Number of context lines to show
    , searchLimit :: Maybe Int
    -- ^ Maximum number of results
    , searchAutoUpdate :: Bool
    -- ^ Auto-update index if stale before searching
    }

-- | Date filter for search queries.
data DateFilter
    = AfterDate UTCTime
    | BeforeDate UTCTime
    | BetweenDates UTCTime UTCTime

-- | Status of the search index.
data SearchIndexStatus
    = IndexCurrent
    | IndexStale Int Int  -- (stale sessions, total sessions)
    | IndexMissing
    | IndexError Text
```

### Search Result Types

```haskell
-- | Complete search results.
data SearchResult = SearchResult
    { resultItems :: [SearchResultItem]
    , resultTotalMatches :: Int
    , resultQueryTimeMs :: Double
    , resultIndexWasUpdated :: Bool
    }

-- | A single search result item.
data SearchResultItem = SearchResultItem
    { resultMetadata :: SearchResultMetadata
    , resultPreview :: Maybe Text
    , resultMatchedTerms :: [Text]
    }

-- | Metadata about a search result.
data SearchResultMetadata = SearchResultMetadata
    { resultSessionId :: Text
    , resultFilePath :: FilePath
    , resultAgentSlug :: Maybe Text
    , resultTurnCount :: Int
    , resultFirstTurnAt :: Maybe UTCTime
    , resultLastTurnAt :: Maybe UTCTime
    , resultRank :: Double
    }
```

### Index Schema (`System.Agents.Session.Search.Index`)

The search index uses SQLite with FTS5 (Full-Text Search version 5):

```sql
-- Session metadata cache
CREATE TABLE session_index (
    session_id TEXT PRIMARY KEY,
    file_path TEXT NOT NULL,
    mtime INTEGER NOT NULL,
    agent_slug TEXT,
    turn_count INTEGER,
    first_turn_at TIMESTAMP,
    last_turn_at TIMESTAMP
);

-- FTS5 virtual table for trigram search
CREATE VIRTUAL TABLE search_content USING fts5(
    session_id,
    content,
    tokenize='trigram'
);

-- Tool call index for filtering
CREATE TABLE tool_index (
    session_id TEXT REFERENCES session_index(session_id),
    tool_name TEXT NOT NULL,
    call_count INTEGER DEFAULT 1,
    PRIMARY KEY (session_id, tool_name)
);

-- Index metadata
CREATE TABLE index_metadata (
    key TEXT PRIMARY KEY,
    value TEXT
);
```

### Index Operations

```haskell
-- | Create a new search index, replacing any existing index.
createSearchIndex :: SearchIndexConfig -> IO ()

-- | Incrementally update the search index.
updateSearchIndex :: SearchIndexConfig -> IO ()

-- | Check the status of the search index.
checkIndexStatus :: SearchIndexConfig -> IO SearchIndexStatus

-- | Remove the search index.
removeSearchIndex :: SearchIndexConfig -> IO ()
```

### Search Execution (`System.Agents.Session.Search.Query`)

```haskell
-- | Execute a search query with the given options.
executeSearchWithOptions :: SearchIndexConfig -> SearchOptions -> IO SearchResult

-- | Execute a search query against the index.
executeSearch :: SearchIndexConfig -> SearchOptions -> IO SearchResult

-- | Format search results for display.
formatResults :: SearchOptions -> SearchResult -> IO ()
```

### Fuzzy Matching

The search uses SQLite FTS5 with trigram tokenization:

- **Trigram tokenization**: Breaks text into 3-character sequences
- **Fuzzy matching**: "error" matches "errors", "erroring", "terror"
- **Prefix matching**: "data" matches "database", "datagram"
- **Typo tolerance**: Small changes in query still find matches

Example:
```sql
-- Find sessions with fuzzy match to "error"
SELECT session_id, rank 
FROM search_content 
WHERE content MATCH 'error'
ORDER BY rank;
```

### CLI: session-index Command

```bash
# Build the search index
agents-exe session-index --build

# Check index status
agents-exe session-index --status

# Update index incrementally
agents-exe session-index --update

# Remove the index
agents-exe session-index --clean

# Build with tool outputs included
agents-exe session-index --build --include-tool-outputs
```

### CLI: session-search Command

```bash
# Basic fuzzy search
agents-exe session-search "database error"

# Search with auto-update
agents-exe session-search "migration" --auto

# Include tool outputs in search
agents-exe session-search "config.yaml" --include-tool-outputs

# Filter by date and tool
agents-exe session-search "auth" --after 2024-01-01 --tool write-file

# Filter by agent
agents-exe session-search "refactor" --agent my-coder

# JSON output for scripting
agents-exe session-search "TODO" --json

# Show context lines
agents-exe session-search "deploy" --preview 5

# Combined filters
agents-exe session-search "fix" \
  --after 2024-01-01 \
  --before 2024-12-31 \
  --tool bash_write-file \
  --json
```

### Search Workflow Example

```bash
# 1. Build the initial index
agents-exe session-index --build

# 2. Search for sessions about errors
agents-exe session-search "error" --json | jq '.resultItems[] | .resultMetadata.resultFilePath'

# 3. Find sessions using specific tools
agents-exe session-search "database" --tool write-file --tool read-file

# 4. Update index after new sessions
agents-exe session-index --update

# 5. Search with date filter
agents-exe session-search "config" --after 2024-06-01 --preview 3
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
4. **Search indexing**: Build index periodically for fast searching

### Search Best Practices

1. **Regular index updates**: Run `session-index --update` periodically
2. **Include tool outputs**: Use `--include-tool-outputs` for comprehensive search
3. **Use filters**: Combine text search with `--tool`, `--agent`, or date filters
4. **JSON output**: Use `--json` for scripting and automation
5. **Auto-update**: Use `--auto` flag to ensure fresh results

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

## Example: Search Integration

```haskell
import System.Agents.Session.Search.Types
import System.Agents.Session.Search.Index
import System.Agents.Session.Search.Query

-- Search for sessions with specific patterns
searchSessions :: Text -> IO SearchResult
searchSessions query = do
    let config = defaultSearchIndexConfig defaultSessionStore
    let opts = (defaultSearchOptions query)
            { searchJsonOutput = False
            , searchPreviewLines = 3
            , searchLimit = Just 20
            }
    executeSearchWithOptions config opts

-- Find sessions that used specific tools
findToolUsage :: [Text] -> IO SearchResult
findToolUsage tools = do
    let config = defaultSearchIndexConfig defaultSessionStore
    let opts = (defaultSearchOptions "")
            { searchTools = tools
            , searchJsonOutput = True
            }
    executeSearchWithOptions config opts
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
| `System.Agents.Session.Search.Types` | Search configuration types |
| `System.Agents.Session.Search.Index` | Index building and maintenance |
| `System.Agents.Session.Search.Query` | Search query execution |
| `System.Agents.SessionStore` | Persistent storage |
| `System.Agents.SessionPrint` | Markdown printing and statistics |
| `System.Agents.SessionPrint.Inject` | Session content injection |

