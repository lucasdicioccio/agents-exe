# Terminal UI (TUI)

The Terminal UI provides an interactive, real-time interface for agent conversations with support for multiple agents, streaming responses, visual feedback, and **conversation tree navigation** for nested agent calls.

## Overview

```
┌─────────────────────────────────────────────────────────────────┐
│ Agents TUI                                          [agents: 3] │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  🤖 file-assistant                                              │
│  ─────────────────────────────────────────────────────────────  │
│  > List all Python files in the current directory               │
│                                                                 │
│  I'll help you list all Python files.                           │
│                                                                 │
│  [Calling tool: list_files]                                     │
│  ✓ list_files completed (0.2s)                                  │
│                                                                 │
│  Found 3 Python files:                                          │
│  - main.py                                                      │
│  - utils.py                                                     │
│  - test_main.py                                                 │
│                                                                 │
│  ─────────────────────────────────────────────────────────────  │
│  > _                                                              │
│                                                                 │
├─────────────────────────────────────────────────────────────────┤
│ [Tab] Switch  [Enter] Send  [Ctrl+C] Quit  [Ctrl+[r|t]] View MD │
└─────────────────────────────────────────────────────────────────┘
```

## Conversation Tree Navigation

The TUI now supports hierarchical display of conversations when agents call sub-agents. Conversations are displayed in a tree structure where:

- Root conversations appear at the top level
- Sub-agent conversations appear as children of their parent
- Expand/collapse functionality allows focusing on specific branches

```
Conversation Tree Example:
┌─────────────────────────────────┐
│ 📁 main-agent (expanded)        │
│   └── 📁 helper-1 (collapsed)   │
│   └── 📄 helper-2               │
│ 📄 standalone-agent             │
└─────────────────────────────────┘
```

### Tree Navigation Keys

When focused on the conversation list:

| Key | Action |
|-----|--------|
| `Tab` | Toggle expansion of selected conversation |
| `→` (Right Arrow) | Expand selected conversation |
| `←` (Left Arrow) | Collapse selected conversation |
| `↑` (Up Arrow) | Navigate to previous conversation in tree |
| `↓` (Down Arrow) | Navigate to next conversation in tree |

### Sub-Agent Session Events

The TUI receives events for sub-agent lifecycle:

```haskell
data AppEvent
    = ...
    | AppEvent_SubAgentSessionStarted ConversationId Session
    | AppEvent_SubAgentSessionUpdated ConversationId Session
    | AppEvent_SubAgentSessionCompleted ConversationId Session
```

These events:
1. **Started**: Auto-expand parent to show the new sub-agent
2. **Updated**: Refresh the conversation display
3. **Completed**: Show status message, refresh display

## Architecture

### Component Structure

```
┌─────────────────────────────────────────────────────────────────┐
│                         TUI.Core                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐      │
│  │   TUI.Types  │───>│  TUI.Render  │───>│  TUI.Event   │      │
│  │   (state)    │    │  (display)   │    │  (input)     │      │
│  └──────────────┘    └──────────────┘    └──────────────┘      │
│         ▲                                            │          │
│         └────────────────────────────────────────────┘          │
│                      (event loop)                               │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### State Management

```haskell
-- TUI.Types
data TUIState = TUIState
    { agents :: [AgentState]
    , currentAgentIndex :: Int
    , inputBuffer :: Text
    , messages :: [Message]
    , scrollOffset :: Int
    , status :: Status
    -- NEW: Conversation tree state
    , conversationTreeExpanded :: Set ConversationId
    , selectedConversationPath :: [ConversationId]
    }

data AgentState = AgentState
    { agentProps :: AgentTree.Props
    , agentRuntime :: Maybe Runtime
    , conversation :: [ConversationItem]
    , isStreaming :: Bool
    }

data ConversationItem
    = UserInput Text
    | AgentOutput Text
    | ToolCallStart Text
    | ToolCallEnd Text Value
    | SystemMessage Text
    | SubAgentStarted Text       -- NEW: Sub-agent call started
    | SubAgentCompleted Text     -- NEW: Sub-agent call completed
```

### Conversation Tree Types

```haskell
-- A node in the conversation tree
data ConversationNode = ConversationNode
    { nodeConversation :: Conversation
    , nodeChildren :: [ConversationNode]
    , nodeExpanded :: Bool
    }

-- Build tree from flat conversation list
buildConversationTree :: [Conversation] -> [ConversationNode]

-- Helper for creating sub-agent callbacks
makeSubAgentCallback :: BChan AppEvent -> ConversationId -> OnSessionProgress
makeSubAgentCallback chan parentConvId progress =
    case progress of
        SessionStarted sess ->
            writeBChan chan (AppEvent_SubAgentSessionStarted parentConvId sess)
        SessionUpdated sess ->
            writeBChan chan (AppEvent_SubAgentSessionUpdated parentConvId sess)
        SessionCompleted sess ->
            writeBChan chan (AppEvent_SubAgentSessionCompleted parentConvId sess)
```

## Rendering

### Screen Layout

```haskell
-- TUI.Render
render :: TUIState -> Widget ()
render state = 
    vBox
        [ renderHeader state
        , renderAgentTabs state
        , renderConversationTree state  -- Updated for tree view
        , renderInput state
        , renderFooter state
        ]

renderHeader :: TUIState -> Widget ()
renderHeader state = 
    withAttr headerAttr $ 
        hBox
            [ str " Agents TUI "
            , fill ' '
            , str $ " [agents: " ++ show (length $ agents state) ++ "]"
            ]

renderAgentTabs :: TUIState -> Widget ()
renderAgentTabs state = 
    hBox $ intersperse (str " | ") $
        zipWith renderTab [0..] (agents state)
  where
    renderTab idx agent =
        if idx == currentAgentIndex state
            then withAttr activeTabAttr $ str $ agentName agent
            else str $ agentName agent
```

### Conversation Tree Rendering

```haskell
renderConversationTree :: TUIState -> Widget ()
renderConversationTree state =
    viewport ScrollVertical $ 
        vBox $ renderTreeNodes 0 (conversationTree state)

renderTreeNode :: Int -> ConversationNode -> Widget ()
renderTreeNode depth node =
    let indent = replicate (depth * 2) ' '
        expander = if nodeExpanded node then "▼ " else "▶ "
        prefix = if null (nodeChildren node) then "  " else expander
    in
    withAttr (conversationAttr node) $
        str indent <+> str prefix <+> renderConversation (nodeConversation node)
```

### Message Rendering

```haskell
renderConversation :: TUIState -> Widget ()
renderConversation state =
    viewport ScrollVertical $ 
        vBox $ map renderItem $ reverse $ take 50 $ conversation currentAgent
  where
    currentAgent = agents state !! currentAgentIndex state
    
    renderItem :: ConversationItem -> Widget ()
    renderItem (UserInput text) =
        withAttr userAttr $ str "> " <+> txtWrap text
    renderItem (AgentOutput text) =
        withAttr agentAttr $ txtWrap text
    renderItem (ToolCallStart name) =
        withAttr toolAttr $ str $ "[Calling tool: " ++ unpack name ++ "]"
    renderItem (ToolCallEnd name result) =
        withAttr toolSuccessAttr $ str $ "✓ " ++ unpack name ++ " completed"
    renderItem (SubAgentStarted slug) =
        withAttr subAgentAttr $ str $ "[Sub-agent started: " ++ unpack slug ++ "]"
    renderItem (SubAgentCompleted slug) =
        withAttr subAgentAttr $ str $ "[Sub-agent completed: " ++ unpack slug ++ "]"
```

## Event Handling

### Input Processing

```haskell
-- TUI.Event
handleEvent :: TUIState -> BrickEvent () CustomEvent -> EventM () (Next TUIState)
handleEvent state (VtyEvent ev) = case ev of
    -- Quit
    EvKey (KChar 'c') [MCtrl] -> halt state
    
    -- Switch agents
    EvKey KTab [] -> continue $ nextAgent state
    EvKey KBackTab [] -> continue $ prevAgent state
    
    -- Tree navigation
    EvKey KRight [] -> continue $ expandSelected state
    EvKey KLeft [] -> continue $ collapseSelected state
    
    -- Scroll
    EvKey KUp [] -> continue $ scrollUp state
    EvKey KDown [] -> continue $ scrollDown state
    EvKey KPageUp [] -> continue $ scrollPageUp state
    EvKey KPageDown [] -> continue $ scrollPageDown state
    
    -- Send message
    EvKey KEnter [] -> do
        let text = inputBuffer state
        if T.null text
            then continue state
            else do
                liftIO $ sendToAgent state text
                continue $ clearInput $ addUserMessage state text
    
    -- Character input
    EvKey (KChar c) [] -> 
        continue $ state { inputBuffer = inputBuffer state <> T.singleton c }
    EvKey KBS [] -> 
        continue $ state { inputBuffer = T.init (inputBuffer state) }
    
    _ -> continue state
```

### Tree Navigation Event Handlers

```haskell
-- Toggle expansion of selected conversation
toggleSelectedConversation :: EventM N TuiState ()
toggleSelectedConversation = do
    mSelectedId <- getSelectedConversationId
    case mSelectedId of
        Nothing -> pure ()
        Just convId -> do
            expanded <- use (tuiUI . conversationTreeExpanded)
            if Set.member convId expanded
                then tuiUI . conversationTreeExpanded .= Set.delete convId expanded
                else tuiUI . conversationTreeExpanded .= Set.insert convId expanded

-- Navigate to previous conversation in tree
navigateConversationUp :: EventM N TuiState ()
navigateConversationUp = do
    visibleConvIds <- getVisibleConversationsInOrder
    currentPath <- use (tuiUI . selectedConversationPath)
    case currentPath of
        [] -> pure ()
        (currentId : _) ->
            case findIndex (== currentId) visibleConvIds of
                Nothing -> pure ()
                Just 0 -> pure ()
                Just idx -> do
                    let prevId = visibleConvIds !! (idx - 1)
                    tuiUI . selectedConversationPath .= [prevId]
                    updateListSelectionToConversation prevId
```

### Custom Events

```haskell
data CustomEvent
    = AgentResponse AgentIndex Text
    | ToolStarted AgentIndex Text
    | ToolCompleted AgentIndex Text Value
    | AgentError AgentIndex Text
    | StreamChunk AgentIndex Text
    | StreamComplete AgentIndex
    -- NEW: Sub-agent events
    | SubAgentSessionStarted ConversationId Session
    | SubAgentSessionUpdated ConversationId Session
    | SubAgentSessionCompleted ConversationId Session

eventChannel :: BChan CustomEvent
eventChannel = newBChan 100
```

### Sub-Agent Event Handlers

```haskell
-- Handle sub-agent session started
handleSubAgentSessionStarted :: ConversationId -> Session -> EventM N TuiState ()
handleSubAgentSessionStarted parentConvId childSession = do
    showStatus StatusInfo $ "Sub-agent started under conversation " <> Text.pack (show parentConvId)
    -- Auto-expand the parent conversation so the child is visible
    tuiUI . conversationTreeExpanded %= Set.insert parentConvId
    -- Refresh the conversation list to show the new session
    handleHeartbeat

-- Handle sub-agent session updated
handleSubAgentSessionUpdated :: ConversationId -> Session -> EventM N TuiState ()
handleSubAgentSessionUpdated _parentConvId _childSession = do
    -- The session is stored in core, just refresh to show updates
    handleHeartbeat

-- Handle sub-agent session completed
handleSubAgentSessionCompleted :: ConversationId -> Session -> EventM N TuiState ()
handleSubAgentSessionCompleted parentConvId _childSession = do
    showStatus StatusInfo $ "Sub-agent completed under conversation " <> Text.pack (show parentConvId)
    handleHeartbeat
```

## Streaming Responses

### Real-time Updates

```haskell
streamAgentResponse :: 
    AgentState -> 
    Text -> 
    BChan CustomEvent -> 
    IO ()
streamAgentResponse agent prompt chan = do
    let runtime = fromJust $ agentRuntime agent
    
    -- Start streaming
    writeBChan chan $ ToolStarted idx "llm-call"
    
    -- Stream chunks
    streamLLM runtime prompt $ \chunk -> do
        writeBChan chan $ StreamChunk idx chunk
    
    -- Complete
    writeBChan chan $ StreamComplete idx
  where
    idx = agentIndex agent
```

### Handling Stream Chunks

```haskell
handleCustomEvent :: TUIState -> CustomEvent -> EventM () (Next TUIState)
handleCustomEvent state (StreamChunk idx chunk) =
    continue $ updateAgent idx (appendToCurrentResponse chunk) state

handleCustomEvent state (StreamComplete idx) =
    continue $ updateAgent idx (finalizeResponse >> setStreaming False) state

handleCustomEvent state (ToolStarted idx name) =
    continue $ updateAgent idx (addConversationItem $ ToolCallStart name) state

handleCustomEvent state (ToolCompleted idx name result) =
    continue $ updateAgent idx (addConversationItem $ ToolCallEnd name result) state

-- NEW: Handle sub-agent events
handleCustomEvent state (SubAgentSessionStarted parentId childSess) =
    continue $ addSubAgentConversation parentId childSess state

handleCustomEvent state (SubAgentSessionCompleted _parentId childSess) =
    continue $ markSubAgentCompleted (sessionId childSess) state
```

## Multi-Agent Support

### Agent Selection

```haskell
nextAgent :: TUIState -> TUIState
nextAgent state = state 
    { currentAgentIndex = (currentAgentIndex state + 1) `mod` length (agents state) }

prevAgent :: TUIState -> TUIState
prevAgent state = state
    { currentAgentIndex = (currentAgentIndex state - 1) `mod` length (agents state) }

-- Send to specific agent
sendToAgent :: TUIState -> Text -> IO ()
sendToAgent state text = do
    let agent = agents state !! currentAgentIndex state
    streamAgentResponse agent text eventChannel
```

### Agent Initialization with Sub-Agent Support

```haskell
initializeAgents :: [AgentTree.Props] -> IO [AgentState]
initializeAgents propsList = do
    forM propsList $ \props -> do
        result <- AgentTree.withAgentTreeRuntime props $ \case
            AgentTree.Initialized tree -> do
                runtime <- AgentTree.getMainRuntime tree
                return $ Right AgentState
                    { agentProps = props
                    , agentRuntime = Just runtime
                    , conversation = []
                    , isStreaming = False
                    }
            AgentTree.Errors errs -> return $ Left (show errs)
        case result of
            Right state -> return state
            Left err -> error err
```

## Session Export and Viewing

The TUI supports exporting and viewing session content in markdown format.

### Export to Markdown

Press `Ctrl+p` to export the current session to a markdown file:

```haskell
handleDumpSessionToMarkdown :: EventM N TuiState ()
handleDumpSessionToMarkdown = do
    mSession <- use (tuiCore . coreSession)
    mConvId <- getFocusedConversationId
    case (mSession, mConvId) of
        (Just session, Just (ConversationId cid)) -> do
            let markdown = formatSessionMarkdown Chronological session
                fileName = "conv." <> show cid <.> "md"
            liftIO $ TextIO.writeFile fileName markdown
            showStatus StatusInfo $ "Exported to " <> Text.pack fileName
        ...
```

### View with External Viewer

The TUI can display session content using an external markdown viewer configured via the `AGENT_MD_VIEWER` environment variable.

**Chronological Order (Oldest First):**
Press `Ctrl+t` to view the session in chronological order (oldest messages first):

```haskell
VtyEvent (Vty.EvKey (Vty.KChar 't') [Vty.MCtrl]) ->
    handleViewSessionWithExternalViewer Chronological
```

**Antichronological Order (Newest First):**
Press `Ctrl+r` to view the session in reverse chronological order (newest messages first):

```haskell
VtyEvent (Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl]) ->
    handleViewSessionWithExternalViewer Antichronological
```

**Example:**
```bash
# Set viewer (e.g., glow, bat, less)
export AGENT_MD_VIEWER="glow -p"

# Or use a pager
export AGENT_MD_VIEWER="less -R"

# Then start TUI
agents-exe tui --agent-file agent.json
```

### Order Preference

```haskell
data OrderPreference = Chronological | Antichronological

formatSessionMarkdown :: OrderPreference -> Session -> Text.Text
formatSessionMarkdown orderPref session =
    let opts = SessionPrintOptions
            { ...
            , orderPreference = orderPref
            , ...
            }
     in formatSessionAsMarkdown opts session
```

Use cases:
- **Chronological (`Ctrl+t`)**: Best for reviewing the full conversation from start to finish
- **Antichronological (`Ctrl+r`)**: Best when you care about recent changes and want to see the most recent messages first

## Running the TUI

### Main Entry Point

```haskell
-- TUI.Core
runTUI :: SessionStore -> [AgentTree.Props] -> IO ()
runTUI store propsList = do
    -- Initialize agents
    agents <- initializeAgents propsList
    
    -- Create initial state
    let initialState = TUIState
            { agents = agents
            , currentAgentIndex = 0
            , inputBuffer = ""
            , scrollOffset = 0
            , status = Ready
            , conversationTreeExpanded = Set.empty  -- NEW
            , selectedConversationPath = []         -- NEW
            }
    
    -- Create event channel
    chan <- newBChan 10
    
    -- Build application
    let app = App
            { appDraw = render
            , appChooseCursor = neverShowCursor
            , appHandleEvent = handleEvent
            , appStartEvent = return
            , appAttrMap = const attributeMap
            }
    
    -- Run
    void $ customMain 
        (V.mkVty V.defaultConfig) 
        (Just chan) 
        app 
        initialState
```

## Styling

### Attributes

```haskell
attributeMap :: AttrMap
attributeMap = attrMap V.defAttr
    [ (headerAttr, fg white `on` blue)
    , (activeTabAttr, fg black `on` yellow)
    , (inactiveTabAttr, fg white)
    , (userAttr, fg cyan)
    , (agentAttr, fg green)
    , (toolAttr, fg yellow)
    , (toolSuccessAttr, fg green)
    , (toolErrorAttr, fg red)
    , (systemAttr, fg magenta)
    , (inputAttr, fg white)
    , (subAgentAttr, fg blue)  -- NEW: For sub-agent messages
    ]
```

## Keyboard Shortcuts

| Key | Action |
|-----|--------|
| `Tab` | Switch to next agent (global) / Toggle expansion (conversation list) |
| `Shift+Tab` | Switch to previous agent |
| `Enter` | Send message |
| `Ctrl+C` | Quit |
| `Ctrl+R` | Refresh tools |
| `Ctrl+P` | Export session to markdown file |
| `Ctrl+T` | View session in chronological order (oldest first) |
| `Ctrl+R` | View session in antichronological order (newest first) |
| `Up/Down` | Scroll conversation |
| `PageUp/PageDown` | Scroll by page |
| `Home` | Scroll to top |
| `End` | Scroll to bottom |
| `→` (Right Arrow) | Expand conversation tree node |
| `←` (Left Arrow) | Collapse conversation tree node |

## Status Bar

```haskell
renderFooter :: TUIState -> Widget ()
renderFooter state = 
    withAttr footerAttr $ 
        hBox
            [ str $ show (status state)
            , fill ' '
            , str "[Tab] Switch  [Enter] Send  [Ctrl+C] Quit"
            ]

data Status = Ready | Streaming | ToolRunning Text | Error Text | SubAgentRunning Text

instance Show Status where
    show Ready = "Ready"
    show Streaming = "Streaming..."
    show (ToolRunning name) = "Running: " ++ unpack name
    show (SubAgentRunning name) = "Sub-agent: " ++ unpack name  -- NEW
    show (Error msg) = "Error: " ++ unpack msg
```

## Best Practices

### Performance

1. **Limit scrollback**: Keep only last N messages in memory
2. **Lazy rendering**: Don't render off-screen content
3. **Rate limiting**: Throttle UI updates during streaming
4. **Tree pruning**: Limit tree depth for deeply nested calls

### User Experience

1. **Visual feedback**: Show typing indicators and tool calls
2. **Error handling**: Display errors without crashing
3. **Help text**: Always show keyboard shortcuts
4. **Tree indicators**: Use clear visual cues for parent/child relationships

### Multi-Agent UI

1. **Clear indicators**: Show which agent is active
2. **Separate contexts**: Each agent maintains its own conversation
3. **Easy switching**: Tab between agents quickly
4. **Hierarchical view**: Show conversation tree structure clearly

### Sub-Agent Display

1. **Auto-expand on start**: When sub-agent starts, expand parent
2. **Status updates**: Show sub-agent lifecycle in status bar
3. **Error propagation**: Display sub-agent errors in parent context
4. **Collapse completed**: Optionally collapse completed sub-agents

## Customization

### Custom Event Handlers

```haskell
customHandleEvent :: TUIState -> BrickEvent () CustomEvent -> EventM () (Next TUIState)
customHandleEvent state ev = case ev of
    -- Add custom shortcuts
    VtyEvent (EvKey (KChar 's') [MCtrl]) -> do
        liftIO $ saveSession state
        continue $ state { status = Ready }
    
    VtyEvent (EvKey (KChar 'l') [MCtrl]) -> do
        newState <- liftIO $ loadSession state
        continue newState
    
    -- Tree navigation
    VtyEvent (EvKey (KChar 'e') []) ->
        continue $ expandAll state
    VtyEvent (EvKey (KChar 'c') []) ->
        continue $ collapseAll state
    
    _ -> handleEvent state ev  -- Fall through to default
```

### Custom Widgets

```haskell
customProgressBar :: Float -> Widget ()
customProgressBar progress = 
    let width = 20
        filled = round (progress * fromIntegral width)
        bar = replicate filled '█' ++ replicate (width - filled) '░'
    in withAttr progressAttr $ str $ "[" ++ bar ++ "]"

-- Tree depth indicator
treeDepthIndicator :: Int -> Widget ()
treeDepthIndicator depth =
    withAttr treeDepthAttr $ str $ replicate depth '│' ++ "─"
```

