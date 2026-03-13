# Terminal UI (TUI)

The Terminal UI provides an interactive, real-time interface for agent conversations with support for multiple agents, streaming responses, and visual feedback.

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
        , renderConversation state
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

### Custom Events

```haskell
data CustomEvent
    = AgentResponse AgentIndex Text
    | ToolStarted AgentIndex Text
    | ToolCompleted AgentIndex Text Value
    | AgentError AgentIndex Text
    | StreamChunk AgentIndex Text
    | StreamComplete AgentIndex

eventChannel :: BChan CustomEvent
eventChannel = newBChan 100
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

### Agent Initialization

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
    ]
```

## Keyboard Shortcuts

| Key | Action |
|-----|--------|
| `Tab` | Switch to next agent |
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

data Status = Ready | Streaming | ToolRunning Text | Error Text

instance Show Status where
    show Ready = "Ready"
    show Streaming = "Streaming..."
    show (ToolRunning name) = "Running: " ++ unpack name
    show (Error msg) = "Error: " ++ unpack msg
```

## Best Practices

### Performance

1. **Limit scrollback**: Keep only last N messages in memory
2. **Lazy rendering**: Don't render off-screen content
3. **Rate limiting**: Throttle UI updates during streaming

### User Experience

1. **Visual feedback**: Show typing indicators and tool calls
2. **Error handling**: Display errors without crashing
3. **Help text**: Always show keyboard shortcuts

### Multi-Agent UI

1. **Clear indicators**: Show which agent is active
2. **Separate contexts**: Each agent maintains its own conversation
3. **Easy switching**: Tab between agents quickly

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
```

