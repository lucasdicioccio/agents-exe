# Terminal UI (TUI)

The Terminal UI provides an interactive, real-time interface for agent conversations with support for multiple agents, streaming responses, visual feedback, and a tabbed interface for organizing different views.

## Overview

```
┌─────────────────────────────────────────────────────────────────┐
│ Agents │ Chats │ History │ Help                                  │
├─────────────────────────────────────────────────────────────────┤
│  Agents                                                         │
│  ─────────────────────────────────────────────────────────────  │
│  file-assistant                                                 │
│  code-reviewer                                                  │
│  documenter                                                     │
│                                                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  # Slug: file-assistant                                         │
│  # Announce: A helpful file assistant                           │
│  # Model: claude-sonnet-4-20250514                              │
│                                                                 │
│  # Tools:                                                       │
│  - [A] read_file                                               │
│  - [A] write_file                                              │
│  - [D:bash] bash_command                                       │
│                                                                 │
├─────────────────────────────────────────────────────────────────┤
│ [Tab] Switch  [Enter] Send  [Ctrl+C] Quit  [Ctrl+[|]] Prev/Next │
└─────────────────────────────────────────────────────────────────┘
```

## Tabbed Interface

The TUI features a tabbed interface with four main tabs:

| Tab | Description | Content |
|-----|-------------|---------|
| **Agents** | Browse and select agents | Agent list and detailed agent information |
| **Chats** | Active conversations | Conversation list and message interface |
| **History** | Past sessions | Session list and history view |
| **Help** | Keyboard shortcuts | Command reference and key bindings |

### Tab Navigation

| Key | Action |
|-----|--------|
| `Ctrl+[` | Switch to previous tab |
| `Ctrl+]` | Switch to next tab |

### Agents Tab

The Agents tab displays:
- **Left sidebar**: List of available agents
- **Main area**: Detailed agent information including:
  - Slug and announce text
  - Model name
  - Tools with activation status
  - System prompt

### Chats Tab

The Chats tab is for active conversations:
- **Left sidebar**: List of ongoing conversations with status indicators:
  - `⟳` - Active (agent is processing)
  - `●` - Waiting for input (unread)
  - `⏸` - Paused
- **Main area**: Message editor, queued messages (when paused), and conversation history

### History Tab

The History tab shows saved sessions:
- **Left sidebar**: List of saved sessions from the session store
- **Main area**: Session content viewer

### Help Tab

The Help tab displays keyboard shortcuts and command reference for quick access to all TUI functionality.

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
data Tab
    = AgentsTab
    | ChatsTab
    | HistoryTab
    | HelpTab
    deriving (Show, Eq)

data UIState = UIState
    { _uiFocusRing :: FocusRing WidgetName
    , _currentTab :: Tab           -- Current active tab
    , _helpContent :: [Text]       -- Help text lines
    , _turnNavigation :: Maybe TurnNavigationState
    -- ^ When Just, we are in turn navigation mode
    , _queuedMessagesFocus :: Maybe Int
    -- ^ Index of currently selected queued message
    , ...
    }

data TUIState = TUIState
    { _tuiCore :: TVar Core
    , _tuiUI :: UIState
    , _eventChan :: BChan AppEvent
    , _sessionConfig :: SessionConfig
    }
```

## Rendering

### Tab Bar Rendering

```haskell
-- TUI.Render
renderTabBar :: Tab -> Widget N
renderTabBar activeTab =
    let tabs = [AgentsTab, ChatsTab, HistoryTab, HelpTab]
        renderTab tab =
            let tabName = case tab of
                    AgentsTab -> " Agents "
                    ChatsTab -> " Chats "
                    HistoryTab -> " History "
                    HelpTab -> " Help "
                tabAttr = if tab == activeTab 
                          then activeTabAttr 
                          else inactiveTabAttr
             in withAttr tabAttr $ txt tabName
     in hBox (intersperse separator $ map renderTab tabs)
```

### Tab-Specific Content

```haskell
render_contentArea :: TuiState -> Widget N
render_contentArea st =
    case st ^. tuiUI . currentTab of
        AgentsTab -> renderAgentsTab st
        ChatsTab -> renderChatsTab st
        HistoryTab -> renderHistoryTab st
        HelpTab -> renderHelpTab st
```

## Event Handling

### Tab Switching

```haskell
-- TUI.Event
tui_appHandleEvent tracer ev = do
    case ev of
        -- Tab switching
        VtyEvent (Vty.EvKey (Vty.KChar '[') [Vty.MCtrl]) ->
            cycleTabBackward
        VtyEvent (Vty.EvKey (Vty.KChar ']') [Vty.MCtrl]) ->
            cycleTabForward
        -- ...

cycleTabForward :: EventM N TuiState ()
cycleTabForward = do
    current <- use (tuiUI . currentTab)
    let next = nextTab current
    tuiUI . currentTab .= next
    -- Update focus ring for the new tab
    mCurrentFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
    tuiUI . uiFocusRing .= buildFocusRingForTabPreserving next mCurrentFocus
```

## Turn Navigation

Turn navigation allows you to browse through conversation history turn-by-turn and fork new conversations from any point.

### Entering Navigation Mode

Press `Enter` when focused on the Conversation view or Session view to enter turn navigation mode:

```
┌─────────────────────────────────────────────────────────────┐
│ Conversation - Turn Navigation (3/8) [Enter:exit F:fork]    │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ -----------------------                                 │ │
│ │▶ < What is the best approach for...      ← SELECTED    │ │
│ │  + ...                                                  │ │
│ │                                                         │ │
│ │ -----------------------                                 │ │
│ │  < You could consider using...                          │ │
│ └─────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

### Navigation Controls

| Key | Action |
|-----|--------|
| `Up` | Navigate to earlier turn |
| `Down` | Navigate to later turn |
| `F` | Fork conversation at selected turn |
| `Enter` | Exit navigation mode |
| `Esc` | Exit navigation mode |

### Forking Conversations

Forking creates a new conversation starting from the selected turn, preserving only the turns before it:

```
Turn 0: User - "Hello!"
Turn 1: Assistant - "Hi there!"
Turn 2: User - "How do I..." ← Selected for fork
Turn 3: Assistant - "You can..."

Forking at Turn 2 creates new conversation with:
Turn 0: User - "Hello!"
Turn 1: Assistant - "Hi there!"
(New conversation starts here)
```

The original session remains untouched. The forked session has `forkedFromSessionId` set to the original session's ID.

### Turn Navigation Types

```haskell
-- | State for turn-by-turn navigation
data TurnNavigationState = TurnNavigationState
    { _navSession :: Session
    -- ^ The session being navigated
    , _navSelectedTurnIndex :: Int
    -- ^ Currently selected turn index (0-based)
    , _navTotalTurns :: Int
    -- ^ Total number of turns for display
    }

-- | Widget name for turn navigation viewport
data WidgetName
    = ...
    | TurnNavigationWidget
    -- ^ For viewport scrolling during turn navigation
```

## Message Queue Management

When a conversation is paused, you can manage queued messages - messages typed while the agent was processing.

### Queue Management UI

When paused with queued messages, the Chats tab shows a queue management panel:

```
┌─────────────────────────────────────────────────────────────┐
│ Message                                                      │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ User's typed message...                                 │ │
│ └─────────────────────────────────────────────────────────┘ │
├─────────────────────────────────────────────────────────────┤
│ Queued Messages (2) - Ctrl+D: clear  Del: delete selected │ │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ ▶ First queued message text...                          │ │
│ │   Second queued message that is longer...               │ │
│ └─────────────────────────────────────────────────────────┘ │
├─────────────────────────────────────────────────────────────┤
│ Conversation                                                 │
│ ...existing conversation content...                          │
└─────────────────────────────────────────────────────────────┘
```

### Queue Management Controls

| Key | Action |
|-----|--------|
| `Ctrl+D` | Clear all queued messages |
| `Del` / `Backspace` | Delete selected message |
| `Up` | Select previous message |
| `Down` | Select next message |

### Activation

Queue management is only available when:
- The conversation status is `ConversationStatus_Paused`
- There are queued messages in the buffer

To pause/unpause a conversation, press `Ctrl+E`.

### Queue State

```haskell
data UIState = UIState
    { ...
    , _uiBufferedMessages :: Map ConversationId [Text]
    -- ^ Copy of buffered messages from Core for UI rendering
    , _queuedMessagesFocus :: Maybe Int
    -- ^ Index of currently selected queued message (Nothing = none selected)
    }

-- The Core also maintains the source of truth
data Core = Core
    { ...
    , coreBufferedMessages :: TVar (Map ConversationId [Text])
    -- ^ Buffered messages per conversation
    , corePausedConversations :: Set ConversationId
    -- ^ Set of paused conversation IDs
    }
```

## Keyboard Shortcuts

### Navigation

| Key | Action |
|-----|--------|
| `Tab` | Cycle focus forward through widgets |
| `Shift+Tab` | Cycle focus backward through widgets |
| `Ctrl+Z` | Toggle zoom mode for current widget |

### Tabs

| Key | Action |
|-----|--------|
| `Ctrl+[` | Switch to previous tab |
| `Ctrl+]` | Switch to next tab |

### Conversations

| Key | Action |
|-----|--------|
| `Ctrl+N` | Start new conversation with selected agent |
| `Ctrl+C` | Continue restored session |
| `Meta+Enter` | Send message |
| `Ctrl+E` | Pause/unpause conversation |

### Turn Navigation

| Key | Action |
|-----|--------|
| `Enter` | Enter turn navigation mode (when on conversation) |
| `Up/Down` | Navigate between turns (in navigation mode) |
| `F` | Fork conversation at selected turn |
| `Enter/Esc` | Exit turn navigation mode |

### Queue Management (when paused)

| Key | Action |
|-----|--------|
| `Ctrl+D` | Clear all queued messages |
| `Del` / `Backspace` | Delete selected queued message |
| `Up/Down` | Select queued message |

### Session Export

| Key | Action |
|-----|--------|
| `Ctrl+P` | Export session to markdown file |
| `Ctrl+T` | View session in external viewer (chronological) |
| `Ctrl+R` | View session in external viewer (reverse) |

### Other

| Key | Action |
|-----|--------|
| `F5` | Refresh tools for selected agent |
| `Esc`, `Ctrl+Q` | Quit application |

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
runTUI :: Tracer IO Trace -> SessionStore -> LoadedApiKeys -> [Props] -> IO ()
runTUI tracer store apiKeys propsList = do
    let config = fileSessionConfig store apiKeys
    runTUIWithConfig tracer config propsList

runTUIWithConfig :: Tracer IO Trace -> SessionConfig -> [Props] -> IO ()
runTUIWithConfig tracer config props = do
    -- Load agent trees and create TuiAgents
    trees <- traverse loadAgentTree props
    let itrees = [tree | Initialized tree <- trees]

    -- Create TUI agents from OS-native trees
    let tuiAgents = map createTuiAgent itrees

    -- Load existing session files
    loadedSessions <- loadSessionFiles config.sessionStore

    -- Collect tools from all agents
    agentTools <- collectAgentTools tuiAgents

    -- Create event channel
    evChan <- newBChan 100

    -- Create core state
    core0 <- initCore tuiAgents
    coreTVar <- newTVarIO core0

    -- Create UI state with help content initialized
    let ui0 = initHelpContent $
            (initUIState tuiAgents [s | (_, Just s) <- loadedSessions])
                { _coreAgentTools = agentTools }

    -- Create TUI state
    let st = TuiState coreTVar ui0 evChan config

    -- Build and run the app
    let app = App
            { appDraw = tui_appDraw
            , appChooseCursor = tui_appChooseCursor
            , appHandleEvent = tui_appHandleEvent tracer
            , appStartEvent = tui_appStartEvent
            , appAttrMap = tui_appAttrMap
            }

    void $ customMainWithDefaultVty (Just evChan) app st
```

## Styling

### Attributes

```haskell
tui_appAttrMap :: TuiState -> AttrMap
tui_appAttrMap _ =
    attrMap Vty.defAttr
        [ (headerAttr, fg white `on` blue)
        , (activeTabAttr, fg black `on` brightWhite `withStyle` bold)
        , (inactiveTabAttr, fg white `on` blue)
        , (userAttr, fg cyan)
        , (agentAttr, fg green)
        , (toolAttr, fg yellow)
        , (toolSuccessAttr, fg green)
        , (toolErrorAttr, fg red)
        , (systemAttr, fg magenta)
        , (inputAttr, fg white)
        , (queuedMessageAttr, fg yellow)
        , (queuedMessageSelectedAttr, bg blue `withStyle` bold)
        , (selectedTurnAttr, bg blue `withStyle` bold)
        ]
```

## Status Bar

```haskell
render_statusBar :: Maybe StatusMessage -> Widget N
render_statusBar Nothing = emptyWidget
render_statusBar (Just msg) =
    withAttr (statusAttr msg.statusSeverity) $
        txt $ " " <> statusText msg

data StatusSeverity
    = StatusInfo
    | StatusWarning
    | StatusError
    deriving (Show, Eq)
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
4. **Tab organization**: Group related functionality into logical tabs
5. **Pause before queue management**: Queue management only works when paused to prevent accidental modifications

### Multi-Agent UI

1. **Clear indicators**: Show which agent is active
2. **Separate contexts**: Each agent maintains its own conversation
3. **Easy switching**: Tab between agents quickly

### Conversation Forking

1. **Non-destructive**: Original session always preserved
2. **Clear lineage**: Forked sessions track their origin
3. **Agent selection**: Current agent selection used for fork
4. **Navigation mode**: Enter navigation to review before forking

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

