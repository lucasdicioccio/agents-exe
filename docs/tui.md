# Terminal UI (TUI)

The Terminal UI provides an interactive, real-time interface for agent conversations with support for multiple agents, streaming responses, visual feedback, file attachments, clipboard integration, subcall conversation visibility, and a tabbed interface for organizing different views.

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
  - `📎` - Has file attachments
- **Main area**: Message editor, attachment list, queued messages (when paused), and conversation history

### History Tab

The History tab shows saved sessions:
- **Left sidebar**: List of saved sessions from the session store
- **Main area**: Session content viewer with search functionality

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
    , _attachedFiles :: Map ConversationId [MediaAttachment]
    -- ^ Media attachments per conversation
    , _attachmentDialogState :: AttachmentDialogState
    -- ^ File attachment dialog state
    , _filePathInput :: Editor Text WidgetName
    -- ^ Editor for file path input
    , _selectedAttachmentIndex :: Maybe Int
    -- ^ Selected attachment index
    , ...
    }

data TUIState = TuiState
    { _tuiCore :: TVar Core
    , _tuiUI :: UIState
    , _eventChan :: BChan AppEvent
    , _sessionConfig :: SessionConfig
    }
```

## Subcall Conversation Visibility

When agents call other agents as tools (via `prompt_agent_<slug>`), the subcall conversations are now visible in the TUI with visual distinction and hierarchy tracking.

### Visual Hierarchy

Subcall conversations are displayed with tree-branch styling to show parent-child relationships:

```
Conversations
├─ @file-assistant (⟳)                    -- Root conversation
│  ├─ @code-reviewer (⟳)                  -- Subcall depth 1
│  │  └─ @syntax-checker (●)              -- Subcall depth 2
│  └─ @documenter (●)                     -- Subcall depth 1
└─ @helper-agent (●)                      -- Another root conversation
```

### Conversation Indicators

| Visual | Meaning |
|--------|---------|
| `@slug` | Root conversation (depth 0) |
| `├─ @slug` | Subcall with siblings |
| `└─ @slug` | Last subcall in branch |
| `│` | Continuation line for parent with more children |
| Dimmed text | Subcall conversation |
| Normal text | Root conversation |
| `⟳` | Active (processing) |
| `●` | Waiting for input |
| `⏸` | Paused |

### Subcall Event Flow

```
Parent Agent (TUI visible)
       │
       ▼ calls helper agent
┌─────────────────────────┐
│ turnAgentRuntimeIntoIOTool
│ (OneShotTool.hs)        │
│                         │
│ 1. Insert into OS World │
│    - ConversationConfig │
│    - ConversationState  │
│    - Lineage (parent)   │
│                         │
│ 2. Emit SubcallStarted  │◄── OSEvent
│    (parentId, convId)   │
│                         │
│ 3. Run sub-agent        │
│                         │
│ 4. Emit SubcallProgress │◄── OSEvent (after each step)
│                         │
│ 5. Emit SubcallCompleted│◄── OSEvent (on success)
│    or SubcallFailed     │◄── OSEvent (on error)
└──────────┬──────────────┘
           │
           ▼ OS Event Queue
┌─────────────────────────┐
│   TUI Event Handler     │
│   (Event.hs)            │
│                         │
│ - Create conversation   │
│ - Show in list          │
│ - Update on progress    │
│ - Mark completed        │
└─────────────────────────┘
```

### Subcall Event Types

```haskell
-- OS Events (from System.Agents.OS.Events)
data OSEvent
    = ...
    | OSEvent_SubcallStarted
        { subcallParentConversationId :: ConversationId
        , subcallConversationId :: ConversationId
        , subcallAgentSlug :: Text
        , subcallDepth :: Int
        }
    | OSEvent_SubcallProgress
        { subcallProgressConversationId :: ConversationId
        , subcallProgressSession :: Session
        }
    | OSEvent_SubcallCompleted
        { subcallCompletedConversationId :: ConversationId
        , subcallCompletedResult :: Text
        }
    | OSEvent_SubcallFailed
        { subcallFailedConversationId :: ConversationId
        , subcallFailedError :: Text
        }

-- App Events (TUI internal)
data AppEvent
    = ...
    | AppEvent_SubcallStarted ConversationId ConversationId Text Int
    | AppEvent_SubcallProgress ConversationId Session
    | AppEvent_SubcallCompleted ConversationId Text
    | AppEvent_SubcallFailed ConversationId Text
```

### Conversation Types

```haskell
data Conversation = Conversation
    { conversationId :: ConversationId
    , conversationAgent :: TuiAgent
    , conversationThreadId :: Maybe ThreadId
    , conversationSession :: Maybe Session
    , conversationName :: Text
    , conversationChan :: BChan (Maybe UserQuery)
    , conversationStatus :: ConversationStatus
    , conversationOnProgress :: OnSessionProgress
    , conversationIsSubcall :: Bool      -- ^ NEW: Is this a subcall?
    , conversationParentId :: Maybe ConversationId  -- ^ NEW: Parent conversation
    , conversationSubcallDepth :: Int    -- ^ NEW: Nesting depth (0 = root)
    }
```

### Rendering Subcall Hierarchy

```haskell
-- Render conversations with tree structure
renderConversationForest :: [ConversationTree] -> [Widget N]
renderConversationForest trees =
    concatMap (renderTreeNode [] False) trees

-- Build tree from flat list
buildConversationForest :: [Conversation] -> [ConversationTree]
buildConversationForest convs =
    let -- Find roots (no parent or orphaned)
        isRoot c = conversationParentId c == Nothing
                || conversationParentId c `notElem` map (Just . conversationId) convs
        roots = filter isRoot convs
        
        -- Build recursively
        buildTree conv = ConversationTree conv $
            map buildTree (findChildren conv)
        findChildren parent =
            filter (\c -> conversationParentId c == Just (conversationId parent)) convs
    in map buildTree roots

-- Make prefix with tree branches
makePrefix :: [Bool] -> Bool -> Text
makePrefix ancestorIsLasts isLast
    | null ancestorIsLasts = if isLast then "└─" else "├─"
    | otherwise =
        let continuation = mconcat $
                map (\isLast' -> if isLast' then "  " else "│ ") ancestorIsLasts
        in continuation <> (if isLast then "└─" else "├─")
```

### Visual Attributes

```haskell
tui_appAttrMap :: TuiState -> AttrMap
tui_appAttrMap _ =
    attrMap Vty.defAttr
        [ ...
        , -- Subcall visual distinction
          (subcallAttr, fg white `withStyle` dim)
        , (subcallSelectedAttr, fg black `on` brightWhite `withStyle` bold)
        , (treeBranchAttr, fg white `withStyle` dim)
        , (rootConversationAttr, defAttr)
        ]
```

## File Attachments

The TUI supports attaching files to messages for multi-modal LLM interactions.

### Attaching Files

**Via File Path Input:**
- Press `Ctrl+F` to open the file path input dialog
- Type or paste the absolute path to the file
- Press `Enter` to attach, `Esc` to cancel

**Supported file path formats:**
```
/path/to/image.png                    # Auto-detect MIME type
image/png;/path/to/image.png          # Explicit MIME type
```

### Attachment Display

Attached files are displayed below the message editor:

```
┌─────────────────────────────────────────────────────────────┐
│ Message [2 attachments]                                     │
├─────────────────────────────────────────────────────────────┤
│ > Your message here...                                      │
│                                                             │
├─────────────────────────────────────────────────────────────┤
│ Attachments (2) - Del/Backspace: remove | Ctrl+Shift+F: clear all│
│   📎 screenshot.png (image/png, 245KB)                     │
│ ▶ 📎 report.pdf (application/pdf, 1.2MB)                   │
└─────────────────────────────────────────────────────────────┘
```

### Managing Attachments

| Key | Action |
|-----|--------|
| `Ctrl+F` | Open file path input dialog |
| `Ctrl+Shift+F` | Clear all attachments |
| `Del` / `Backspace` | Remove selected attachment |
| `Up` / `Down` | Select attachment (when focus is on attachment list) |

### Supported File Types

The TUI can attach any file type. MIME type detection is automatic based on file extension:

| Category | Extensions | MIME Types |
|----------|------------|------------|
| **Images** | `.png`, `.jpg`, `.jpeg`, `.gif`, `.webp`, `.svg` | `image/png`, `image/jpeg`, `image/gif`, `image/webp`, `image/svg+xml` |
| **Documents** | `.pdf`, `.txt`, `.md`, `.json`, `.xml` | `application/pdf`, `text/plain`, `text/markdown`, `application/json`, `application/xml` |
| **Audio** | `.mp3`, `.wav`, `.ogg`, `.aac`, `.flac` | `audio/mp3`, `audio/wav`, `audio/ogg`, `audio/aac`, `audio/flac` |
| **Video** | `.mp4`, `.webm`, `.mov`, `.avi` | `video/mp4`, `video/webm`, `video/quicktime`, `video/avi` |
| **Archives** | `.zip` | `application/zip` |

**Size Limit:** 50MB per file

### Attachment State

```haskell
-- Attachments are stored per conversation
type AttachedFiles = Map ConversationId [MediaAttachment]

data MediaAttachment = MediaAttachment
    { mediaMimeType :: Text        -- e.g., "image/png"
    , mediaBase64Data :: Text      -- Base64-encoded content
    , mediaFilename :: Maybe Text  -- Original filename
    }

-- Dialog state for file attachment
data AttachmentDialogState
    = AttachmentDialogClosed
    | AttachmentDialogPathInput
```

### Attachment Flow

```
User presses Ctrl+F
       │
       ▼
┌──────────────────┐
│ Open path dialog │
│ (text input)     │
└────────┬─────────┘
         │
         ▼
┌──────────────────┐
│ User enters path │
│ Presses Enter    │
└────────┬─────────┘
         │
         ▼
┌──────────────────┐
│ Load file        │
│ Detect MIME type │
│ Base64 encode    │
└────────┬─────────┘
         │
         ▼
┌──────────────────┐
│ Add to           │
│ attachedFiles    │
│ map              │
└────────┬─────────┘
         │
         ▼
┌──────────────────┐
│ Render in        │
│ attachment list  │
└──────────────────┘
```

## Clipboard Integration

The TUI supports pasting content from the system clipboard, including images, file paths, and text.

### Clipboard Pasting

Press `Ctrl+V` to paste from clipboard:

| Content Type | Action |
|--------------|--------|
| **Image** | Save to temp file and attach |
| **File path** | Attach the file |
| **Multiple file paths** | Attach all valid files |
| **Text** | Insert into message editor |

### Platform Support

| Platform | Backend | Required Tools |
|----------|---------|----------------|
| **Linux (X11)** | `xclip` or `xsel` | `xclip` or `xsel` |
| **Linux (Wayland)** | `wl-clipboard` | `wl-paste` |
| **macOS** | Built-in | `pbpaste` (included) |
| **Windows** | PowerShell | `powershell.exe` |

### Smart Content Detection

The clipboard module automatically detects content type:

```haskell
data ClipboardContent
    = ClipboardImage ByteString Text    -- Image data with MIME type
    | ClipboardFilePath FilePath        -- Single file path
    | ClipboardText Text                -- Plain text
    | ClipboardFilePaths [FilePath]     -- Multiple file paths
    | ClipboardUnknown                  -- Unsupported content
```

Detection order:
1. Check for image data (via magic bytes: PNG `\x89PNG`, JPEG `\xFF\xD8\xFF`, GIF `GIF87a/GIF89a`, WebP `RIFF`)
2. Check for file paths (valid paths that exist)
3. Check for multiple file paths (one per line)
4. Fall back to plain text

### Clipboard Module

```haskell
-- System.Agents.TUI.Clipboard

-- Detect available backend
detectBackend :: IO ClipboardBackend

-- Read clipboard content
readClipboard :: ClipboardBackend -> IO (Maybe ByteString)

-- Detect content type
detectClipboardContent :: IO (Maybe ClipboardContent)

-- Analyze and convert to action
analyzeContent :: ClipboardContent -> IO ContentAction

data ContentAction
    = AttachAsMedia MediaAttachment
    | PasteAsText Text
    | AttachMultipleFiles [MediaAttachment]
    | IgnoreContent
```

### Image Pasting from Clipboard

When an image is pasted from clipboard:

1. Detect image format from magic bytes
2. Save to temporary file with appropriate extension
3. Create `MediaAttachment` with detected MIME type
4. Attach to current conversation
5. Show status: "Attached from clipboard: image.png"

**Temporary file location:** `$TMPDIR/agents-exe-clipboard/clipboard-*.png`

**Size limit:** 50MB for clipboard images

### File Drop Support (Terminal Protocols)

The clipboard module also supports file drop events from modern terminals:

| Protocol | Terminal | Support |
|----------|----------|---------|
| iTerm2 File Drop | iTerm2 (macOS) | ✅ Supported |
| Kitty Graphics | Kitty | ✅ Supported |
| OSC 52 | Various | ✅ Read support |

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

### Attachment List Rendering

```haskell
render_attachmentPanel :: TuiState -> [MediaAttachment] -> Widget N
render_attachmentPanel st attachments =
    borderWithFocus
        st
        AttachmentListWidget
        (" Attachments (" <> Text.pack (show $ length attachments) <> ") ")
        $ vBox
            [ txt "Del/Backspace: remove | Ctrl+Shift+F: clear all"
            , txt ""
            , vBox $ zipWith (render_attachment_item selectedIdx) [0 ..] attachments
            ]

render_attachment_item :: Maybe Int -> Int -> MediaAttachment -> Widget N
render_attachment_item selectedIdx idx att =
    let isSelected = selectedIdx == Just idx
        marker = if isSelected then "▶ " else "  "
        filename = maybe "unnamed" id att.mediaFilename
        mimeType = att.mediaMimeType
        sizeStr = formatAttachmentSize att.mediaBase64Data
     in hBox
         [ txt marker
         , txt "📎 "
         , txt filename
         , txt " ("
         , withAttr attachmentSizeAttr $ txt mimeType
         , txt ", "
         , withAttr attachmentSizeAttr $ txt sizeStr
         , txt ")"
         ]
```

### Conversation List with Nesting

```haskell
-- Render conversation list with subcall hierarchy
render_conversationList :: TuiState -> Widget N
render_conversationList st =
    let convs = Vector.toList (listElements (st ^. tuiUI . conversationList))
        forest = buildConversationForest convs
        hasFocus = focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just ConversationListWidget
        selectedId = case listSelectedElement (st ^. tuiUI . conversationList) of
            Just (_, conv) -> Just (conversationId conv)
            Nothing -> Nothing
     in borderWithFocus
            st
            ConversationListWidget
            "Conversations"
            ( viewport ConversationListWidget Both $
                vBox $ renderConversationForest st selectedId hasFocus forest
            )

renderConversationForest :: TuiState -> Maybe ConversationId -> Bool -> [ConversationTree] -> [Widget N]
renderConversationForest st selectedId hasFocus trees =
    concatMap (\(idx, tree) -> renderTreeNode st selectedId hasFocus [] (idx == length trees - 1) tree) (zip [0 ..] trees)

renderTreeNode :: TuiState -> Maybe ConversationId -> Bool -> [Bool] -> Bool -> ConversationTree -> [Widget N]
renderTreeNode st selectedId hasFocus ancestorIsLasts isLast (ConversationTree conv children) =
    let isSelected = selectedId == Just (conversationId conv)
        nodeWidget = renderNestedConversationItem st hasFocus isSelected ancestorIsLasts isLast conv
        childWidgets = concatMap (\(idx, child) -> renderTreeNode st selectedId hasFocus (ancestorIsLasts ++ [isLast]) (idx == length children - 1) child) (zip [0 ..] children)
     in nodeWidget : childWidgets
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

### Subcall Event Handling

```haskell
-- Handle subcall events in both normal and navigation mode
handleNormalEvent tracer ev =
    case ev of
        AppEvent (AppEvent_SubcallStarted parentId subcallId slug depth) ->
            handleSubcallStarted tracer parentId subcallId slug depth
        AppEvent (AppEvent_SubcallProgress subcallId sess) ->
            handleSubcallProgress subcallId sess
        AppEvent (AppEvent_SubcallCompleted subcallId result) ->
            handleSubcallCompleted subcallId result
        AppEvent (AppEvent_SubcallFailed subcallId err) ->
            handleSubcallFailed subcallId err
        ...

-- Create subcall conversation entry
handleSubcallStarted :: Tracer IO Trace -> ConversationId -> ConversationId -> Text -> Int -> EventM N TuiState ()
handleSubcallStarted _tracer parentId subcallId slug depth = do
    agents <- use (tuiUI . agentList . to listElements)
    case findAgentBySlug slug agents of
        Just tuiAgent -> do
            inChan <- liftIO $ newBChan 100
            let conv = Conversation
                    { conversationId = subcallId
                    , conversationAgent = tuiAgent
                    , conversationThreadId = Nothing
                    , conversationSession = Nothing
                    , conversationName = "@" <> tuiSlug tuiAgent
                    , conversationChan = inChan
                    , conversationStatus = ConversationStatus_Active
                    , conversationOnProgress = \_ -> pure ()
                    , conversationIsSubcall = True
                    , conversationParentId = Just parentId
                    , conversationSubcallDepth = depth
                    }
            coreRef <- use tuiCore
            liftIO $ atomically $ modifyTVar coreRef $ appendConversation conv
            tuiUI . conversationList %= listInsert 0 conv
        Nothing -> showStatus StatusWarning $ "Agent not found for subcall: " <> slug

-- Update subcall progress
handleSubcallProgress :: ConversationId -> Session -> EventM N TuiState ()
handleSubcallProgress subcallId sess = do
    coreRef <- use tuiCore
    liftIO $ atomically $ modifyTVar coreRef $ \c ->
        c { coreConversations = updateConversationSession subcallId sess (coreConversations c) }

-- Mark subcall completed
handleSubcallCompleted :: ConversationId -> Text -> EventM N TuiState ()
handleSubcallCompleted subcallId _result = do
    updateConversationStatus subcallId ConversationStatus_WaitingForInput
    showStatus StatusInfo "Subcall completed"
```

### Attachment Event Handling

```haskell
-- Handle Ctrl+F for file attachment
VtyEvent (Vty.EvKey (Vty.KChar 'f') [Vty.MCtrl]) -> do
    resetQuitConfirmation
    openFilePathDialog

-- Handle Ctrl+Shift+F to clear all attachments
VtyEvent (Vty.EvKey (Vty.KChar 'F') [Vty.MCtrl, Vty.MShift]) -> do
    resetQuitConfirmation
    handleClearAllAttachments

-- Handle clipboard paste
VtyEvent (Vty.EvKey (Vty.KChar 'v') [Vty.MCtrl]) -> do
    resetQuitConfirmation
    handleClipboardPaste tracer
```

### Clipboard Paste Handler

```haskell
handleClipboardPaste :: Tracer IO Trace -> EventM N TuiState ()
handleClipboardPaste _tracer = do
    hasSupport <- liftIO hasClipboardSupport
    if not hasSupport
        then showStatus StatusError "Clipboard not available - install xclip, wl-clipboard, or pbpaste"
        else do
            mContent <- liftIO detectClipboardContent
            case mContent of
                Nothing ->
                    showStatus StatusWarning "Clipboard is empty or inaccessible"
                Just content -> do
                    action <- liftIO $ analyzeContent content
                    case action of
                        IgnoreContent ->
                            showStatus StatusWarning "No attachable content in clipboard"
                        PasteAsText text -> do
                            editorContents <- use (tuiUI . messageEditor . editContentsL)
                            let newContents = TextZipper.insertMany text editorContents
                            tuiUI . messageEditor . editContentsL .= newContents
                            showStatus StatusInfo "Text pasted from clipboard"
                        AttachAsMedia attachment -> do
                            mConv <- getFocusedConversation
                            case mConv of
                                Nothing -> showStatus StatusError "No conversation selected"
                                Just conv -> do
                                    let convId = conversationId conv
                                    tuiUI . attachedFiles %= Map.insertWith (\new old -> old ++ new) convId [attachment]
                                    let filename = maybe "unnamed" id attachment.mediaFilename
                                    showStatus StatusInfo $ "Attached from clipboard: " <> filename
                        AttachMultipleFiles attachments -> do
                            mConv <- getFocusedConversation
                            case mConv of
                                Nothing -> showStatus StatusError "No conversation selected"
                                Just conv -> do
                                    let convId = conversationId conv
                                    tuiUI . attachedFiles %= Map.insertWith (\new old -> old ++ new) convId attachments
                                    showStatus StatusInfo $ "Attached " <> Text.pack (show $ length attachments) <> " files from clipboard"
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
├─────────────────────────────────────────────────────────────┤
│ > User's typed message...                                   │
│                                                             │
├─────────────────────────────────────────────────────────────┤
│ Queued Messages (2) - Ctrl+D: clear  Del: delete selected │
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
| `Del` / `Backspace` | Delete selected queued message |
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

### File Attachments

| Key | Action |
|-----|--------|
| `Ctrl+F` | Open file path input dialog |
| `Ctrl+Shift+F` | Clear all attachments |
| `Del` / `Backspace` | Remove selected attachment |
| `Up` / `Down` | Select attachment (when focused) |

### Clipboard

| Key | Action |
|-----|--------|
| `Ctrl+V` | Paste from clipboard (images, files, text) |

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

### Session Search (History Tab)

| Key | Action |
|-----|--------|
| `/` | Start search |
| `n` | Next result |
| `N` | Previous result |
| `Esc` | Clear search |

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

## Session Search

The History tab includes session search functionality for finding past conversations.

### Search Interface

```
┌─────────────────────────────────────────────────────────────┐
│ Sessions                                [Search: database] │
│ ─────────────────────────────────────────────────────────  │
│  ▶ 2024-01-15 10:30 - database migration                  │
│    2024-01-14 15:20 - api design                          │
│  ▶ 2024-01-13 09:00 - database schema review              │
│                                                             │
├─────────────────────────────────────────────────────────────┤
│ Session View                                                 │
│ ...selected session content...                              │
└─────────────────────────────────────────────────────────────┘
```

### Search Features

- **Real-time filtering**: Sessions are filtered as you type
- **Full-text search**: Searches across session content
- **Highlighting**: Matching terms are highlighted
- **Keyboard navigation**: `n`/`N` to jump between results

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

    -- Create OS event queue for subcall visibility
    osEventQueue <- newTQueueIO

    -- Start the event bridge
    startOSEventBridge osEventQueue evChan

    -- Create and initialize the OS World
    world <- atomically initWorld

    -- Create core state with World and EventQueue
    core0 <- initCore (Just world) (Just osEventQueue)
    coreTVar <- newTVarIO core0

    -- Create UI state
    let ui0 = (initUIState defaultHelpContent tuiAgents [s | (_, Just s) <- loadedSessions])
                { _uiAgentTools = agentTools }

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
        , (attachmentAttr, fg cyan)
        , (attachmentSelectedAttr, bg blue `withStyle` bold)
        , (attachmentSizeAttr, fg white `withStyle` dim)
        , -- Subcall conversation attributes
          (subcallAttr, fg white `withStyle` dim)
        , (subcallSelectedAttr, fg black `on` brightWhite `withStyle` bold)
        , (treeBranchAttr, fg white `withStyle` dim)
        , (rootConversationAttr, defAttr)
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
4. **Subcall visibility**: Show nested agent calls with visual hierarchy

### Conversation Forking

1. **Non-destructive**: Original session always preserved
2. **Clear lineage**: Forked sessions track their origin
3. **Agent selection**: Current agent selection used for fork
4. **Navigation mode**: Enter navigation to review before forking

### File Attachments

1. **Size limits**: Warn users about large files
2. **MIME detection**: Automatic type detection from extensions
3. **Visual feedback**: Show attachment count in UI
4. **Temporary cleanup**: Clipboard images are temporary files

### Clipboard Integration

1. **Graceful degradation**: Handle missing clipboard tools gracefully
2. **Security**: Validate file paths before attachment
3. **Size limits**: Prevent memory issues with large clipboard content
4. **Platform detection**: Auto-detect best clipboard backend

### Subcall Visibility

1. **Tree rendering**: Show parent-child relationships clearly
2. **Visual distinction**: Use different attributes for subcalls vs root conversations
3. **Event bridging**: Convert OS events to AppEvents for Brick integration
4. **Orphan handling**: Handle async race conditions where child arrives before parent
5. **Nesting depth**: Track and display recursion depth

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

## Related Modules

| Module | Purpose |
|--------|---------|
| `System.Agents.TUI.Core` | Main TUI application |
| `System.Agents.TUI.Types` | TUI state and types |
| `System.Agents.TUI.Render` | Rendering functions |
| `System.Agents.TUI.Event` | Event handling |
| `System.Agents.TUI.Clipboard` | Clipboard and drag-and-drop support |
| `System.Agents.Media.Types` | Media attachment types |
| `System.Agents.SessionPrint` | Session formatting |
| `System.Agents.OS.Events` | OS event types for subcall visibility |
| `System.Agents.AgentTree.OneShotTool` | Subcall execution |

