{-# LANGUAGE OverloadedStrings #-}

{- | Configurable keymap system for TUI event bindings.

This module provides a flexible keymap system that allows users to customize
keyboard shortcuts via a JSON configuration file. It also includes input
configuration for message composer behavior.
-}
module System.Agents.TUI.KeyMapping (
    -- * Event Names
    EventName (..),
    allEventNames,
    eventNameToText,
    eventNameFromText,

    -- * Key Names
    KeyName (..),
    keyNameToText,
    keyNameFromText,

    -- * Modifiers
    Modifiers (..),
    noModifiers,
    ctrlModifier,
    metaModifier,
    shiftModifier,

    -- * Key Bindings
    KeyBinding (..),

    -- * Key Mapping
    KeyMapping (..),
    defaultKeyMapping,

    -- * Input Configuration (re-exported from MessageComposer)
    InputConfig,
    SendTrigger (..),
    defaultInputConfig,

    -- * Combined TUI User Configuration
    TUIUserConfig (..),
    defaultTUIUserConfig,

    -- * Matching
    matchesEvent,
    keyBindingToVtyEvent,

    -- * Help Text
    generateHelpContent,

    -- * Loading
    loadKeymapFromFile,
    mergeWithDefault,
) where

import Data.Aeson (FromJSON, ToJSON, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Default (Default (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as Vty
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)

-- Import from MessageComposer for re-export and integration
import System.Agents.TUI.MessageComposer (InputConfig, SendTrigger (..), defaultInputConfig)

-------------------------------------------------------------------------------
-- Event Names
-------------------------------------------------------------------------------

{- | Name of a TUI event/action.
These represent all the actions that can be triggered by keyboard shortcuts.
-}
data EventName
    = EventQuit
    | EventSendMessage
    | EventNewConversation
    | EventContinueSession
    | EventTogglePause
    | EventAttachFile
    | EventClearAttachments
    | EventPasteClipboard
    | EventExportSession
    | EventViewSessionChronological
    | EventViewSessionReverse
    | EventClearQueuedMessages
    | EventCycleTabForward
    | EventCycleTabBackward
    | EventCycleFocusForward
    | EventCycleFocusBackward
    | EventToggleZoom
    | EventRefreshTools
    | EventEnterTurnNavigation
    | EventForkAtTurn
    | EventExitTurnNavigation
    | EventNavigateUp
    | EventNavigateDown
    | EventDeleteItem
    | EventOpenConversation
    | EventSaveBuffer
    | EventResumeBuffer
    | EventClearBuffers
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | All possible event names.
allEventNames :: [EventName]
allEventNames = [minBound .. maxBound]

-- | Convert an EventName to its JSON key representation.
eventNameToText :: EventName -> Text
eventNameToText EventQuit = "quit"
eventNameToText EventSendMessage = "send-message"
eventNameToText EventNewConversation = "new-conversation"
eventNameToText EventContinueSession = "continue-session"
eventNameToText EventTogglePause = "toggle-pause"
eventNameToText EventAttachFile = "attach-file"
eventNameToText EventClearAttachments = "clear-attachments"
eventNameToText EventPasteClipboard = "paste-clipboard"
eventNameToText EventExportSession = "export-session"
eventNameToText EventViewSessionChronological = "view-session-chrono"
eventNameToText EventViewSessionReverse = "view-session-reverse"
eventNameToText EventClearQueuedMessages = "clear-queued"
eventNameToText EventCycleTabForward = "next-tab"
eventNameToText EventCycleTabBackward = "prev-tab"
eventNameToText EventCycleFocusForward = "next-focus"
eventNameToText EventCycleFocusBackward = "prev-focus"
eventNameToText EventToggleZoom = "toggle-zoom"
eventNameToText EventRefreshTools = "refresh-tools"
eventNameToText EventEnterTurnNavigation = "enter-turn-nav"
eventNameToText EventForkAtTurn = "fork-at-turn"
eventNameToText EventExitTurnNavigation = "exit-turn-nav"
eventNameToText EventNavigateUp = "navigate-up"
eventNameToText EventNavigateDown = "navigate-down"
eventNameToText EventDeleteItem = "delete-item"
eventNameToText EventOpenConversation = "open-conversation"
eventNameToText EventSaveBuffer = "save-buffer"
eventNameToText EventResumeBuffer = "resume-buffer"
eventNameToText EventClearBuffers = "clear-buffers"

-- | Parse an EventName from its JSON key representation.
eventNameFromText :: Text -> Maybe EventName
eventNameFromText "quit" = Just EventQuit
eventNameFromText "send-message" = Just EventSendMessage
eventNameFromText "new-conversation" = Just EventNewConversation
eventNameFromText "continue-session" = Just EventContinueSession
eventNameFromText "toggle-pause" = Just EventTogglePause
eventNameFromText "attach-file" = Just EventAttachFile
eventNameFromText "clear-attachments" = Just EventClearAttachments
eventNameFromText "paste-clipboard" = Just EventPasteClipboard
eventNameFromText "export-session" = Just EventExportSession
eventNameFromText "view-session-chrono" = Just EventViewSessionChronological
eventNameFromText "view-session-reverse" = Just EventViewSessionReverse
eventNameFromText "clear-queued" = Just EventClearQueuedMessages
eventNameFromText "next-tab" = Just EventCycleTabForward
eventNameFromText "prev-tab" = Just EventCycleTabBackward
eventNameFromText "next-focus" = Just EventCycleFocusForward
eventNameFromText "prev-focus" = Just EventCycleFocusBackward
eventNameFromText "toggle-zoom" = Just EventToggleZoom
eventNameFromText "refresh-tools" = Just EventRefreshTools
eventNameFromText "enter-turn-nav" = Just EventEnterTurnNavigation
eventNameFromText "fork-at-turn" = Just EventForkAtTurn
eventNameFromText "exit-turn-nav" = Just EventExitTurnNavigation
eventNameFromText "navigate-up" = Just EventNavigateUp
eventNameFromText "navigate-down" = Just EventNavigateDown
eventNameFromText "delete-item" = Just EventDeleteItem
eventNameFromText "open-conversation" = Just EventOpenConversation
eventNameFromText "save-buffer" = Just EventSaveBuffer
eventNameFromText "resume-buffer" = Just EventResumeBuffer
eventNameFromText "clear-buffers" = Just EventClearBuffers
eventNameFromText _ = Nothing

instance ToJSON EventName where
    toJSON = Aeson.String . eventNameToText

instance FromJSON EventName where
    parseJSON = Aeson.withText "EventName" $ \t ->
        case eventNameFromText t of
            Just e -> pure e
            Nothing -> fail $ "Unknown event name: " ++ Text.unpack t

-------------------------------------------------------------------------------
-- Key Names
-------------------------------------------------------------------------------

-- | Virtual key name (can be character, special key, or function key).
data KeyName
    = KeyChar Char
    | KeyEnter
    | KeyEscape
    | KeySpace
    | KeyTab
    | KeyBackTab
    | KeyBackspace
    | KeyDelete
    | KeyUp
    | KeyDown
    | KeyLeft
    | KeyRight
    | KeyPageUp
    | KeyPageDown
    | KeyHome
    | KeyEnd
    | -- | Function key F1-F12
      KeyFun Int
    deriving (Show, Eq, Ord)

-- | Convert a KeyName to its JSON representation.
keyNameToText :: KeyName -> Text
keyNameToText (KeyChar c) = Text.pack [c]
keyNameToText KeyEnter = "enter"
keyNameToText KeyEscape = "escape"
keyNameToText KeySpace = "space"
keyNameToText KeyTab = "tab"
keyNameToText KeyBackTab = "backtab"
keyNameToText KeyBackspace = "backspace"
keyNameToText KeyDelete = "delete"
keyNameToText KeyUp = "up"
keyNameToText KeyDown = "down"
keyNameToText KeyLeft = "left"
keyNameToText KeyRight = "right"
keyNameToText KeyPageUp = "pageup"
keyNameToText KeyPageDown = "pagedown"
keyNameToText KeyHome = "home"
keyNameToText KeyEnd = "end"
keyNameToText (KeyFun n) = "f" <> Text.pack (show n)

-- | Parse a KeyName from its JSON representation.
keyNameFromText :: Text -> Maybe KeyName
keyNameFromText t = case Text.toLower $ Text.strip t of
    "enter" -> Just KeyEnter
    "return" -> Just KeyEnter
    "escape" -> Just KeyEscape
    "esc" -> Just KeyEscape
    "space" -> Just KeySpace
    "tab" -> Just KeyTab
    "backtab" -> Just KeyBackTab
    "backspace" -> Just KeyBackspace
    "bs" -> Just KeyBackspace
    "delete" -> Just KeyDelete
    "del" -> Just KeyDelete
    "up" -> Just KeyUp
    "down" -> Just KeyDown
    "left" -> Just KeyLeft
    "right" -> Just KeyRight
    "pageup" -> Just KeyPageUp
    "pgup" -> Just KeyPageUp
    "pagedown" -> Just KeyPageDown
    "pgdown" -> Just KeyPageDown
    "home" -> Just KeyHome
    "end" -> Just KeyEnd
    "f1" -> Just $ KeyFun 1
    "f2" -> Just $ KeyFun 2
    "f3" -> Just $ KeyFun 3
    "f4" -> Just $ KeyFun 4
    "f5" -> Just $ KeyFun 5
    "f6" -> Just $ KeyFun 6
    "f7" -> Just $ KeyFun 7
    "f8" -> Just $ KeyFun 8
    "f9" -> Just $ KeyFun 9
    "f10" -> Just $ KeyFun 10
    "f11" -> Just $ KeyFun 11
    "f12" -> Just $ KeyFun 12
    txt | Text.length txt == 1 -> Just $ KeyChar (Text.head txt)
    _ -> Nothing

instance ToJSON KeyName where
    toJSON = Aeson.String . keyNameToText

instance FromJSON KeyName where
    parseJSON = Aeson.withText "KeyName" $ \t ->
        case keyNameFromText t of
            Just k -> pure k
            Nothing -> fail $ "Unknown key name: " ++ Text.unpack t

-------------------------------------------------------------------------------
-- Modifiers
-------------------------------------------------------------------------------

-- | Modifier keys for key bindings.
data Modifiers = Modifiers
    { modCtrl :: Bool
    , modMeta :: Bool
    , modShift :: Bool
    }
    deriving (Show, Eq, Ord)

-- | No modifiers pressed.
noModifiers :: Modifiers
noModifiers = Modifiers False False False

-- | Ctrl modifier only.
ctrlModifier :: Modifiers
ctrlModifier = Modifiers True False False

-- | Meta/Alt modifier only.
metaModifier :: Modifiers
metaModifier = Modifiers False True False

-- | Shift modifier only.
shiftModifier :: Modifiers
shiftModifier = Modifiers False False True

instance ToJSON Modifiers where
    toJSON mods =
        Aeson.object $
            ["ctrl" .= modCtrl mods | modCtrl mods]
                ++ ["meta" .= modMeta mods | modMeta mods]
                ++ ["shift" .= modShift mods | modShift mods]

instance FromJSON Modifiers where
    parseJSON = Aeson.withObject "Modifiers" $ \v ->
        Modifiers
            <$> v .:? "ctrl" .!= False
            <*> v .:? "meta" .!= False
            <*> v .:? "shift" .!= False

-------------------------------------------------------------------------------
-- Key Bindings
-------------------------------------------------------------------------------

-- | A single key binding with key and modifiers.
data KeyBinding = KeyBinding
    { bindingKey :: KeyName
    , bindingModifiers :: Modifiers
    }
    deriving (Show, Eq, Ord)

instance ToJSON KeyBinding where
    toJSON binding =
        Aeson.object
            [ "key" .= bindingKey binding
            , "modifiers" .= bindingModifiers binding
            ]

instance FromJSON KeyBinding where
    parseJSON = Aeson.withObject "KeyBinding" $ \v ->
        KeyBinding
            <$> v .: "key"
            <*> v .:? "modifiers" .!= noModifiers

-------------------------------------------------------------------------------
-- Key Mapping
-------------------------------------------------------------------------------

{- | Complete key mapping from events to key bindings.
Multiple bindings per event allow alternatives (e.g., Esc and Ctrl+Q for quit).
-}
newtype KeyMapping = KeyMapping
    { keyMappingBindings :: Map EventName [KeyBinding]
    }
    deriving (Show, Eq)

instance Default KeyMapping where
    def = defaultKeyMapping

-- | Convert key mapping to JSON (as a flat object with event names as keys).
instance ToJSON KeyMapping where
    toJSON (KeyMapping bindings) =
        Aeson.object
            [ Aeson.Key.fromText (eventNameToText event) .= bindings'
            | (event, bindings') <- Map.toList bindings
            , not (null bindings')
            ]

-- | Parse key mapping from JSON (flat object with event names as keys).
instance FromJSON KeyMapping where
    parseJSON = Aeson.withObject "KeyMapping" $ \v -> do
        let pairs = Aeson.KeyMap.toList v
        bindings <- traverse parseEntry pairs
        pure $ KeyMapping $ Map.fromList bindings
      where
        parseEntry (key, val) = do
            event <- case eventNameFromText (Aeson.Key.toText key) of
                Just e -> pure e
                Nothing -> fail $ "Unknown event name: " ++ show key
            bindings <- Aeson.parseJSON val
            pure (event, bindings)

-- | Default key mapping matching current hardcoded behavior.
defaultKeyMapping :: KeyMapping
defaultKeyMapping =
    KeyMapping $
        Map.fromList
            [ (EventQuit, [KeyBinding (KeyChar 'q') ctrlModifier])
            , (EventSendMessage, [KeyBinding KeyEnter metaModifier])
            , (EventNewConversation, [KeyBinding (KeyChar 'n') ctrlModifier])
            , (EventContinueSession, [KeyBinding (KeyChar 'c') ctrlModifier])
            , (EventTogglePause, [KeyBinding (KeyChar 'e') ctrlModifier])
            , (EventAttachFile, [KeyBinding (KeyChar 'f') ctrlModifier])
            , (EventClearAttachments, [KeyBinding (KeyChar 'F') (Modifiers True False True)])
            , (EventPasteClipboard, [KeyBinding (KeyChar 'v') ctrlModifier])
            , (EventExportSession, [KeyBinding (KeyChar 'p') ctrlModifier])
            , (EventViewSessionChronological, [KeyBinding (KeyChar 't') ctrlModifier])
            , (EventViewSessionReverse, [KeyBinding (KeyChar 'r') ctrlModifier])
            , (EventClearQueuedMessages, [KeyBinding (KeyChar 'd') ctrlModifier])
            , (EventCycleTabForward, [KeyBinding (KeyChar ']') ctrlModifier])
            , (EventCycleTabBackward, [KeyBinding (KeyChar '[') ctrlModifier])
            , (EventCycleFocusForward, [KeyBinding KeyTab noModifiers])
            , (EventCycleFocusBackward, [KeyBinding KeyBackTab noModifiers])
            , (EventToggleZoom, [KeyBinding (KeyChar 'z') ctrlModifier])
            , (EventRefreshTools, [KeyBinding (KeyFun 5) noModifiers])
            , (EventEnterTurnNavigation, [KeyBinding KeyEnter noModifiers])
            , (EventForkAtTurn, [KeyBinding (KeyChar 'f') noModifiers, KeyBinding (KeyChar 'F') noModifiers])
            , (EventExitTurnNavigation, [KeyBinding KeyEnter noModifiers, KeyBinding KeyEscape noModifiers])
            , (EventNavigateUp, [KeyBinding KeyUp noModifiers])
            , (EventNavigateDown, [KeyBinding KeyDown noModifiers])
            , (EventDeleteItem, [KeyBinding KeyDelete noModifiers, KeyBinding KeyBackspace noModifiers])
            , (EventOpenConversation, [KeyBinding KeyEnter noModifiers])
            , (EventSaveBuffer, [KeyBinding (KeyChar 'k') ctrlModifier])
            , (EventResumeBuffer, [KeyBinding KeyEnter noModifiers])
            , (EventClearBuffers, [KeyBinding (KeyChar 'K') (Modifiers True False True)])
            ]

-------------------------------------------------------------------------------
-- Combined TUI User Configuration
-------------------------------------------------------------------------------

{- | Combined TUI user configuration containing both key mappings and input settings.

This is the top-level configuration type that users can customize via a JSON file.
It includes both keyboard shortcuts (KeyMapping) and message composer behavior
(InputConfig).

Example JSON structure:

@
{
  "keymap": {
    "quit": [{"key": "q", "modifiers": {"ctrl": true}}],
    "send-message": [{"key": "enter", "modifiers": {"meta": true}}]
  },
  "input": {
    "send_trigger": "triple_newline",
    "show_send_indicator": true
  }
}
@
-}
data TUIUserConfig = TUIUserConfig
    { userConfigKeymap :: KeyMapping
    -- ^ Key mapping for keyboard shortcuts
    , userConfigInput :: InputConfig
    -- ^ Input configuration for message composer
    }
    deriving (Show, Eq)

-- | Default TUI user configuration.
defaultTUIUserConfig :: TUIUserConfig
defaultTUIUserConfig =
    TUIUserConfig
        { userConfigKeymap = defaultKeyMapping
        , userConfigInput = defaultInputConfig
        }

instance Default TUIUserConfig where
    def = defaultTUIUserConfig

instance ToJSON TUIUserConfig where
    toJSON config =
        Aeson.object
            [ "keymap" .= userConfigKeymap config
            , "input" .= userConfigInput config
            ]

instance FromJSON TUIUserConfig where
    parseJSON = Aeson.withObject "TUIUserConfig" $ \v ->
        TUIUserConfig
            <$> v .:? "keymap" .!= defaultKeyMapping
            <*> v .:? "input" .!= defaultInputConfig

-------------------------------------------------------------------------------
-- Matching
-------------------------------------------------------------------------------

-- | Check if a Vty event matches any binding for an event name.
matchesEvent :: KeyMapping -> EventName -> Vty.Event -> Bool
matchesEvent (KeyMapping bindings) eventName vtyEvent =
    case Map.lookup eventName bindings of
        Nothing -> False
        Just keyBindings -> any (matchesBinding vtyEvent) keyBindings

-- | Check if a Vty event matches a specific key binding.
matchesBinding :: Vty.Event -> KeyBinding -> Bool
matchesBinding (Vty.EvKey vtyKey vtyMods) binding =
    keyMatches && modsMatch
  where
    keyMatches = vtyKey == keyNameToVtyKey (bindingKey binding)
    modsMatch = vtyMods == modifiersToVtyMods (bindingModifiers binding)
matchesBinding _ _ = False

-- | Convert our KeyName to a Vty Key.
keyNameToVtyKey :: KeyName -> Vty.Key
keyNameToVtyKey (KeyChar c) = Vty.KChar c
keyNameToVtyKey KeyEnter = Vty.KEnter
keyNameToVtyKey KeyEscape = Vty.KEsc
keyNameToVtyKey KeySpace = Vty.KChar ' '
keyNameToVtyKey KeyTab = Vty.KChar '\t'
keyNameToVtyKey KeyBackTab = Vty.KBackTab
keyNameToVtyKey KeyBackspace = Vty.KBS
keyNameToVtyKey KeyDelete = Vty.KDel
keyNameToVtyKey KeyUp = Vty.KUp
keyNameToVtyKey KeyDown = Vty.KDown
keyNameToVtyKey KeyLeft = Vty.KLeft
keyNameToVtyKey KeyRight = Vty.KRight
keyNameToVtyKey KeyPageUp = Vty.KPageUp
keyNameToVtyKey KeyPageDown = Vty.KPageDown
keyNameToVtyKey KeyHome = Vty.KHome
keyNameToVtyKey KeyEnd = Vty.KEnd
keyNameToVtyKey (KeyFun n) = Vty.KFun n

-- | Convert our Modifiers to Vty modifiers.
modifiersToVtyMods :: Modifiers -> [Vty.Modifier]
modifiersToVtyMods mods =
    [Vty.MCtrl | modCtrl mods]
        ++ [Vty.MMeta | modMeta mods]
        ++ [Vty.MShift | modShift mods]

-- | Convert a KeyBinding to a Vty Event (for display/help).
keyBindingToVtyEvent :: KeyBinding -> Vty.Event
keyBindingToVtyEvent binding =
    Vty.EvKey
        (keyNameToVtyKey $ bindingKey binding)
        (modifiersToVtyMods $ bindingModifiers binding)

-------------------------------------------------------------------------------
-- Help Text
-------------------------------------------------------------------------------

-- | Generate help text from key mapping.
generateHelpContent :: KeyMapping -> [Text]
generateHelpContent keymap =
    [ "Keyboard Shortcuts:"
    , ""
    , "Navigation:"
    , "  " <> formatBindings EventCycleFocusForward <> " - Cycle focus forward through widgets"
    , "  " <> formatBindings EventCycleFocusBackward <> " - Cycle focus backward through widgets"
    , "  " <> formatBindings EventToggleZoom <> " - Toggle zoom mode (based on current tab)"
    , ""
    , "Zoom Mode (Ctrl+Z):"
    , "  Agents  tab  - Zooms the agent description panel"
    , "  Chats   tab  - Zooms the conversation panel"
    , "  History tab  - Zooms the session panel"
    , "  Help    tab  - Zooms the help content"
    , ""
    , "Tabs:"
    , "  " <> formatBindings EventCycleTabBackward <> " - Switch to previous tab"
    , "  " <> formatBindings EventCycleTabForward <> " - Switch to next tab"
    , "  " <> formatBindings EventOpenConversation <> " - Open selected conversation (from Conversations list)"
    , ""
    , "Conversations:"
    , "  " <> formatBindings EventNewConversation <> " - Start new conversation with selected agent"
    , "  " <> formatBindings EventContinueSession <> " - Continue restored session"
    , "  " <> formatBindings EventSendMessage <> " - Send message"
    , "  " <> formatBindings EventTogglePause <> " - Pause/unpause conversation"
    , ""
    , "Attachments:"
    , "  " <> formatBindings EventAttachFile <> " - Attach file (opens file browser)"
    , "  " <> formatBindings EventClearAttachments <> " - Clear all attachments"
    , "  " <> formatBindings EventPasteClipboard <> " - Paste from clipboard (images/files)"
    , "  " <> formatBindings EventDeleteItem <> " - Remove selected attachment"
    , "  " <> formatBindings EventNavigateUp <> "/" <> formatBindings EventNavigateDown <> " - Select attachment"
    , ""
    , "Buffers:"
    , "  " <> formatBindings EventSaveBuffer <> " - Save current message as buffer"
    , "  " <> formatBindings EventResumeBuffer <> " - Resume editing selected buffer"
    , "  " <> formatBindings EventClearBuffers <> " - Clear all buffers"
    , ""
    , "Queue Management (when paused):"
    , "  " <> formatBindings EventClearQueuedMessages <> " - Clear all queued messages"
    , "  " <> formatBindings EventDeleteItem <> " - Delete selected queued message"
    , "  " <> formatBindings EventNavigateUp <> "/" <> formatBindings EventNavigateDown <> " - Select queued message"
    , ""
    , "Session Navigation & Forking:"
    , "  " <> formatBindings EventEnterTurnNavigation <> " - Enter turn navigation mode (when on conversation)"
    , "  " <> formatBindings EventNavigateUp <> "/" <> formatBindings EventNavigateDown <> " - Navigate between turns (in navigation mode)"
    , "  " <> formatBindings EventForkAtTurn <> " - Fork conversation at selected turn"
    , "  " <> formatBindings EventExitTurnNavigation <> " - Exit turn navigation mode"
    , ""
    , "Session Export:"
    , "  " <> formatBindings EventExportSession <> " - Export session to markdown file"
    , "  " <> formatBindings EventViewSessionChronological <> " - View session in external viewer (chronological)"
    , "  " <> formatBindings EventViewSessionReverse <> " - View session in external viewer (reverse)"
    , ""
    , "Other:"
    , "  " <> formatBindings EventRefreshTools <> " - Refresh tools for selected agent"
    , "  " <> formatBindings EventQuit <> " - Quit application (press twice to confirm)"
    ]
  where
    formatBindings :: EventName -> Text
    formatBindings event = Text.intercalate ", " $ map formatBinding (getBindings event)

    getBindings :: EventName -> [KeyBinding]
    getBindings event = fromMaybe [] $ Map.lookup event (keyMappingBindings keymap)

    formatBinding :: KeyBinding -> Text
    formatBinding binding = case binding of
        KeyBinding (KeyChar c) mods -> formatModifiers mods <> Text.pack [c]
        KeyBinding key mods -> formatModifiers mods <> keyNameToText key

    formatModifiers :: Modifiers -> Text
    formatModifiers (Modifiers False False False) = ""
    formatModifiers (Modifiers True False False) = "Ctrl+"
    formatModifiers (Modifiers False True False) = "Meta+"
    formatModifiers (Modifiers False False True) = "Shift+"
    formatModifiers (Modifiers True True False) = "Ctrl+Meta+"
    formatModifiers (Modifiers True False True) = "Ctrl+Shift+"
    formatModifiers (Modifiers False True True) = "Meta+Shift+"
    formatModifiers (Modifiers True True True) = "Ctrl+Meta+Shift+"

-------------------------------------------------------------------------------
-- Loading
-------------------------------------------------------------------------------

{- | Load keymap from file, falling back to default on error.
If the file doesn't exist or is invalid, prints a warning and returns the default.

This function now loads the complete TUI user configuration including both
key mappings and input configuration.
-}
loadKeymapFromFile :: FilePath -> IO TUIUserConfig
loadKeymapFromFile path = do
    exists <- doesFileExist path
    if not exists
        then do
            hPutStrLn stderr $ "Warning: Keymap file not found: " ++ path
            hPutStrLn stderr "Using default key bindings and input configuration."
            pure defaultTUIUserConfig
        else do
            result <- Aeson.eitherDecodeFileStrict' path
            case result of
                Left err -> do
                    hPutStrLn stderr $ "Warning: Failed to parse keymap file " ++ path ++ ": " ++ err
                    hPutStrLn stderr "Using default key bindings and input configuration."
                    pure defaultTUIUserConfig
                Right config -> do
                    hPutStrLn stderr $ "Loaded custom keymap from: " ++ path
                    pure $ mergeWithDefault config

{- | Merge a partial user config with the default.
Events not specified in the custom keymap will use default bindings.
Input configuration fields not specified will use defaults.
-}
mergeWithDefault :: TUIUserConfig -> TUIUserConfig
mergeWithDefault custom =
    TUIUserConfig
        { userConfigKeymap = mergeKeymap (userConfigKeymap custom) defaultKeyMapping
        , userConfigInput = mergeInputConfig (userConfigInput custom) defaultInputConfig
        }
  where
    mergeKeymap (KeyMapping customBindings) (KeyMapping defaultBindings) =
        KeyMapping $ Map.unionWith (\customBindings' _ -> customBindings') customBindings defaultBindings

    -- InputConfig only has simple fields, so we just use the custom one if it's different from default
    -- The FromJSON instance already provides defaults for missing fields via .!=
    mergeInputConfig customInput _defaultInput = customInput
