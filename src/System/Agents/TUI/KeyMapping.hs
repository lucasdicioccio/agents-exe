{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Key mapping configuration for TUI event bindings.
-}
module System.Agents.TUI.KeyMapping (
    KeyMapping (..),
    KeyBinding (..),
    Modifiers (..),
    KeyName (..),
    EventName (..),
    defaultKeyMapping,
    lookupBinding,
    resolveKeyEvent,
    keyBindingToVtyEvent,
) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Graphics.Vty as Vty

data EventName
    = EventCycleTabForward
    | EventCycleTabBackward
    | EventCycleFocusForward
    | EventCycleFocusBackward
    | EventToggleZoom
    | EventEnterTurnNavigation
    | EventExitTurnNavigation
    | EventNavigateUp
    | EventNavigateDown
    | EventForkAtTurn
    | EventNewConversation
    | EventContinueSession
    | EventSendMessage
    | EventTogglePause
    | EventAttachFile
    | EventClearAttachments
    | EventPasteClipboard
    | EventRemoveAttachment
    | EventSelectAttachmentUp
    | EventSelectAttachmentDown
    | EventClearQueuedMessages
    | EventDeleteQueuedMessage
    | EventSelectQueuedUp
    | EventSelectQueuedDown
    | EventExportSession
    | EventViewSessionChronological
    | EventViewSessionReverse
    | EventRefreshTools
    | EventQuit
    | EventScrollUp
    | EventScrollDown
    | EventScrollLeft
    | EventScrollRight
    | EventPageUp
    | EventPageDown
    deriving (Show, Eq, Ord, Generic)

instance Aeson.FromJSON EventName where
    parseJSON = Aeson.withText "EventName" $ \t ->
        case Text.toLower t of
            "cycle-tab-forward" -> pure EventCycleTabForward
            "cycle-tab-backward" -> pure EventCycleTabBackward
            "cycle-focus-forward" -> pure EventCycleFocusForward
            "cycle-focus-backward" -> pure EventCycleFocusBackward
            "toggle-zoom" -> pure EventToggleZoom
            "enter-turn-navigation" -> pure EventEnterTurnNavigation
            "exit-turn-navigation" -> pure EventExitTurnNavigation
            "navigate-up" -> pure EventNavigateUp
            "navigate-down" -> pure EventNavigateDown
            "fork-at-turn" -> pure EventForkAtTurn
            "new-conversation" -> pure EventNewConversation
            "continue-session" -> pure EventContinueSession
            "send-message" -> pure EventSendMessage
            "toggle-pause" -> pure EventTogglePause
            "attach-file" -> pure EventAttachFile
            "clear-attachments" -> pure EventClearAttachments
            "paste-clipboard" -> pure EventPasteClipboard
            "remove-attachment" -> pure EventRemoveAttachment
            "select-attachment-up" -> pure EventSelectAttachmentUp
            "select-attachment-down" -> pure EventSelectAttachmentDown
            "clear-queued-messages" -> pure EventClearQueuedMessages
            "delete-queued-message" -> pure EventDeleteQueuedMessage
            "select-queued-up" -> pure EventSelectQueuedUp
            "select-queued-down" -> pure EventSelectQueuedDown
            "export-session" -> pure EventExportSession
            "view-session-chronological" -> pure EventViewSessionChronological
            "view-session-reverse" -> pure EventViewSessionReverse
            "refresh-tools" -> pure EventRefreshTools
            "quit" -> pure EventQuit
            "scroll-up" -> pure EventScrollUp
            "scroll-down" -> pure EventScrollDown
            "scroll-left" -> pure EventScrollLeft
            "scroll-right" -> pure EventScrollRight
            "page-up" -> pure EventPageUp
            "page-down" -> pure EventPageDown
            _ -> fail $ "Unknown event name: " ++ Text.unpack t

instance Aeson.ToJSON EventName where
    toJSON ev = Aeson.String $ case ev of
        EventCycleTabForward -> "cycle-tab-forward"
        EventCycleTabBackward -> "cycle-tab-backward"
        EventCycleFocusForward -> "cycle-focus-forward"
        EventCycleFocusBackward -> "cycle-focus-backward"
        EventToggleZoom -> "toggle-zoom"
        EventEnterTurnNavigation -> "enter-turn-navigation"
        EventExitTurnNavigation -> "exit-turn-navigation"
        EventNavigateUp -> "navigate-up"
        EventNavigateDown -> "navigate-down"
        EventForkAtTurn -> "fork-at-turn"
        EventNewConversation -> "new-conversation"
        EventContinueSession -> "continue-session"
        EventSendMessage -> "send-message"
        EventTogglePause -> "toggle-pause"
        EventAttachFile -> "attach-file"
        EventClearAttachments -> "clear-attachments"
        EventPasteClipboard -> "paste-clipboard"
        EventRemoveAttachment -> "remove-attachment"
        EventSelectAttachmentUp -> "select-attachment-up"
        EventSelectAttachmentDown -> "select-attachment-down"
        EventClearQueuedMessages -> "clear-queued-messages"
        EventDeleteQueuedMessage -> "delete-queued-message"
        EventSelectQueuedUp -> "select-queued-up"
        EventSelectQueuedDown -> "select-queued-down"
        EventExportSession -> "export-session"
        EventViewSessionChronological -> "view-session-chronological"
        EventViewSessionReverse -> "view-session-reverse"
        EventRefreshTools -> "refresh-tools"
        EventQuit -> "quit"
        EventScrollUp -> "scroll-up"
        EventScrollDown -> "scroll-down"
        EventScrollLeft -> "scroll-left"
        EventScrollRight -> "scroll-right"
        EventPageUp -> "page-up"
        EventPageDown -> "page-down"

-------------------------------------------------------------------------------
-- Key Names
-------------------------------------------------------------------------------

data KeyName
    = KeyChar Char
    | KeyEnter
    | KeyEsc
    | KeyBackspace
    | KeyDelete
    | KeyTab
    | KeyBackTab
    | KeyUp
    | KeyDown
    | KeyLeft
    | KeyRight
    | KeyPageUp
    | KeyPageDown
    | KeyHome
    | KeyEnd
    | KeyFun Int
    deriving (Show, Eq, Ord, Generic)

instance Aeson.FromJSON KeyName where
    parseJSON = Aeson.withText "KeyName" $ \t ->
        case Text.toLower t of
            "enter" -> pure KeyEnter
            "return" -> pure KeyEnter
            "esc" -> pure KeyEsc
            "escape" -> pure KeyEsc
            "backspace" -> pure KeyBackspace
            "bs" -> pure KeyBackspace
            "delete" -> pure KeyDelete
            "del" -> pure KeyDelete
            "tab" -> pure KeyTab
            "backtab" -> pure KeyBackTab
            "shift-tab" -> pure KeyBackTab
            "up" -> pure KeyUp
            "down" -> pure KeyDown
            "left" -> pure KeyLeft
            "right" -> pure KeyRight
            "pageup" -> pure KeyPageUp
            "page-up" -> pure KeyPageUp
            "pagedown" -> pure KeyPageDown
            "page-down" -> pure KeyPageDown
            "home" -> pure KeyHome
            "end" -> pure KeyEnd
            _ -> case Text.stripPrefix "f" t of
                Just numStr -> case Text.unpack numStr of
                    [n] | n >= '1' && n <= '9' -> pure $ KeyFun (read [n])
                    "10" -> pure $ KeyFun 10
                    "11" -> pure $ KeyFun 11
                    "12" -> pure $ KeyFun 12
                    _ -> parseChar t
                Nothing -> parseChar t
      where
        parseChar :: Text -> Parser KeyName
        parseChar txt
            | Text.length txt == 1 = pure $ KeyChar (Text.head txt)
            | otherwise = fail $ "Unknown key name: " ++ Text.unpack txt

instance Aeson.ToJSON KeyName where
    toJSON k = Aeson.String $ case k of
        KeyChar c -> Text.singleton c
        KeyEnter -> "enter"
        KeyEsc -> "esc"
        KeyBackspace -> "backspace"
        KeyDelete -> "delete"
        KeyTab -> "tab"
        KeyBackTab -> "backtab"
        KeyUp -> "up"
        KeyDown -> "down"
        KeyLeft -> "left"
        KeyRight -> "right"
        KeyPageUp -> "pageup"
        KeyPageDown -> "pagedown"
        KeyHome -> "home"
        KeyEnd -> "end"
        KeyFun n -> "f" <> Text.pack (show n)

-------------------------------------------------------------------------------
-- Modifiers
-------------------------------------------------------------------------------

data Modifiers = Modifiers
    { modCtrl :: Bool
    , modMeta :: Bool
    , modShift :: Bool
    }
    deriving (Show, Eq, Ord, Generic)

instance Aeson.FromJSON Modifiers where
    parseJSON = Aeson.withObject "Modifiers" $ \v ->
        Modifiers
            <$> v Aeson..:? "ctrl" Aeson..!= False
            <*> v Aeson..:? "meta" Aeson..!= False
            <*> v Aeson..:? "shift" Aeson..!= False

instance Aeson.ToJSON Modifiers where
    toJSON (Modifiers ctrl meta shift) =
        Aeson.object $ concat
            [ ["ctrl" Aeson..= True | ctrl]
            , ["meta" Aeson..= True | meta]
            , ["shift" Aeson..= True | shift]
            ]

defaultModifiers :: Modifiers
defaultModifiers = Modifiers False False False

-------------------------------------------------------------------------------
-- Key Binding
-------------------------------------------------------------------------------

data KeyBinding = KeyBinding
    { bindingKey :: KeyName
    , bindingModifiers :: Modifiers
    }
    deriving (Show, Eq, Ord, Generic)

instance Aeson.FromJSON KeyBinding where
    parseJSON = Aeson.withObject "KeyBinding" $ \v ->
        KeyBinding
            <$> v Aeson..: "key"
            <*> v Aeson..:? "modifiers" Aeson..!= defaultModifiers

instance Aeson.ToJSON KeyBinding where
    toJSON (KeyBinding key mods) =
        Aeson.object $ ["key" Aeson..= key] ++
            ["modifiers" Aeson..= mods | mods /= defaultModifiers]

-------------------------------------------------------------------------------
-- Key Mapping
-------------------------------------------------------------------------------

newtype KeyMapping = KeyMapping
    { keyBindings :: Map EventName KeyBinding
    }
    deriving (Show, Eq, Generic)

instance Aeson.FromJSON KeyMapping where
    parseJSON = Aeson.withObject "KeyMapping" $ \v -> do
        bindingsList <- v Aeson..:? "bindings" Aeson..!= ([] :: [(EventName, KeyBinding)])
        pure $ KeyMapping $ Map.fromList bindingsList

instance Aeson.ToJSON KeyMapping where
    toJSON (KeyMapping bindings) =
        Aeson.object ["bindings" Aeson..= Map.toList bindings]

-------------------------------------------------------------------------------
-- Default Key Mapping
-------------------------------------------------------------------------------

defaultKeyMapping :: KeyMapping
defaultKeyMapping = KeyMapping $ Map.fromList
    [ (EventCycleTabForward, KeyBinding (KeyChar ']') (Modifiers True False False))
    , (EventCycleTabBackward, KeyBinding KeyEsc (Modifiers True False False))
    , (EventCycleFocusForward, KeyBinding KeyTab defaultModifiers)
    , (EventCycleFocusBackward, KeyBinding KeyBackTab defaultModifiers)
    , (EventToggleZoom, KeyBinding (KeyChar 'z') (Modifiers True False False))
    , (EventEnterTurnNavigation, KeyBinding KeyEnter defaultModifiers)
    , (EventExitTurnNavigation, KeyBinding KeyEnter defaultModifiers)
    , (EventNavigateUp, KeyBinding KeyUp defaultModifiers)
    , (EventNavigateDown, KeyBinding KeyDown defaultModifiers)
    , (EventForkAtTurn, KeyBinding (KeyChar 'f') defaultModifiers)
    , (EventNewConversation, KeyBinding (KeyChar 'n') (Modifiers True False False))
    , (EventContinueSession, KeyBinding (KeyChar 'c') (Modifiers True False False))
    , (EventSendMessage, KeyBinding KeyEnter (Modifiers False True False))
    , (EventTogglePause, KeyBinding (KeyChar 'e') (Modifiers True False False))
    , (EventAttachFile, KeyBinding (KeyChar 'f') (Modifiers True False False))
    , (EventClearAttachments, KeyBinding (KeyChar 'F') (Modifiers True False True))
    , (EventPasteClipboard, KeyBinding (KeyChar 'v') (Modifiers True False False))
    , (EventRemoveAttachment, KeyBinding KeyDelete defaultModifiers)
    , (EventSelectAttachmentUp, KeyBinding KeyUp defaultModifiers)
    , (EventSelectAttachmentDown, KeyBinding KeyDown defaultModifiers)
    , (EventClearQueuedMessages, KeyBinding (KeyChar 'd') (Modifiers True False False))
    , (EventDeleteQueuedMessage, KeyBinding KeyDelete defaultModifiers)
    , (EventSelectQueuedUp, KeyBinding KeyUp defaultModifiers)
    , (EventSelectQueuedDown, KeyBinding KeyDown defaultModifiers)
    , (EventExportSession, KeyBinding (KeyChar 'p') (Modifiers True False False))
    , (EventViewSessionChronological, KeyBinding (KeyChar 't') (Modifiers True False False))
    , (EventViewSessionReverse, KeyBinding (KeyChar 'r') (Modifiers True False False))
    , (EventRefreshTools, KeyBinding (KeyFun 5) defaultModifiers)
    , (EventQuit, KeyBinding (KeyChar 'q') (Modifiers True False False))
    , (EventScrollUp, KeyBinding KeyUp defaultModifiers)
    , (EventScrollDown, KeyBinding KeyDown defaultModifiers)
    , (EventScrollLeft, KeyBinding KeyLeft defaultModifiers)
    , (EventScrollRight, KeyBinding KeyRight defaultModifiers)
    , (EventPageUp, KeyBinding KeyPageUp defaultModifiers)
    , (EventPageDown, KeyBinding KeyPageDown defaultModifiers)
    ]

-------------------------------------------------------------------------------
-- Key Resolution
-------------------------------------------------------------------------------

lookupBinding :: EventName -> KeyMapping -> Maybe KeyBinding
lookupBinding ev (KeyMapping bindings) = Map.lookup ev bindings

keyNameToVty :: KeyName -> Vty.Key
keyNameToVty (KeyChar c) = Vty.KChar c
keyNameToVty KeyEnter = Vty.KEnter
keyNameToVty KeyEsc = Vty.KEsc
keyNameToVty KeyBackspace = Vty.KBS
keyNameToVty KeyDelete = Vty.KDel
keyNameToVty KeyTab = Vty.KChar '\t'
keyNameToVty KeyBackTab = Vty.KBackTab
keyNameToVty KeyUp = Vty.KUp
keyNameToVty KeyDown = Vty.KDown
keyNameToVty KeyLeft = Vty.KLeft
keyNameToVty KeyRight = Vty.KRight
keyNameToVty KeyPageUp = Vty.KPageUp
keyNameToVty KeyPageDown = Vty.KPageDown
keyNameToVty KeyHome = Vty.KHome
keyNameToVty KeyEnd = Vty.KEnd
keyNameToVty (KeyFun n) = Vty.KFun n

modifiersToVty :: Modifiers -> [Vty.Modifier]
modifiersToVty (Modifiers ctrl meta shift) = concat
    [ [Vty.MCtrl | ctrl]
    , [Vty.MMeta | meta]
    , [Vty.MShift | shift]
    ]

keyBindingToVtyEvent :: KeyBinding -> Vty.Event
keyBindingToVtyEvent (KeyBinding key mods) =
    Vty.EvKey (keyNameToVty key) (modifiersToVty mods)

resolveKeyEvent :: Vty.Event -> KeyMapping -> Maybe EventName
resolveKeyEvent vtyEvent (KeyMapping bindings) =
    Map.lookup vtyEvent reverseMap
  where
    reverseMap = Map.fromList $ map (\(ev, kb) -> (keyBindingToVtyEvent kb, ev)) $ Map.toList bindings

