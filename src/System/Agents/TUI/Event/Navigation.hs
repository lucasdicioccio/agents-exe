{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Navigation event handlers for the TUI.

This module handles turn navigation, focus cycling, tab cycling,
and zoom functionality.
-}
module System.Agents.TUI.Event.Navigation (
    -- * Tab Cycling
    cycleTabForward,
    cycleTabBackward,
    nextTab,
    prevTab,
    
    -- * Focus Management
    cycleFocusForward,
    cycleFocusBackward,
    updateTabFromFocus,
    toggleZoom,
    switchToChatsAndFocusMessage,
    
    -- * Focus Ring Construction (re-exported from Types.State)
    buildFocusRingForTabPreserving,
    tabEntryWidget,
    focusRingElements,
    widgetToTab,
    
    -- * Turn Navigation
    handleTurnNavigationEvent,
    handleForkAtTurn,
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Focus (FocusRing, focusGetCurrent, focusNext, focusPrev, focusSetCurrent)
import Control.Lens ((%=), (.=), (^.), to, use)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text

import Prod.Tracer (Tracer)

import System.Agents.TUI.KeyMapping (
    EventName (..),
    matchesEvent,
 )
import System.Agents.TUI.Types.Core (
    AppEvent (..),
    N,
    StatusSeverity (..),
    Tab (..),
    TurnNavigationState (..),
    WidgetName (..),
    navSelectedTurnIndex,
    navTotalTurns,
 )
import System.Agents.TUI.Types.State (
    TuiState,
    buildFocusRingForTab,
    currentTab,
    eventChan,
    keyMapping,
    quitConfirmationPending,
    tuiUI,
    turnNavigation,
    uiFocusRing,
    zoomed,
 )

-- | Show a status message in the TUI.
showStatus :: StatusSeverity -> Text.Text -> EventM N TuiState ()
showStatus severity text = do
    chan <- use eventChan
    liftIO $ writeBChan chan (AppEvent_ShowStatus severity text)

-- | Reset quit confirmation state.
resetQuitConfirmation :: EventM N TuiState ()
resetQuitConfirmation = do
    tuiUI . quitConfirmationPending .= False

-------------------------------------------------------------------------------
-- Tab Cycling
-------------------------------------------------------------------------------

-- | Get the next tab in the cycle.
nextTab :: Tab -> Tab
nextTab AgentsTab = ChatsTab
nextTab ChatsTab = HistoryTab
nextTab HistoryTab = HelpTab
nextTab HelpTab = AgentsTab

-- | Get the previous tab in the cycle.
prevTab :: Tab -> Tab
prevTab AgentsTab = HelpTab
prevTab ChatsTab = AgentsTab
prevTab HistoryTab = ChatsTab
prevTab HelpTab = HistoryTab

-- | Cycle to the next tab forward.
cycleTabForward :: EventM N TuiState ()
cycleTabForward = do
    current <- use (tuiUI . currentTab)
    let next = nextTab current
    tuiUI . currentTab .= next
    mCurrentFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
    tuiUI . uiFocusRing .= buildFocusRingForTabPreserving next mCurrentFocus

-- | Cycle to the previous tab backward.
cycleTabBackward :: EventM N TuiState ()
cycleTabBackward = do
    current <- use (tuiUI . currentTab)
    let prev = prevTab current
    tuiUI . currentTab .= prev
    mCurrentFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
    tuiUI . uiFocusRing .= buildFocusRingForTabPreserving prev mCurrentFocus

-------------------------------------------------------------------------------
-- Focus Ring Construction
-------------------------------------------------------------------------------

{- | Get the default (entry) widget for a tab.
This is the widget that should receive focus when switching to this tab.
-}
tabEntryWidget :: Tab -> WidgetName
tabEntryWidget AgentsTab = AgentListWidget
tabEntryWidget ChatsTab = ConversationListWidget
tabEntryWidget HistoryTab = SessionsListWidget
tabEntryWidget HelpTab = AgentListWidget

{- | Build a focus ring for a tab, attempting to preserve the current focus if valid.
If the current focus is not in the new tab's focus ring, falls back to the tab's entry widget.
-}
buildFocusRingForTabPreserving :: Tab -> Maybe WidgetName -> FocusRing WidgetName
buildFocusRingForTabPreserving tab mCurrentFocus =
    let newRing = buildFocusRingForTab tab
        validFocus = case mCurrentFocus of
            Just wf | wf `elem` focusRingElements newRing -> Just wf
            _ -> Nothing
        startFocus = case validFocus of
            Just wf -> wf
            Nothing -> tabEntryWidget tab
     in focusSetCurrent startFocus newRing

-- | Get all elements in a focus ring.
focusRingElements :: FocusRing WidgetName -> [WidgetName]
focusRingElements fr =
    go (focusSetCurrent (tabEntryWidget AgentsTab) fr) []
  where
    go ring acc =
        case focusGetCurrent ring of
            Just w | w `elem` acc -> reverse acc
            Just w -> go (focusNext ring) (w : acc)
            Nothing -> reverse acc

-------------------------------------------------------------------------------
-- Navigation Helpers
-------------------------------------------------------------------------------

-- | Switch to the Chats tab and focus the message editor.
switchToChatsAndFocusMessage :: EventM N TuiState ()
switchToChatsAndFocusMessage = do
    tuiUI . currentTab .= ChatsTab
    tuiUI . uiFocusRing .= focusSetCurrent MessageEditorWidget (buildFocusRingForTab ChatsTab)
    tuiUI . zoomed .= False

-------------------------------------------------------------------------------
-- Focus Management
-------------------------------------------------------------------------------

-- | Get the corresponding Tab for a WidgetName.
widgetToTab :: WidgetName -> Maybe Tab
widgetToTab AgentListWidget = Just AgentsTab
widgetToTab ConversationListWidget = Just ChatsTab
widgetToTab SessionsListWidget = Just HistoryTab
widgetToTab _ = Nothing

-- | Update the current tab based on the focused widget.
updateTabFromFocus :: EventM N TuiState ()
updateTabFromFocus = do
    mFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
    case mFocus >>= widgetToTab of
        Just tab -> do
            currentTab' <- use (tuiUI . currentTab)
            when (tab /= currentTab') $ do
                tuiUI . currentTab .= tab
                mCurrentFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
                tuiUI . uiFocusRing .= buildFocusRingForTabPreserving tab mCurrentFocus
        Nothing -> pure ()

-- | Cycle focus forward through widgets.
cycleFocusForward :: EventM N TuiState ()
cycleFocusForward = do
    tuiUI . uiFocusRing %= focusNext
    tuiUI . zoomed .= False
    updateTabFromFocus

-- | Cycle focus backward through widgets.
cycleFocusBackward :: EventM N TuiState ()
cycleFocusBackward = do
    tuiUI . uiFocusRing %= focusPrev
    tuiUI . zoomed .= False
    updateTabFromFocus

-- | Toggle zoom mode for current widget.
toggleZoom :: EventM N TuiState ()
toggleZoom = tuiUI . zoomed %= not

-------------------------------------------------------------------------------
-- Turn Navigation
-------------------------------------------------------------------------------

-- | Handle events when in turn navigation mode.
handleTurnNavigationEvent :: Tracer IO a -> TurnNavigationState -> BrickEvent N AppEvent -> EventM N TuiState ()
handleTurnNavigationEvent _tracer navState ev = do
    keymap <- use keyMapping
    case ev of
        VtyEvent vtyEv
            | matchesEvent keymap EventExitTurnNavigation vtyEv -> do
                tuiUI . turnNavigation .= Nothing
                showStatus StatusInfo "Exited turn navigation"
                resetQuitConfirmation
        VtyEvent vtyEv
            | matchesEvent keymap EventNavigateUp vtyEv -> do
                let currentIdx = navState ^. navSelectedTurnIndex
                    newIdx = max 0 (currentIdx - 1)
                tuiUI . turnNavigation .= Just (navState{_navSelectedTurnIndex = newIdx})
        VtyEvent vtyEv
            | matchesEvent keymap EventNavigateDown vtyEv -> do
                let currentIdx = navState ^. navSelectedTurnIndex
                    maxIdx = (navState ^. navTotalTurns) - 1
                    newIdx = min maxIdx (currentIdx + 1)
                tuiUI . turnNavigation .= Just (navState{_navSelectedTurnIndex = newIdx})
        VtyEvent vtyEv
            | matchesEvent keymap EventForkAtTurn vtyEv -> handleForkAtTurn navState
        _ -> pure ()

-- | Fork a new conversation at the selected turn.
handleForkAtTurn :: TurnNavigationState -> EventM N TuiState ()
handleForkAtTurn _navState = do
    -- This is a placeholder - the actual implementation is in Event.hs
    -- which has access to the Tracer and can call runConversation
    showStatus StatusWarning "Fork not available from Navigation module"

