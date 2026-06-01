{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main layout rendering functions for the TUI application.
module System.Agents.TUI.Render.Layout where

import Brick
import Brick.Focus (focusGetCurrent)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.List (listSelectedElement)
import Control.Lens ((^.))
import Data.List (intersperse)

import System.Agents.TUI.Render.Attributes
import System.Agents.TUI.Render.Conversation (
    render_conversationList,
    render_conversationView,
    render_sessionView,
 )
import System.Agents.TUI.Render.Widgets (
    render_agentDetail,
    render_agentList,
    render_attachmentList,
    render_buffer_manager,
    render_messageEditor,
    render_messageEditorWithAttachments,
    render_queued_messages_manager,
    render_sessionList,
 )
import System.Agents.TUI.Types

-- | Render the main UI based on current state.
render_ui :: TuiState -> Widget N
render_ui st = render_mainLayout st

-- | Main layout with tab bar, sidebar, and content area.
render_mainLayout :: TuiState -> Widget N
render_mainLayout st =
    vBox
        [ renderTabBar (st ^. tuiUI . currentTab)
        , hBox
            [ render_sidebar st
            , render_contentArea st
            ]
        , render_statusBar (st ^. tuiUI . statusMessage)
        ]

-- | Render the tab bar with all tabs, highlighting the active one.
renderTabBar :: Tab -> Widget N
renderTabBar activeTab =
    let tabs = [AgentsTab, ChatsTab, HistoryTab, HelpTab]
        renderTab tab =
            let tabName = case tab of
                    AgentsTab -> " Agents "
                    ChatsTab -> " Chats "
                    HistoryTab -> " History "
                    HelpTab -> " Help "
                tabAttr = if tab == activeTab then activeTabAttr else inactiveTabAttr
             in withAttr tabAttr $ txt tabName
        tabWidgets = map renderTab tabs
        separator = withAttr inactiveTabAttr $ txt "│"
     in hBorder <=> hBox (intersperse separator tabWidgets)

-- | Sidebar with agent and conversation lists.
render_sidebar :: TuiState -> Widget N
render_sidebar st =
    hLimit 35 $
        vBox
            [ render_agentList st
            , render_conversationList st
            , render_sessionList st
            ]

-- | Content area showing content based on the current tab.
render_contentArea :: TuiState -> Widget N
render_contentArea st =
    case st ^. tuiUI . currentTab of
        AgentsTab -> renderAgentsTab st
        ChatsTab -> renderChatsTab st
        HistoryTab -> renderHistoryTab st
        HelpTab -> renderHelpTab st

-- | Render the Agents tab content.
renderAgentsTab :: TuiState -> Widget N
renderAgentsTab st =
    case focusGetCurrent (st ^. tuiUI . uiFocusRing) of
        Just AgentListWidget -> render_agentDetail st
        Just AgentInfoWidget -> render_agentDetail st
        _ -> render_agentDetail st

-- | Render the Chats tab content.
renderChatsTab :: TuiState -> Widget N
renderChatsTab st =
    case focusGetCurrent (st ^. tuiUI . uiFocusRing) of
        Just MessageEditorWidget -> render_conversationArea st
        Just ConversationListWidget -> render_conversationArea st
        Just ConversationViewWidget -> render_conversationArea st
        Just AttachmentListWidget -> render_conversationArea st
        Just BufferListWidget -> render_conversationArea st
        _ -> render_conversationArea st

-- | Render the History tab content (sessions view).
renderHistoryTab :: TuiState -> Widget N
renderHistoryTab st =
    case focusGetCurrent (st ^. tuiUI . uiFocusRing) of
        Just SessionsListWidget -> render_sessionArea st
        Just SessionViewWidget -> render_sessionArea st
        _ -> render_sessionArea st

-- | Render the Help tab content.
renderHelpTab :: TuiState -> Widget N
renderHelpTab st =
    borderWithLabel (txt " Help ") $
        viewport AgentInfoWidget Both $
            vBox $
                map txt (st ^. tuiUI . helpContent)

-- | Conversation area with message input and conversation history.
render_conversationArea :: TuiState -> Widget N
render_conversationArea st =
    case listSelectedElement (st ^. tuiUI . conversationList) of
        Nothing ->
            vBox
                [ txt "Select or create a conversation (Ctrl+n)"
                , render_messageEditor st
                ]
        Just (_, conv) ->
            vBox
                [ render_messageEditorWithAttachments st conv
                , render_attachmentList st conv
                , render_buffer_manager st
                , render_queued_messages_manager st conv
                , render_conversationView st
                , render_shortcutsHelp
                ]

-- | Render shortcuts help bar.
render_shortcutsHelp :: Widget N
render_shortcutsHelp =
    withAttr (attrName "help") $
        hBox
            [ txt "Ctrl+E: pause | Ctrl+p: export md | Ctrl+[r|t]: view md | Ctrl+F: attach file | Ctrl+K: save buffer"
            ]

-- | Render status bar if there's a message.
render_statusBar :: Maybe StatusMessage -> Widget N
render_statusBar Nothing = emptyWidget
render_statusBar (Just msg) =
    withAttr (statusAttr msg.statusSeverity) $
        txt $
            " " <> statusText msg

-- | Get the appropriate attribute for a status severity level.
statusAttr :: StatusSeverity -> AttrName
statusAttr StatusInfo = statusInfoAttr
statusAttr StatusWarning = statusWarningAttr
statusAttr StatusError = statusErrorAttr

-- | Conversation area with message input and conversation history.
render_sessionArea :: TuiState -> Widget N
render_sessionArea st =
    case listSelectedElement (st ^. tuiUI . sessionList) of
        Nothing ->
            vBox
                [ txt "Select or resume a session (Ctrl+c)"
                ]
        Just _ ->
            hBox
                [ render_sessionView st
                ]

