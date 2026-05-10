{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rendering functions for the TUI application.
--
-- This module re-exports functionality from submodules:
--
-- * "System.Agents.TUI.Render.Attributes" - Attribute definitions
-- * "System.Agents.TUI.Render.Conversation" - Conversation tree and view rendering
-- * "System.Agents.TUI.Render.Dialog" - File attachment dialogs
-- * "System.Agents.TUI.Render.Layout" - Main layout and tab rendering
-- * "System.Agents.TUI.Render.Utils" - Utility rendering functions
-- * "System.Agents.TUI.Render.Widgets" - UI widgets (agent list, editor, etc.)
module System.Agents.TUI.Render (
    -- Main entry point
    tui_appDraw,
    
    -- Re-exported from Attributes
    tui_appAttrMap,
    focusedAttr,
    userMessageAttr,
    llmMessageAttr,
    thinkingAttr,
    statusInfoAttr,
    statusWarningAttr,
    statusErrorAttr,
    byteUsageAttr,
    tokenUsageAttr,
    signalMetricsAttr,
    pausedAttr,
    activationAlwaysAttr,
    activationOnDemandAttr,
    activationFirstNAttr,
    activationDefaultAttr,
    activeTabAttr,
    inactiveTabAttr,
    queuedMessageAttr,
    queuedMessageSelectedAttr,
    selectedTurnAttr,
    attachmentAttr,
    attachmentSelectedAttr,
    attachmentSizeAttr,
    dialogAttr,
    subcallAttr,
    defaultAttr,
    sendIndicatorAttr,
    treeBranchAttr,
    rootConversationAttr,
    subcallSelectedAttr,
    
    -- Re-exported from Layout
    render_ui,
    render_mainLayout,
    renderTabBar,
    render_sidebar,
    render_contentArea,
    renderAgentsTab,
    renderChatsTab,
    renderHistoryTab,
    renderHelpTab,
    render_agentDetail,
    render_conversationArea,
    render_sessionArea,
    render_shortcutsHelp,
    render_statusBar,
    statusAttr,
    
    -- Re-exported from Utils
    borderWithFocus,
    
    -- Re-exported from Conversation
    ConversationTree(..),
    buildConversationForest,
    sortConversationsForNesting,
    render_conversationList,
    renderConversationForest,
    renderTreeNode,
    renderNestedConversationItem,
    getQueuedMessageCount,
    getAttachmentCount,
    makePrefix,
    render_conversationView,
    render_sessionView,
    render_session,
    render_turn_navigation,
    render_navigable_turn,
    render_session_usage,
    renderSignalSummary,
    aggregateSessionTokenUsage,
    TokenUsageStats(..),
    formatTokenStats,
    formatTokenCount,
    addThousandSeparators,
    formatBytes,
    render_turn,
    render_usage,
    
    -- Re-exported from Dialog
    renderFilePathDialog,
    renderFileBrowserDialog,
    
    -- Re-exported from Widgets
    render_agentList,
    render_agentItem,
    render_agentInfo,
    render_sessionList,
    render_sessionItem,
    render_messageEditor,
    render_messageEditorWithAttachments,
    render_attachmentList,
    getAttachments,
    render_attachmentPanel,
    render_attachment_item,
    formatAttachmentSize,
    render_queued_messages_manager,
    render_queue_panel,
    render_queued_message_list,
    render_queued_item,
) where

import Brick (Widget)
import Control.Lens ((^.))

import System.Agents.TUI.Render.Attributes
import System.Agents.TUI.Render.Conversation
import System.Agents.TUI.Render.Dialog
import System.Agents.TUI.Render.Layout
import System.Agents.TUI.Render.Utils
import System.Agents.TUI.Render.Widgets
import System.Agents.TUI.Types (TuiState, N, tuiUI, attachmentDialogState, AttachmentDialogState(..))

-- | Main application draw function.
tui_appDraw :: TuiState -> [Widget N]
tui_appDraw st =
    case st ^. tuiUI . attachmentDialogState of
        AttachmentDialogPathInput -> [renderFilePathDialog st, render_ui st]
        AttachmentDialogFileBrowser -> [renderFileBrowserDialog st, render_ui st]
        AttachmentDialogClosed -> [render_ui st]

