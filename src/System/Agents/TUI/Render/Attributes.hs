{-# LANGUAGE OverloadedStrings #-}

-- | Attribute definitions for the TUI application.
module System.Agents.TUI.Render.Attributes where

import Brick
import qualified Brick.Util as BrickUtil
import Brick.Widgets.FileBrowser (
    fileBrowserAttr,
    fileBrowserCurrentDirectoryAttr,
    fileBrowserDirectoryAttr,
    fileBrowserRegularFileAttr,
    fileBrowserSelectedAttr,
    fileBrowserSelectionInfoAttr,
 )
import Brick.Widgets.List (listSelectedAttr)
import qualified Graphics.Vty as Vty

import System.Agents.TUI.Types (TuiState)

-- | Attribute for focused widgets.
focusedAttr :: AttrName
focusedAttr = attrName "focused"

-- | Attribute for user messages.
userMessageAttr :: AttrName
userMessageAttr = attrName "userMessage"

-- | Attribute for LLM messages.
llmMessageAttr :: AttrName
llmMessageAttr = attrName "llmMessage"

-- | Attribute for thinking/reasoning content.
thinkingAttr :: AttrName
thinkingAttr = attrName "thinking"

-- | Attribute for info status messages.
statusInfoAttr :: AttrName
statusInfoAttr = attrName "statusInfo"

-- | Attribute for warning status messages.
statusWarningAttr :: AttrName
statusWarningAttr = attrName "statusWarning"

-- | Attribute for error status messages.
statusErrorAttr :: AttrName
statusErrorAttr = attrName "statusError"

-- | Attribute for byte usage text (dimmed/smaller).
byteUsageAttr :: AttrName
byteUsageAttr = attrName "byteUsage"

-- | Attribute for token usage text.
tokenUsageAttr :: AttrName
tokenUsageAttr = attrName "tokenUsage"

-- | Attribute for signal metrics text.
signalMetricsAttr :: AttrName
signalMetricsAttr = attrName "signalMetrics"

-- | Attribute for paused conversation indicator.
pausedAttr :: AttrName
pausedAttr = attrName "paused"

-- | Attribute for always-on tool activation marker.
activationAlwaysAttr :: AttrName
activationAlwaysAttr = attrName "activationAlways"

-- | Attribute for on-demand tool activation marker.
activationOnDemandAttr :: AttrName
activationOnDemandAttr = attrName "activationOnDemand"

-- | Attribute for first-N steps tool activation marker.
activationFirstNAttr :: AttrName
activationFirstNAttr = attrName "activationFirstN"

-- | Attribute for default/no activation marker.
activationDefaultAttr :: AttrName
activationDefaultAttr = attrName "activationDefault"

-- | Attribute for the active tab in the tab bar.
activeTabAttr :: AttrName
activeTabAttr = attrName "activeTab"

-- | Attribute for inactive tabs in the tab bar.
inactiveTabAttr :: AttrName
inactiveTabAttr = attrName "inactiveTab"

-- | Attribute for queued messages.
queuedMessageAttr :: AttrName
queuedMessageAttr = attrName "queuedMessage"

-- | Attribute for selected queued messages.
queuedMessageSelectedAttr :: AttrName
queuedMessageSelectedAttr = attrName "queuedMessageSelected"

-- | Attribute for selected turn in navigation mode.
selectedTurnAttr :: AttrName
selectedTurnAttr = attrName "selectedTurn"

-- | Attribute for attachment items.
attachmentAttr :: AttrName
attachmentAttr = attrName "attachment"

-- | Attribute for selected attachment items.
attachmentSelectedAttr :: AttrName
attachmentSelectedAttr = attrName "attachmentSelected"

-- | Attribute for attachment file size.
attachmentSizeAttr :: AttrName
attachmentSizeAttr = attrName "attachmentSize"

-- | Attribute for buffer items.
bufferAttr :: AttrName
bufferAttr = attrName "buffer"

-- | Attribute for selected buffer items.
bufferSelectedAttr :: AttrName
bufferSelectedAttr = attrName "bufferSelected"

-- | Attribute for dialog overlays.
dialogAttr :: AttrName
dialogAttr = attrName "dialog"

-- | Attribute for subcall conversations (dimmed).
subcallAttr :: AttrName
subcallAttr = attrName "subcall"

-- | Default attribute.
defaultAttr :: AttrName
defaultAttr = attrName "default"

-- | Attribute for send indicator (ready to send state).
sendIndicatorAttr :: AttrName
sendIndicatorAttr = attrName "sendIndicator"

-- | Attribute for tree branch lines.
treeBranchAttr :: AttrName
treeBranchAttr = attrName "treeBranch"

-- | Attribute for root conversation items.
rootConversationAttr :: AttrName
rootConversationAttr = attrName "rootConversation"

-- | Attribute for selected subcall conversation items.
subcallSelectedAttr :: AttrName
subcallSelectedAttr = attrName "subcallSelected"

-- | Attribute map for styling.
tui_appAttrMap :: TuiState -> AttrMap
tui_appAttrMap _ =
    attrMap
        Vty.defAttr
        [ (focusedAttr, BrickUtil.bg Vty.blue)
        , (listSelectedAttr, Vty.defAttr `Vty.withForeColor` Vty.blue)
        , (userMessageAttr, BrickUtil.fg Vty.green)
        , (llmMessageAttr, BrickUtil.fg Vty.cyan)
        , (thinkingAttr, BrickUtil.fg Vty.magenta `Vty.withStyle` Vty.italic)
        , (byteUsageAttr, BrickUtil.fg Vty.brightYellow `Vty.withStyle` Vty.dim)
        , (tokenUsageAttr, BrickUtil.fg Vty.brightGreen `Vty.withStyle` Vty.dim)
        , (signalMetricsAttr, BrickUtil.fg Vty.brightCyan `Vty.withStyle` Vty.dim)
        , (statusInfoAttr, BrickUtil.fg Vty.white `Vty.withStyle` Vty.dim)
        , (statusWarningAttr, BrickUtil.fg Vty.yellow)
        , (statusErrorAttr, BrickUtil.fg Vty.red `Vty.withStyle` Vty.bold)
        , (pausedAttr, BrickUtil.fg Vty.yellow `Vty.withStyle` Vty.bold)
        , (sendIndicatorAttr, BrickUtil.fg Vty.brightGreen `Vty.withStyle` Vty.bold)
        , (activationOnDemandAttr, BrickUtil.fg Vty.yellow)
        , (activationFirstNAttr, BrickUtil.fg Vty.cyan)
        , (activationDefaultAttr, BrickUtil.fg Vty.white `Vty.withStyle` Vty.dim)
        , (activeTabAttr, Vty.defAttr `Vty.withForeColor` Vty.black `Vty.withBackColor` Vty.brightWhite `Vty.withStyle` Vty.bold)
        , (inactiveTabAttr, Vty.defAttr `Vty.withForeColor` Vty.white `Vty.withBackColor` Vty.blue)
        , (queuedMessageAttr, BrickUtil.fg Vty.yellow)
        , (queuedMessageSelectedAttr, BrickUtil.bg Vty.blue `Vty.withStyle` Vty.bold)
        , (selectedTurnAttr, BrickUtil.bg Vty.blue `Vty.withStyle` Vty.bold)
        , (attachmentAttr, BrickUtil.fg Vty.cyan)
        , (attachmentSelectedAttr, BrickUtil.bg Vty.blue `Vty.withStyle` Vty.bold)
        , (attachmentSizeAttr, BrickUtil.fg Vty.white `Vty.withStyle` Vty.dim)
        , (bufferAttr, BrickUtil.fg Vty.cyan)
        , (bufferSelectedAttr, BrickUtil.bg Vty.blue `Vty.withStyle` Vty.bold)
        , (dialogAttr, Vty.defAttr `Vty.withBackColor` Vty.black)
        , -- Tree and conversation hierarchy attributes
          (treeBranchAttr, BrickUtil.fg Vty.white `Vty.withStyle` Vty.dim)
        , (rootConversationAttr, Vty.defAttr)
        , (subcallAttr, BrickUtil.fg Vty.white `Vty.withStyle` Vty.dim)
        , (subcallSelectedAttr, Vty.defAttr `Vty.withForeColor` Vty.black `Vty.withBackColor` Vty.brightWhite `Vty.withStyle` Vty.bold)
        , -- FileBrowser attributes
          (fileBrowserAttr, Vty.defAttr)
        , (fileBrowserCurrentDirectoryAttr, BrickUtil.fg Vty.cyan)
        , (fileBrowserSelectionInfoAttr, BrickUtil.fg Vty.white `Vty.withStyle` Vty.dim)
        , (fileBrowserSelectedAttr, BrickUtil.bg Vty.blue `Vty.withStyle` Vty.bold)
        , (fileBrowserDirectoryAttr, BrickUtil.fg Vty.blue)
        , (fileBrowserRegularFileAttr, Vty.defAttr)
        ]
