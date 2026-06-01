{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Widget rendering functions for agents, sessions, message editor, attachments, queued messages, and buffers.
module System.Agents.TUI.Render.Widgets where

import Brick
import Brick.Focus (focusGetCurrent)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Edit (getEditContents, renderEditor)
import Brick.Widgets.List (renderList)
import Control.Lens ((^.))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import System.Agents.AgentTree (OSAgentNode (..))
import System.Agents.Base (Agent (..))
import System.Agents.Media.Types (MediaAttachment (..))
import System.Agents.Session.Base (Session (..))
import System.Agents.TUI.Buffer (Buffer, bufferContent)
import System.Agents.TUI.MessageComposer (
    InputConfig (..),
    SendTrigger (..),
    willSendOnNextNewline,
 )
import System.Agents.TUI.Render.Attributes
import System.Agents.TUI.Render.Conversation (formatBytes, getAttachmentCount)
import System.Agents.TUI.Render.Utils (borderWithFocus)
import System.Agents.TUI.Types
import System.Agents.ToolRegistration (ToolRegistration, declareTool, toolActivation)
import System.Agents.ToolSchema (ToolDescription (..), ToolName (..))
import System.Agents.Tools.Activation (Activation (..))

-------------------------------------------------------------------------------
-- Agent List Rendering
-------------------------------------------------------------------------------

-- | Render the agent list.
render_agentList :: TuiState -> Widget N
render_agentList st =
    borderWithFocus
        st
        AgentListWidget
        "Agents"
        (renderList render_agentItem hasFocus (st ^. tuiUI . agentList))
  where
    hasFocus = focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just AgentListWidget

-- | Render a single agent item.
render_agentItem :: Bool -> TuiAgent -> Widget N
render_agentItem _ agent =
    txt $ " " <> agentSlug0
  where
    agentSlug0 = slug (osNodeConfig (tuiNode agent))

-- | Render agent information panel.
render_agentInfo :: TuiState -> Widget N
render_agentInfo st =
    borderWithFocus
        st
        AgentInfoWidget
        "Agent Info"
        ( case st ^. tuiUI . selectedAgentInfo of
            Nothing -> txt "No agent selected"
            Just agent ->
                let node = tuiNode agent
                    agentCfg = osNodeConfig node
                    mtools = lookup (tuiAgentId agent) (st ^. tuiUI . uiAgentTools)
                 in viewport AgentInfoWidget Both $
                        vBox $
                            mconcat [agentHeader agentCfg, renderToolsSection mtools, agentPrompt agentCfg]
        )
  where
    agentHeader :: Agent -> [Widget N]
    agentHeader agentCfg =
        [ txt $ "# Slug: " <> slug agentCfg
        , txt $ "# Announce: " <> announce agentCfg
        , txt ""
        , txt $ "# Model: " <> modelName agentCfg
        , txt ""
        ]
    renderToolsSection :: Maybe [ToolRegistration] -> [Widget N]
    renderToolsSection Nothing =
        [ txt "# Tools: not loaded"
        ]
    renderToolsSection (Just toolz) =
        [ txt "# Tools:"
        , vBox $ map renderToolItem toolz
        ]
    renderToolItem :: ToolRegistration -> Widget N
    renderToolItem tool =
        let toolName = tool.declareTool.toolDescriptionName.getToolName
            activation = toolActivation tool
            activationMarker = renderActivationMarker activation
         in hBox [txt "- ", activationMarker, txt $ " " <> toolName]
    renderActivationMarker :: Maybe Activation -> Widget N
    renderActivationMarker Nothing = withAttr activationDefaultAttr $ txt "[a]"
    renderActivationMarker (Just activation) = case activation of
        AlwaysActivated -> withAttr activationAlwaysAttr $ txt "[A]"
        OnDemandActivated group -> withAttr activationOnDemandAttr $ txt $ "[D:" <> group <> "]"
    agentPrompt :: Agent -> [Widget N]
    agentPrompt agentCfg =
        [ txt "# System Prompt:"
        , txt $ Text.unlines $ systemPrompt agentCfg
        ]

-------------------------------------------------------------------------------
-- Agent Detail Rendering
-------------------------------------------------------------------------------

-- | Agent detail view with info and tools.
render_agentDetail :: TuiState -> Widget N
render_agentDetail st =
    vBox
        [ render_agentInfo st
        ]

-------------------------------------------------------------------------------
-- Session List Rendering
-------------------------------------------------------------------------------

-- | Render the session list.
render_sessionList :: TuiState -> Widget N
render_sessionList st =
    borderWithFocus
        st
        SessionsListWidget
        "Sessions"
        (renderList (render_sessionItem st) hasFocus (st ^. tuiUI . sessionList))
  where
    hasFocus = focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just SessionsListWidget

-- | Render a single session item.
render_sessionItem :: TuiState -> Bool -> Session -> Widget N
render_sessionItem _st _isSelected sess =
    txt $ Text.pack $ " " <> show sess.sessionId

-------------------------------------------------------------------------------
-- Message Editor Rendering
-------------------------------------------------------------------------------

-- | Render the message input editor.
render_messageEditor :: TuiState -> Widget N
render_messageEditor st =
    borderWithFocus
        st
        MessageEditorWidget
        "Message"
        $ renderEditor
            (txt . Text.unlines)
            (focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just MessageEditorWidget)
            (st ^. tuiUI . messageEditor)

-- | Render the message editor with attachment count and send indicator in the label.
render_messageEditorWithAttachments :: TuiState -> Conversation -> Widget N
render_messageEditorWithAttachments st conv =
    let attachmentCount = getAttachmentCount st conv
        inputCfg = sessionInputConfig (st ^. sessionConfig)
        showIndicator = showSendIndicator inputCfg
        trigger = sendTrigger inputCfg

        -- Get current editor content to check for send indicator
        editorContent = Text.unlines $ getEditContents (st ^. tuiUI . messageEditor)
        willSend = willSendOnNextNewline inputCfg editorContent

        -- Build the label text
        baseLabel =
            if attachmentCount > 0
                then "Message [" <> Text.pack (show attachmentCount) <> " 📎]"
                else "Message"

        -- Add mode indicator for triple-newline mode
        modeLabel = case trigger of
            TripleNewline -> baseLabel <> " (↵↵↵ to send)"
            Keymap -> baseLabel

        -- Add ready-to-send indicator
        labelText =
            if showIndicator && willSend
                then modeLabel <> " [READY TO SEND]"
                else modeLabel

        -- Determine label attribute
        labelAttr =
            if focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just MessageEditorWidget
                then
                    if showIndicator && willSend
                        then withAttr sendIndicatorAttr . withAttr focusedAttr
                        else withAttr focusedAttr
                else
                    if showIndicator && willSend
                        then withAttr sendIndicatorAttr
                        else id
     in borderWithLabel
            (labelAttr $ txt labelText)
            $ renderEditor
                (txt . Text.unlines)
                (focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just MessageEditorWidget)
                (st ^. tuiUI . messageEditor)

-------------------------------------------------------------------------------
-- Attachment Rendering
-------------------------------------------------------------------------------

-- | Render the attachment list for a conversation.
render_attachmentList :: TuiState -> Conversation -> Widget N
render_attachmentList st conv =
    let attachments = getAttachments st conv
     in if null attachments
            then emptyWidget
            else render_attachmentPanel st attachments

-- | Get attachments for a conversation.
getAttachments :: TuiState -> Conversation -> [MediaAttachment]
getAttachments st conv =
    let atts = st ^. tuiUI . attachedFiles
     in case Map.lookup (conversationId conv) atts of
            Nothing -> []
            Just xs -> xs

-- | Render the attachment panel.
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
  where
    selectedIdx = st ^. tuiUI . selectedAttachmentIndex

-- | Render a single attachment item.
render_attachment_item :: Maybe Int -> Int -> MediaAttachment -> Widget N
render_attachment_item selectedIdx idx att =
    let isSelected = selectedIdx == Just idx
        marker = if isSelected then "▶ " else "  "
        filename = maybe "unnamed" id att.mediaFilename
        mimeType = att.mediaMimeType
        sizeStr = formatAttachmentSize att.mediaBase64Data
        attr = if isSelected then attachmentSelectedAttr else attachmentAttr
     in withAttr attr $
            hBox
                [ txt marker
                , txt "📎 "
                , txt filename
                , txt " ("
                , withAttr attachmentSizeAttr $ txt mimeType
                , txt ", "
                , withAttr attachmentSizeAttr $ txt sizeStr
                , txt ")"
                ]

-- | Format attachment size based on base64 data length.
formatAttachmentSize :: Text -> Text
formatAttachmentSize base64Data =
    let base64Len = Text.length base64Data
        originalBytes = (base64Len * 3) `div` 4
     in formatBytes originalBytes

-------------------------------------------------------------------------------
-- Queued Messages Management Rendering
-------------------------------------------------------------------------------

-- | Render the queued messages management panel.
render_queued_messages_manager :: TuiState -> Conversation -> Widget N
render_queued_messages_manager st conv =
    if conversationStatus conv /= ConversationStatus_Paused
        then emptyWidget
        else
            let queuedMsgs = getQueuedMessages st conv
                count = length queuedMsgs
             in if count == 0
                    then emptyWidget
                    else render_queue_panel st count queuedMsgs

-- | Get the list of queued messages for a conversation.
getQueuedMessages :: TuiState -> Conversation -> [Text]
getQueuedMessages st conv =
    let buffered = st ^. tuiUI . uiBufferedMessages
     in case Map.lookup (conversationId conv) buffered of
            Nothing -> []
            Just msgs -> reverse msgs

-- | Render the queue management UI panel.
render_queue_panel :: TuiState -> Int -> [Text] -> Widget N
render_queue_panel st count msgs =
    borderWithFocus
        st
        QueuedMessageListWidget
        (" Queued Messages (" <> Text.pack (show count) <> ") ")
        $ vBox
            [ txt "Ctrl+D: clear all | Del/Backspace: delete selected | Up/Down: select"
            , txt ""
            , render_queued_message_list selectedIdx msgs
            ]
  where
    selectedIdx = st ^. tuiUI . queuedMessagesFocus

-- | Render the list of queued messages with selection.
render_queued_message_list :: Maybe Int -> [Text] -> Widget N
render_queued_message_list selectedIdx msgs =
    vBox $ zipWith (render_queued_item selectedIdx) [0 ..] msgs

-- | Render a single queued message item.
render_queued_item :: Maybe Int -> Int -> Text -> Widget N
render_queued_item selectedIdx idx msg =
    let isSelected = selectedIdx == Just idx
        marker = if isSelected then "▶ " else "  "
        truncated = Text.take 60 msg <> if Text.length msg > 60 then "..." else ""
        attr = if isSelected then queuedMessageSelectedAttr else queuedMessageAttr
     in withAttr attr $ txt $ marker <> truncated

-------------------------------------------------------------------------------
-- Buffer Rendering
-------------------------------------------------------------------------------

-- | Render the buffer list widget below message editor.
render_buffer_manager :: TuiState -> Widget N
render_buffer_manager st =
    let bufs = st ^. tuiUI . buffers
        count = length bufs
     in if count == 0
            then emptyWidget
            else render_buffer_panel st count bufs

-- | Render the buffer panel with controls hint.
render_buffer_panel :: TuiState -> Int -> [Buffer] -> Widget N
render_buffer_panel st count bufs =
    borderWithFocus
        st
        BufferListWidget
        (" Buffers (" <> Text.pack (show count) <> ") ")
        $ vBox
            [ txt "Enter: resume | Del: delete | Ctrl+Shift+K: clear all"
            , txt ""
            , render_buffer_list selectedIdx bufs
            ]
  where
    selectedIdx = st ^. tuiUI . bufferFocus

-- | Render the list of buffers with selection.
render_buffer_list :: Maybe Int -> [Buffer] -> Widget N
render_buffer_list selectedIdx bufs =
    vBox $ zipWith (render_buffer_item selectedIdx) [0 ..] bufs

-- | Render a single buffer item with preview.
render_buffer_item :: Maybe Int -> Int -> Buffer -> Widget N
render_buffer_item selectedIdx idx buf =
    let isSelected = selectedIdx == Just idx
        marker = if isSelected then "▶ " else "  "
        preview = Text.take 50 (buf ^. bufferContent)
        displayText = if Text.null preview then "(empty)" else preview
        attr = if isSelected then bufferSelectedAttr else bufferAttr
     in withAttr attr $ txt $ marker <> displayText

