{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Widget rendering functions for agents, sessions, message editor, attachments, the draft panel, and buffers.
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

import System.Agents.Media.Types (MediaAttachment (..))
import System.Agents.Protocol (AgentDescriptor (..), ToolDescriptor (..))
import System.Agents.SessionStore (SessionMeta (..))
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
    txt $ " " <> (tuiAgentDescriptor agent).adSlug

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
                let d = tuiAgentDescriptor agent
                 in viewport AgentInfoWidget Both $
                        vBox $
                            mconcat [agentHeader d, renderToolsSection d.adTools, agentPrompt d]
        )
  where
    agentHeader :: AgentDescriptor -> [Widget N]
    agentHeader d =
        [ txt $ "# Slug: " <> d.adSlug
        , txt $ "# Announce: " <> d.adDescription
        , txt ""
        , txt $ "# Model: " <> d.adModel
        , txt ""
        ]
    renderToolsSection :: [ToolDescriptor] -> [Widget N]
    renderToolsSection [] =
        [ txt "# Tools: none"
        ]
    renderToolsSection toolz =
        [ txt "# Tools:"
        , vBox $ map renderToolItem toolz
        ]
    renderToolItem :: ToolDescriptor -> Widget N
    renderToolItem tool =
        let toolName = tool.tdName
            activationMarker = renderActivationMarker tool.tdActivation
         in hBox [txt "- ", activationMarker, txt $ " " <> toolName]
    renderActivationMarker :: Maybe Activation -> Widget N
    renderActivationMarker Nothing = withAttr activationDefaultAttr $ txt "[a]"
    renderActivationMarker (Just activation) = case activation of
        AlwaysActivated -> withAttr activationAlwaysAttr $ txt "[A]"
        OnDemandActivated group -> withAttr activationOnDemandAttr $ txt $ "[D:" <> group <> "]"
    agentPrompt :: AgentDescriptor -> [Widget N]
    agentPrompt d =
        [ txt "# System Prompt:"
        , txt $ Text.unlines d.adSystemPrompt
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
render_sessionItem :: TuiState -> Bool -> SessionMeta -> Widget N
render_sessionItem _st _isSelected meta =
    txt $ Text.pack $ " " <> show meta.smSessionId

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
-- Draft Management Rendering (§5, D3: the Draft tab)
-------------------------------------------------------------------------------

{- | Render the draft panel for a conversation: collapsed, it shows the
draft's first line and a size indicator (chars, paragraphs)
(@todos/os-as-standalone-server.md@ §5). Hidden when the draft is empty.
-}
render_draft_manager :: TuiState -> Conversation -> Widget N
render_draft_manager st conv =
    if draftIsEmpty draft
        then emptyWidget
        else render_draft_panel st draft
  where
    draft = conversationDraft conv

-- | Render the collapsed draft summary panel.
render_draft_panel :: TuiState -> Draft -> Widget N
render_draft_panel st draft =
    borderWithFocus
        st
        DraftPanelWidget
        (" Draft (" <> sizeText <> ") ")
        $ vBox
            [ txt "Ctrl+A: edit draft | Ctrl+G: send now | Ctrl+D: clear"
            , txt ""
            , withAttr draftAttr $ txt (draftFirstLine draft)
            ]
  where
    chars = Text.length (draftText draft)
    paras = draftParagraphCount draft
    sizeText =
        Text.pack (show chars)
            <> " chars, "
            <> Text.pack (show paras)
            <> if paras == 1 then " paragraph" else " paragraphs"

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
