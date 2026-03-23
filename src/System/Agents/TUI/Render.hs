{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rendering functions for the TUI application.
module System.Agents.TUI.Render where

import Brick
import Brick.Focus (focusGetCurrent)
import qualified Brick.Util as BrickUtil
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Edit (renderEditor)
import Brick.Widgets.List (listSelectedAttr, listSelectedElement, renderList)
import Control.Lens ((^.))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as Vty

import System.Agents.AgentTree (AgentTree (..))
import System.Agents.Base (ConversationId)
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Runtime (Runtime (..))
import System.Agents.Session.Base
import System.Agents.Session.Types (StepByteUsage (..), sessionTotalBytes)
import System.Agents.TUI.Types
import System.Agents.ToolRegistration (ToolRegistration, declareTool)

-------------------------------------------------------------------------------
-- Attribute Names
-------------------------------------------------------------------------------

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

-- | Attribute for paused conversation indicator.
pausedAttr :: AttrName
pausedAttr = attrName "paused"

-------------------------------------------------------------------------------
-- Main Draw Function
-------------------------------------------------------------------------------

-- | Main application draw function.
tui_appDraw :: TuiState -> [Widget N]
tui_appDraw st = [render_ui st]

-- | Render the main UI based on current state.
render_ui :: TuiState -> Widget N
render_ui st
    | st ^. tuiUI . zoomed =
        case focusGetCurrent (st ^. tuiUI . uiFocusRing) of
            Just MessageEditorWidget -> render_messageEditor st
            Just ConversationViewWidget -> render_conversationView st
            Just SessionViewWidget -> render_sessionView st
            Just AgentInfoWidget -> render_agentInfo st
            _ -> render_mainLayout st
    | otherwise = render_mainLayout st

-------------------------------------------------------------------------------
-- Layout Components
-------------------------------------------------------------------------------

-- | Main layout with sidebar and content area.
render_mainLayout :: TuiState -> Widget N
render_mainLayout st =
    vBox
        [ hBox
            [ render_sidebar st
            , render_contentArea st
            ]
        , render_statusBar (st ^. tuiUI . statusMessage)
        ]

-- | Sidebar with agent and conversation lists.
render_sidebar :: TuiState -> Widget N
render_sidebar st =
    hLimit 25 $
        vBox
            [ render_agentList st
            , render_conversationList st
            , render_sessionList st
            ]

-- | Content area showing either agent info or conversation.
render_contentArea :: TuiState -> Widget N
render_contentArea st =
    case focusGetCurrent (st ^. tuiUI . uiFocusRing) of
        Just AgentListWidget -> render_agentDetail st
        Just AgentInfoWidget -> render_agentDetail st
        Just SessionsListWidget -> render_sessionArea st
        Just SessionViewWidget -> render_sessionArea st
        Just MessageEditorWidget -> render_conversationArea st
        Just ConversationListWidget -> render_conversationArea st
        Just ConversationViewWidget -> render_conversationArea st
        Nothing -> txt "hello"

-- | Agent detail view with info and tools.
render_agentDetail :: TuiState -> Widget N
render_agentDetail st =
    vBox
        [ render_agentInfo st
        ]

-- | Conversation area with message input and conversation history.
render_conversationArea :: TuiState -> Widget N
render_conversationArea st =
    case listSelectedElement (st ^. tuiUI . conversationList) of
        Nothing ->
            vBox
                [ txt "Select or create a conversation (Ctrl+n)"
                , render_messageEditor st
                ]
        Just _ ->
            vBox
                [ render_messageEditor st
                , render_conversationView st
                , render_shortcutsHelp
                ]

-- | Render shortcuts help bar.
render_shortcutsHelp :: Widget N
render_shortcutsHelp =
    withAttr (attrName "help") $
        hBox
            [ txt "Ctrl+E: pause | Ctrl+p: export md | Ctrl+[r|t]: view md"
            ]

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

-------------------------------------------------------------------------------
-- Status Bar Rendering
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Agent List Rendering
-------------------------------------------------------------------------------

-- | Render the agent list.
render_agentList :: TuiState -> Widget N
render_agentList st =
    borderWithFocus st AgentListWidget "Agents" $
        renderList render_agentItem hasFocus (st ^. tuiUI . agentList)
  where
    hasFocus = focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just AgentListWidget

-- | Render a single agent item.
render_agentItem :: Bool -> TuiAgent -> Widget N
render_agentItem _ agent =
    txt $ " " <> agentSlug0
  where
    -- Access the agent slug from the tree's runtime
    agentSlug0 = (tuiTree agent).agentRuntime.agentSlug

-------------------------------------------------------------------------------
-- Conversation List Rendering
-------------------------------------------------------------------------------

-- | Render the conversation list.
render_conversationList :: TuiState -> Widget N
render_conversationList st =
    borderWithFocus st ConversationListWidget "Conversations" $
        renderList (render_conversationItem st) hasFocus (st ^. tuiUI . conversationList)
  where
    hasFocus = focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just ConversationListWidget

-- | Render a single conversation item.
render_conversationItem :: TuiState -> Bool -> Conversation -> Widget N
render_conversationItem st _ conv =
    let indicator = case conversationStatus conv of
            ConversationStatus_Active -> "⟳ "
            ConversationStatus_WaitingForInput ->
                if isUnread then "● " else "  "
            ConversationStatus_Paused -> "⏸ "
        baseText = indicator <> Text.take 18 (conversationName conv)
        -- Add turn count next to conversation name
        turnCount = case conversationSession conv of
            Nothing -> 0
            Just session -> length session.turns
        turnSuffix = if turnCount > 0 then " (" <> Text.pack (show turnCount) <> ")" else ""
        fullText = baseText <> turnSuffix
        widget = case conversationStatus conv of
            ConversationStatus_Paused ->
                withAttr pausedAttr $ txt $ " " <> fullText
            _ -> txt $ " " <> fullText
     in widget
  where
    isUnread = Set.member (conversationId conv) (st ^. tuiUI . unreadConversations)

-- | Render the sessions list.
render_sessionList :: TuiState -> Widget N
render_sessionList st =
    borderWithFocus st SessionsListWidget "Sessions" $
        renderList (render_sessionItem st) hasFocus (st ^. tuiUI . sessionList)
  where
    hasFocus = focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just SessionsListWidget

-- | Render a single conversation item.
render_sessionItem :: TuiState -> Bool -> Session -> Widget N
render_sessionItem _st _ sess =
    let
        widget = txt $ Text.pack $ " " <> show sess.sessionId
     in
        widget

-------------------------------------------------------------------------------
-- Agent Info Rendering
-------------------------------------------------------------------------------

-- | Render agent information panel.
render_agentInfo :: TuiState -> Widget N
render_agentInfo st =
    borderWithFocus st AgentInfoWidget "Agent Info" $
        case st ^. tuiUI . selectedAgentInfo of
            Nothing -> txt "No agent selected"
            Just agent ->
                let tree = tuiTree agent
                    rt = tree.agentRuntime
                    mtools = lookup rt.agentId (st ^. tuiUI . coreAgentTools)
                 in viewport AgentInfoWidget Both $
                        vBox $
                            mconcat [agentHeader rt, renderToolsSection mtools, agentPrompt rt]
  where
    agentHeader :: Runtime -> [Widget N]
    agentHeader rt =
        [ txt $ "# Slug: " <> rt.agentSlug
        , txt $ "# Announce: " <> rt.agentAnnounce
        , txt ""
        , txt "# Model: " <=> txt (Text.pack $ show rt.agentModel.modelName)
        , txt ""
        ]
    renderToolsSection :: Maybe [ToolRegistration] -> [Widget N]
    renderToolsSection Nothing =
        [ txt "# Tools: not loaded"
        ]
    renderToolsSection (Just toolz) =
        [ txt "# Tools:"
        , txt $ Text.unlines ["- " <> (OpenAI.getToolName $ OpenAI.toolName (declareTool tool)) | tool <- toolz]
        ]
    agentPrompt :: Runtime -> [Widget N]
    agentPrompt rt =
        [ txt "# System Prompt:"
        , txt $ OpenAI.getSystemPrompt rt.agentModel.modelSystemPrompt
        ]

-------------------------------------------------------------------------------
-- Message Editor Rendering
-------------------------------------------------------------------------------

-- | Render the message input editor.
render_messageEditor :: TuiState -> Widget N
render_messageEditor st =
    borderWithFocus st MessageEditorWidget "Message" $
        renderEditor
            (txt . Text.unlines)
            (focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just MessageEditorWidget)
            (st ^. tuiUI . messageEditor)

-------------------------------------------------------------------------------
-- Conversation View Rendering
-------------------------------------------------------------------------------

-- | Render the conversation history view.
render_conversationView :: TuiState -> Widget N
render_conversationView st =
    borderWithFocus st ConversationViewWidget "Conversation" content
  where
    content =
        case listSelectedElement (st ^. tuiUI . conversationList) of
            Nothing -> txt "No conversation selected"
            Just (_, conv) ->
                viewport ConversationViewWidget Both $ render_session (conversationSession conv) (st ^. tuiUI . ongoingConversations)

-- | Render the session history view.
render_sessionView :: TuiState -> Widget N
render_sessionView st =
    borderWithFocus st SessionViewWidget "Sessions" content
  where
    content =
        case listSelectedElement (st ^. tuiUI . sessionList) of
            Nothing -> txt "No session selected"
            Just (_, session) ->
                viewport SessionViewWidget Both $ render_session (Just session) (st ^. tuiUI . ongoingConversations)

-- | Render a session's turns.
render_session :: Maybe Session -> Set ConversationId -> Widget N
render_session Nothing _ =
    vBox $ [txt "session not started yet"]
render_session (Just session) _ongoingConvs =
    vBox $
        [render_session_total_bytes session]
            ++ map render_turn (Prelude.reverse (zip [(0 :: Int) ..] $ Prelude.reverse session.turns))

-- | Render total session bytes and turn count.
render_session_total_bytes :: Session -> Widget N
render_session_total_bytes session =
    let totalBytes = sessionTotalBytes session
        turnCount = length session.turns
     in if totalBytes == 0 && turnCount == 0
            then emptyWidget
            else
                withAttr byteUsageAttr $
                    txt $
                        "Session total: " <> formatBytes totalBytes <> "  (" <> Text.pack (show turnCount) <> " turns)  "

-- | Format bytes in human-readable form.
formatBytes :: Int -> Text
formatBytes n
    | n >= 1024 * 1024 * 1024 = Text.pack (show (n `div` (1024 * 1024 * 1024))) <> " GiB"
    | n >= 1024 * 1024 = Text.pack (show (n `div` (1024 * 1024))) <> " MiB"
    | n >= 1024 = Text.pack (show (n `div` 1024)) <> " KiB"
    | otherwise = Text.pack (show n) <> " B"

-- | Render a single turn with byte usage.
render_turn :: (Int, Turn) -> Widget N
render_turn (k, turn) =
    case turn of
        UserTurn userTurn mUsage ->
            withAttr userMessageAttr $
                vBox
                    [ txt "-----------------------"
                    , case userQuery userTurn of
                        Just (UserQuery q) ->
                            vBox
                                [ txt $ "< " <> q
                                , txt ""
                                ]
                        Nothing -> emptyWidget
                    , if k == 0
                        then txt $ "+ " <> getSystemPromptText (userPrompt userTurn)
                        else txt $ "+ ..."
                    , emptyWidget -- TODO n-tools, n-responses here
                    , render_byte_usage mUsage
                    , txt ""
                    ]
        LlmTurn llmTurn mUsage ->
            withAttr llmMessageAttr $
                vBox
                    [ txt "-----------------------"
                    , vBox
                        [ case llmTurn.llmResponse.responseText of
                            Just txt0 -> txt $ "< " <> txt0
                            Nothing -> txt "< (no response)"
                        , case llmTurn.llmResponse.responseThinking of
                            Just thinking ->
                                withAttr thinkingAttr $
                                    vBox [txt "🤔 Thinking...", txt thinking]
                            Nothing -> txt ""
                        , render_byte_usage mUsage
                        , txt " "
                        ]
                    , if null llmTurn.llmToolCalls
                        then emptyWidget
                        else
                            vBox
                                [ txt $ "  [tool calls: " <> Text.pack (show (length llmTurn.llmToolCalls)) <> "]"
                                , render_byte_usage mUsage
                                , txt " "
                                ]
                    ]

-- | Render byte usage for a turn.
render_byte_usage :: Maybe StepByteUsage -> Widget N
render_byte_usage Nothing = emptyWidget
render_byte_usage (Just usage) =
    withAttr byteUsageAttr $
        txt $
            "[" <> formatByteBreakdown usage <> "]"

{- | Format byte usage breakdown for display.
TODO: display tools and tool-responses explicitly
-}
formatByteBreakdown :: StepByteUsage -> Text
formatByteBreakdown usage =
    let parts =
            concat
                [ if stepInputBytes usage > 0 then ["in: " <> formatBytes (stepInputBytes usage)] else []
                , if stepOutputBytes usage > 0 then ["out: " <> formatBytes (stepOutputBytes usage)] else []
                , if stepReasoningBytes usage > 0 then ["reason: " <> formatBytes (stepReasoningBytes usage)] else []
                , if stepToolBytes usage > 0 then ["tool: " <> formatBytes (stepToolBytes usage)] else []
                ]
     in if null parts
            then "total: " <> formatBytes (stepTotalBytes usage)
            else Text.intercalate ", " parts

-- | Helper to extract text from SystemPrompt.
getSystemPromptText :: SystemPrompt -> Text
getSystemPromptText (SystemPrompt txt0) = txt0

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- | Create a border that shows focus.
borderWithFocus :: TuiState -> WidgetName -> Text -> Widget N -> Widget N
borderWithFocus st widgetName label content =
    let labelWidget =
            if focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just widgetName
                then withAttr focusedAttr (txt label)
                else txt label
     in borderWithLabel labelWidget content

-------------------------------------------------------------------------------
-- Attribute Map
-------------------------------------------------------------------------------

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
        , (attrName "help", BrickUtil.fg Vty.yellow `Vty.withStyle` Vty.dim)
        , (statusInfoAttr, BrickUtil.fg Vty.white `Vty.withStyle` Vty.dim)
        , (statusWarningAttr, BrickUtil.fg Vty.yellow)
        , (statusErrorAttr, BrickUtil.fg Vty.red `Vty.withStyle` Vty.bold)
        , (pausedAttr, BrickUtil.fg Vty.yellow `Vty.withStyle` Vty.bold)
        ]

