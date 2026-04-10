{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rendering functions for the TUI application.
module System.Agents.TUI.Render where

import Brick
import Brick.Focus (focusGetCurrent)
import qualified Brick.Util as BrickUtil
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Edit (renderEditor)
import Brick.Widgets.List (listSelectedAttr, listSelectedElement, renderList)
import Control.Lens ((^.))
import Data.List (intersperse)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as Vty

import System.Agents.AgentTree (OSAgentNode (..))
import System.Agents.Base (Agent (..), ConversationId)
import System.Agents.LLMs.OpenAI (TokenUsage (..))
import System.Agents.Session.Base hiding (Agent)
import System.Agents.Session.Types (StepByteUsage (..), sessionTotalBytes)
import System.Agents.TUI.Types
import System.Agents.ToolRegistration (ToolRegistration, declareTool, toolActivation)
import System.Agents.ToolSchema (ToolDescription (..), ToolName (..))
import System.Agents.Tools.Activation (Activation (..))

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

-- | Attribute for token usage text.
tokenUsageAttr :: AttrName
tokenUsageAttr = attrName "tokenUsage"

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

-------------------------------------------------------------------------------
-- Main Draw Function
-------------------------------------------------------------------------------

-- | Main application draw function.
tui_appDraw :: TuiState -> [Widget N]
tui_appDraw st = [render_ui st]

-- | Render the main UI based on current state.
-- When zoomed, the content displayed is based on the current tab rather than
-- the currently focused widget.
render_ui :: TuiState -> Widget N
render_ui st
    | st ^. tuiUI . zoomed =
        case st ^. tuiUI . currentTab of
            AgentsTab -> render_agentInfo st
            ChatsTab -> render_conversationView st
            HistoryTab -> render_sessionView st
            HelpTab -> renderHelpTab st
    | otherwise = render_mainLayout st

-------------------------------------------------------------------------------
-- Layout Components
-------------------------------------------------------------------------------

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
    hLimit 30 $
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

-- | Render the Chats tab content (conversation area).
renderChatsTab :: TuiState -> Widget N
renderChatsTab st =
    case focusGetCurrent (st ^. tuiUI . uiFocusRing) of
        Just MessageEditorWidget -> render_conversationArea st
        Just ConversationListWidget -> render_conversationArea st
        Just ConversationViewWidget -> render_conversationArea st
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
    -- Access the agent slug from the OS-native tree
    agentSlug0 = slug (osNodeConfig (tuiNode agent))

-------------------------------------------------------------------------------
-- Conversation List Rendering
-------------------------------------------------------------------------------

-- | Render the conversation list.
render_conversationList :: TuiState -> Widget N
render_conversationList st =
    borderWithFocus
        st
        ConversationListWidget
        "Conversations"
        (renderList (render_conversationItem st) hasFocus (st ^. tuiUI . conversationList))
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
        -- Add queued message count if any
        queueCount = getQueuedMessageCount st conv
        queueSuffix = if queueCount > 0 then " [" <> Text.pack (show queueCount) <> " queued]" else ""
        fullText = baseText <> turnSuffix <> queueSuffix
        widget = case conversationStatus conv of
            ConversationStatus_Paused ->
                withAttr pausedAttr $ txt $ " " <> fullText
            _ -> txt $ " " <> fullText
     in widget
  where
    isUnread = Set.member (conversationId conv) (st ^. tuiUI . unreadConversations)

-- | Get the number of queued messages for a conversation.
getQueuedMessageCount :: TuiState -> Conversation -> Int
getQueuedMessageCount st conv =
    let buffered = st ^. tuiUI . uiBufferedMessages
     in case Map.lookup (conversationId conv) buffered of
            Nothing -> 0
            Just msgs -> length msgs

-- | Render the sessions list.
render_sessionList :: TuiState -> Widget N
render_sessionList st =
    borderWithFocus
        st
        SessionsListWidget
        "Sessions"
        (renderList (render_sessionItem st) hasFocus (st ^. tuiUI . sessionList))
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
    borderWithFocus
        st
        AgentInfoWidget
        "Agent Info"
        ( case st ^. tuiUI . selectedAgentInfo of
            Nothing -> txt "No agent selected"
            Just agent ->
                let node = tuiNode agent
                    agentCfg = osNodeConfig node
                    mtools = lookup (tuiAgentId agent) (st ^. tuiUI . coreAgentTools)
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
-- Message Editor Rendering
-------------------------------------------------------------------------------

-- | Render the message input editor.
render_messageEditor :: TuiState -> Widget N
render_messageEditor st =
    borderWithFocus
        st
        MessageEditorWidget
        "Message"
        ( renderEditor
            (txt . Text.unlines)
            (focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just MessageEditorWidget)
            (st ^. tuiUI . messageEditor)
        )

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
                let queuedMsgs = getQueuedMessages st conv
                 in viewport ConversationViewWidget Both $ render_session (conversationSession conv) (st ^. tuiUI . ongoingConversations) queuedMsgs

-- | Get the list of queued messages for a conversation.
getQueuedMessages :: TuiState -> Conversation -> [Text]
getQueuedMessages st conv =
    let buffered = st ^. tuiUI . uiBufferedMessages
     in case Map.lookup (conversationId conv) buffered of
            Nothing -> []
            Just msgs -> reverse msgs -- Reverse to show oldest first

-- | Render the session history view.
render_sessionView :: TuiState -> Widget N
render_sessionView st =
    borderWithFocus st SessionViewWidget "Sessions" content
  where
    content =
        case listSelectedElement (st ^. tuiUI . sessionList) of
            Nothing -> txt "No session selected"
            Just (_, session) ->
                viewport SessionViewWidget Both $ render_session (Just session) (st ^. tuiUI . ongoingConversations) []

-- | Render a session's turns.
render_session :: Maybe Session -> Set ConversationId -> [Text] -> Widget N
render_session Nothing _ _ =
    vBox $ [txt "session not started yet"]
render_session (Just session) _ongoingConvs queuedMsgs =
    vBox $
        [render_queued_messages queuedMsgs]
            ++ [render_session_usage session]
            ++ map render_turn (Prelude.reverse (zip [(0 :: Int) ..] $ Prelude.reverse session.turns))

-- | Render queued messages for a conversation.
render_queued_messages :: [Text] -> Widget N
render_queued_messages [] = emptyWidget
render_queued_messages msgs =
    withAttr queuedMessageAttr $
        vBox $
            [txt "Queued messages:"]
                ++ map (\m -> txt $ "  ▶ " <> Text.take 60 m) msgs
                ++ [txt ""]

-- | Render total session usage (tokens if available, else bytes) and turn count.
render_session_usage :: Session -> Widget N
render_session_usage session =
    let turnCount = length session.turns
        -- Try to get aggregated token usage from all turns
        mTokenStats = aggregateSessionTokenUsage session
     in if turnCount == 0
            then emptyWidget
            else case mTokenStats of
                Just tokenStats ->
                    withAttr tokenUsageAttr $
                        txt $
                            "Session total: " <> formatTokenStats tokenStats <> "  (" <> Text.pack (show turnCount) <> " turns)  "
                Nothing ->
                    let totalBytes = sessionTotalBytes session
                     in if totalBytes == 0
                            then emptyWidget
                            else
                                withAttr byteUsageAttr $
                                    txt $
                                        "Session total: " <> formatBytes totalBytes <> "  (" <> Text.pack (show turnCount) <> " turns)  "

-- | Aggregate token usage across all turns in a session.
aggregateSessionTokenUsage :: Session -> Maybe TokenUsageStats
aggregateSessionTokenUsage session =
    let allTokenUsages = collectTokenUsages session.turns
     in if null allTokenUsages
            then Nothing
            else Just $ aggregateTokenUsages allTokenUsages
  where
    collectTokenUsages :: [Turn] -> [TokenUsage]
    collectTokenUsages turnList =
        [ usage
        | turn <- turnList
        , usage <- extractUsageFromTurn turn
        ]

    extractUsageFromTurn :: Turn -> [TokenUsage]
    extractUsageFromTurn turn =
        let mUsage = case turn of
                UserTurn _ mStepUsage -> mStepUsage
                LlmTurn _ mStepUsage -> mStepUsage
         in case mUsage of
                Just stepUsage -> maybeToList (stepTokenUsage stepUsage)
                Nothing -> []

    maybeToList :: Maybe a -> [a]
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

    aggregateTokenUsages :: [TokenUsage] -> TokenUsageStats
    aggregateTokenUsages usages =
        TokenUsageStats
            { statPromptTokens = sum $ map tokenPromptTokens usages
            , statCompletionTokens = sum $ map tokenCompletionTokens usages
            , statTotalTokens = sum $ map tokenTotalTokens usages
            , statCachedTokens = sumMaybe $ map tokenCachedTokens usages
            , statThinkingTokens = sumMaybe $ map tokenThinkingTokens usages
            }

    sumMaybe :: [Maybe Int] -> Maybe Int
    sumMaybe ms =
        let vs = [v | Just v <- ms]
         in if null vs then Nothing else Just (sum vs)

-- | Token usage statistics aggregated across a session.
data TokenUsageStats = TokenUsageStats
    { statPromptTokens :: Int
    , statCompletionTokens :: Int
    , statTotalTokens :: Int
    , statCachedTokens :: Maybe Int
    , statThinkingTokens :: Maybe Int
    }

-- | Format token stats for display.
formatTokenStats :: TokenUsageStats -> Text
formatTokenStats stats =
    let baseText = formatTokenCount (statTotalTokens stats) <> " tokens"
        details =
            [ "in: " <> formatTokenCount (statPromptTokens stats)
            , "out: " <> formatTokenCount (statCompletionTokens stats)
            ]
        withCached = case statCachedTokens stats of
            Just n | n > 0 -> details ++ ["cached: " <> formatTokenCount n]
            _ -> details
        withThinking = case statThinkingTokens stats of
            Just n | n > 0 -> withCached ++ ["think: " <> formatTokenCount n]
            _ -> withCached
     in baseText <> " (" <> Text.intercalate ", " withThinking <> ")"

-- | Format token count with thousands separator.
formatTokenCount :: Int -> Text
formatTokenCount n =
    let numText = Text.pack (show n)
     in addThousandSeparators numText

-- | Add thousand separators to a numeric text.
addThousandSeparators :: Text -> Text
addThousandSeparators numText =
    let digits = Text.unpack numText
        grouped = reverse $ group3 (reverse digits)
     in Text.pack $ concat (intersperse "," grouped)
  where
    group3 :: String -> [String]
    group3 [] = []
    group3 s = take 3 s : group3 (drop 3 s)

-- | Format bytes in human-readable form.
formatBytes :: Int -> Text
formatBytes n
    | n >= 1024 * 1024 * 1024 = Text.pack (show (n `div` (1024 * 1024 * 1024))) <> " GiB"
    | n >= 1024 * 1024 = Text.pack (show (n `div` (1024 * 1024))) <> " MiB"
    | n >= 1024 = Text.pack (show (n `div` 1024)) <> " KiB"
    | otherwise = Text.pack (show n) <> " B"

-- | Render a single turn with usage info (tokens preferred, bytes fallback).
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
                    , render_usage mUsage
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
                        , render_usage mUsage
                        , txt " "
                        ]
                    , if null llmTurn.llmToolCalls
                        then emptyWidget
                        else
                            vBox
                                [ txt $ "  [tool calls: " <> Text.pack (show (length llmTurn.llmToolCalls)) <> "]"
                                , render_usage mUsage
                                , txt " "
                                ]
                    ]

-- | Render usage info for a turn (tokens if available, else bytes).
render_usage :: Maybe StepByteUsage -> Widget N
render_usage Nothing = emptyWidget
render_usage (Just usage) =
    case stepTokenUsage usage of
        Just tokenUsage ->
            withAttr tokenUsageAttr $ txt $ "[" <> formatTokenUsage tokenUsage <> "]"
        Nothing ->
            withAttr byteUsageAttr $ txt $ "[" <> formatByteBreakdown usage <> "]"

-- | Format token usage for display.
formatTokenUsage :: TokenUsage -> Text
formatTokenUsage usage =
    let parts =
            concat
                [ ["in: " <> formatTokenCount (tokenPromptTokens usage)]
                , ["out: " <> formatTokenCount (tokenCompletionTokens usage)]
                , case tokenCachedTokens usage of
                    Just n | n > 0 -> ["cached: " <> formatTokenCount n]
                    _ -> []
                , case tokenThinkingTokens usage of
                    Just n | n > 0 -> ["think: " <> formatTokenCount n]
                    _ -> []
                ]
     in Text.intercalate ", " parts <> " | total: " <> formatTokenCount (tokenTotalTokens usage)

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
borderWithFocus :: TuiState -> WidgetName -> Text -> Widget n -> Widget n
borderWithFocus st widgetName labelText content =
    let labelWidget =
            if focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just widgetName
                then withAttr focusedAttr (txt labelText)
                else txt labelText
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
        , (tokenUsageAttr, BrickUtil.fg Vty.brightGreen `Vty.withStyle` Vty.dim)
        , (attrName "help", BrickUtil.fg Vty.yellow `Vty.withStyle` Vty.dim)
        , (statusInfoAttr, BrickUtil.fg Vty.white `Vty.withStyle` Vty.dim)
        , (statusWarningAttr, BrickUtil.fg Vty.yellow)
        , (statusErrorAttr, BrickUtil.fg Vty.red `Vty.withStyle` Vty.bold)
        , (pausedAttr, BrickUtil.fg Vty.yellow `Vty.withStyle` Vty.bold)
        , (activationAlwaysAttr, BrickUtil.fg Vty.brightGreen)
        , (activationOnDemandAttr, BrickUtil.fg Vty.yellow)
        , (activationFirstNAttr, BrickUtil.fg Vty.cyan)
        , (activationDefaultAttr, BrickUtil.fg Vty.white `Vty.withStyle` Vty.dim)
        , (activeTabAttr, Vty.defAttr `Vty.withForeColor` Vty.black `Vty.withBackColor` Vty.brightWhite `Vty.withStyle` Vty.bold)
        , (inactiveTabAttr, Vty.defAttr `Vty.withForeColor` Vty.white `Vty.withBackColor` Vty.blue)
        , (queuedMessageAttr, BrickUtil.fg Vty.yellow)
        ]

