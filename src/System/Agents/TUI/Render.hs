{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.TUI.Render where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified System.Agents.Agent as Agent
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.Party as Party
import qualified System.Agents.Tools as Tools
import qualified System.Agents.Tools.Bash as Tools
import qualified System.Agents.Tools.IO as Tools

import Brick
import Brick.Focus (focusGetCurrent)
import qualified Brick.Util as BrickUtil
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Edit
import Brick.Widgets.List
import qualified Graphics.Vty as Vty

import System.Agents.TUI.State

tui_appChooseCursor :: TuiState -> [CursorLocation N] -> Maybe (CursorLocation N)
tui_appChooseCursor st locs =
    case focusGetCurrent st._ui._focus of
        Just PromptEditor -> showCursorNamed PromptEditor locs
        Just UnifiedList -> Nothing
        Just FocusedConversation -> Nothing
        Nothing -> Nothing

blueBg :: AttrName
blueBg = attrName "blueBg"

tui_appAttrMap :: TuiState -> AttrMap
tui_appAttrMap _ =
    attrMap
        Vty.defAttr
        [ (blueBg, BrickUtil.bg Vty.blue)
        , (listSelectedAttr, BrickUtil.fg Vty.blue)
        ]

tui_appDraw :: TuiState -> [Widget N]
tui_appDraw tuiState = [render_ui tuiState]
  where
    render_ui :: TuiState -> Widget N
    render_ui st
        | st._ui._zoomed == True =
            case (focusGetCurrent st._ui._focus) of
                Nothing -> render_ui_general st
                (Just PromptEditor) -> render_promptEditor st
                (Just FocusedConversation) -> render_focusedConversation st
                (Just UnifiedList) -> render_unifiedList st
    render_ui st
        | otherwise =
            render_ui_general st

    borderWithFocus :: TuiState -> N -> Text -> Widget N -> Widget N
    borderWithFocus st n name
        | focusGetCurrent st._ui._focus == Just n =
            borderWithLabel (withAttr blueBg (txt name))
        | otherwise =
            borderWithLabel (txt name)

    render_ui_general :: TuiState -> Widget N
    render_ui_general st =
        let
            chatList =
                borderWithFocus
                    st
                    UnifiedList
                    "chat"
                    (hLimit 18 $ render_unifiedList st)

            ongoingConversation =
                borderWithFocus
                    st
                    FocusedConversation
                    "conv"
                    ( hLimit 120 $
                        viewport FocusedConversation Both $
                            render_focusedConversation st
                    )
            agent_infos =
                borderWithLabel
                    (txt "info")
                    (hLimit 60 $ render_focusedAgentInfo st)
            agent_tools =
                borderWithLabel
                    (txt "tools")
                    (hLimit 60 $ render_focusedAgentTools st)
            prompt_input =
                borderWithFocus
                    st
                    PromptEditor
                    "prompt"
                    (render_promptEditor st)
         in
            case listSelectedElement st._ui._unifiedList of
                Nothing ->
                    hBox [chatList]
                (Just (_, (ChatEntryPoint _))) ->
                    hBox [chatList] <+> vBox [hBox [agent_infos, agent_tools], prompt_input]
                (Just (_, (ConversationEntryPoint _))) ->
                    hBox [chatList] <+> vBox [prompt_input, ongoingConversation]

    render_promptEditor :: TuiState -> Widget N
    render_promptEditor st =
        renderEditor
            (txt . Text.unlines)
            (focusGetCurrent st._ui._focus == Just PromptEditor)
            st._ui._promptEditor

    render_unifiedList :: TuiState -> Widget N
    render_unifiedList st =
        let lst = st._ui._unifiedList
            hasFocus = focusGetCurrent st._ui._focus == Just UnifiedList
         in renderList render_unifiedList_Item hasFocus lst

    render_unifiedList_Item :: Bool -> ChatHandle -> Widget N
    render_unifiedList_Item _ item =
        case item of
            ChatEntryPoint la ->
                txt (" @" <> la.loadedAgentInfo.slug)
            ConversationEntryPoint conv ->
                txt (" -" <> renderHeadline conv.headline)
              where
                renderHeadline :: Text -> Text
                renderHeadline = Text.take 8

    render_focusedAgentInfo :: TuiState -> Widget N
    render_focusedAgentInfo st =
        case listSelectedElement st._ui._unifiedList of
            Just (_, (ChatEntryPoint la)) ->
                txt la.loadedAgentInfo.announce
                    <=> txt (Text.unlines la.loadedAgentInfo.systemPrompt)
            _ ->
                txt "select an agent to show info"

    render_focusedAgentTools :: TuiState -> Widget N
    render_focusedAgentTools st =
        case listSelectedElement st._ui._unifiedList of
            Just (_, (ChatEntryPoint la)) ->
                txt (renderToolRegistry la.loadedAgentTools)
                    <=> str la.loadedAgentInfo.toolDirectory
            _ ->
                txt "select an agent to show tools"

    render_focusedConversation :: TuiState -> Widget N
    render_focusedConversation st =
        case listSelectedElement st._ui._unifiedList of
            Just (_, (ConversationEntryPoint conv)) ->
                render_focusedConversation_Status st conv
                    <=> vBox (Maybe.mapMaybe (render_focusedConversation_HistoryItem st) conv.conversationHistory)
            _ ->
                txt "missing conversation"

    render_focusedConversation_Status :: TuiState -> OngoingConversation -> Widget N
    render_focusedConversation_Status _ conv =
        case conv.conversationStatus of
            Party.WaitingForPrompt -> txt ""
            Party.Executing -> txt "(executing)"
            Party.Final -> txt "(ended)"

    render_focusedConversation_HistoryItem :: TuiState -> Agent.Trace -> Maybe (Widget N)
    render_focusedConversation_HistoryItem _ tr =
        case tr of
            (Agent.AgentTrace_Loading _ _ _) -> Nothing
            (Agent.AgentTrace_Conversation _ _ _ _) -> Nothing
            (Agent.AgentTrace_Memorize _ _ _ (Agent.GotResponse q _ _ rsp)) ->
                Just $
                    vBox $
                        Maybe.catMaybes
                            [ Just $ separator
                            , render_focusedConversation_HistoryItem_query q
                            , Just $ txt ""
                            , render_focusedConversation_HistoryItem_response_text rsp
                            , render_focusedConversation_HistoryItem_response_toolcalls rsp
                            , Just $ txt ""
                            ]
            (Agent.AgentTrace_Memorize _ _ _ _) -> Nothing

    render_focusedConversation_HistoryItem_query :: Agent.PendingQuery -> Maybe (Widget N)
    render_focusedConversation_HistoryItem_query Agent.GaveToolAnswers = Nothing
    render_focusedConversation_HistoryItem_query Agent.Done = Just $ txt "~~~ done ~~~"
    render_focusedConversation_HistoryItem_query (Agent.SomeQuery t) = Just $ txt ("> " <> t)

    render_focusedConversation_HistoryItem_response_text :: OpenAI.Response -> Maybe (Widget N)
    render_focusedConversation_HistoryItem_response_text rsp = do
        content <- rsp.rspContent
        pure $ txt ("< " <> content)

    render_focusedConversation_HistoryItem_response_toolcalls :: OpenAI.Response -> Maybe (Widget N)
    render_focusedConversation_HistoryItem_response_toolcalls rsp = do
        calls <- rsp.rspToolCalls
        if length calls == 0
            then
                Nothing
            else
                pure $ txt ("< tool calls")

renderToolRegistry :: (Aeson.ToJSON b) => [Tools.Registration a b c] -> Text
renderToolRegistry registry =
    Text.unlines $
        fmap renderRegisteredTool registry
  where
    renderRegisteredTool :: (Aeson.ToJSON b) => Tools.Registration a b c -> Text
    renderRegisteredTool reg =
        case reg.innerTool.toolDef of
            Tools.BashTool bashScript ->
                Text.unwords ["command", Text.pack bashScript.scriptPath, Text.decodeUtf8 $ LByteString.toStrict $ Aeson.encode reg.declareTool]
            Tools.IOTool ioScript ->
                Text.unwords ["io", ioScript.ioSlug, ioScript.ioDescription]

separator :: Widget a
separator = txt "=============="
