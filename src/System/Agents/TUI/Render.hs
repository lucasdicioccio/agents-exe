{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.TUI.Render where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (atomically)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import GHC.IO.Handle (Handle)
import Prod.Tracer (Tracer (..), silent)
import System.IO (stderr, stdout)

import qualified System.Agents.Agent as Agent
import System.Agents.Base (newConversationId)
import System.Agents.Conversation
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.Party as Party
import qualified System.Agents.Tools as Tools
import qualified System.Agents.Tools.Bash as Tools
import qualified System.Agents.Tools.IO as Tools

import Brick
import Brick.Focus (FocusRing, focusGetCurrent, focusNext, focusPrev, focusRing)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Edit
import Brick.Widgets.List
import Control.Lens hiding (zoom) -- (makeLenses, to, use, (%=))
import qualified Graphics.Vty as Vty

import System.Agents.TUI.State

tui_appChooseCursor :: TuiState -> [CursorLocation N] -> Maybe (CursorLocation N)
tui_appChooseCursor st locs =
    case focusGetCurrent st._ui._focus of
        Just PromptEditor -> showCursorNamed PromptEditor locs
        Just AgentsList -> Nothing
        Just ConversationsList -> Nothing
        Just UnifiedList -> Nothing
        Just FocusedConversation -> Nothing
        Nothing -> Nothing

tui_appAttrMap :: TuiState -> AttrMap
tui_appAttrMap _ = attrMap Vty.defAttr []

tui_appDraw :: TuiState -> [Widget N]
tui_appDraw st = [render_ui st]
  where
    render_ui :: TuiState -> Widget N
    render_ui st
        | st._ui._zoomed == False =
            render_ui_general st
    render_ui st
        | st._ui._zoomed == True =
            case (focusGetCurrent st._ui._focus) of
                Nothing -> render_ui_general st
                (Just PromptEditor) -> render_promptEditor st
                (Just FocusedConversation) -> render_focusedConversation st
                (Just AgentsList) -> render_agentsList st
                (Just ConversationsList) -> render_conversationsList st
                (Just UnifiedList) -> render_unifiedList st

    render_ui_general :: TuiState -> Widget N
    render_ui_general st =
        hBox
            [ borderWithLabel
                (txt "chat")
                (hLimit 18 $ render_unifiedList st)
                {-
                            , borderWithLabel
                                (txt "agents")
                                (hLimit 18 $ render_agentsList st)
                            , borderWithLabel
                                (txt "conversations")
                                (hLimit 50 $ render_conversationsList st)
                                {-
                                            , borderWithLabel
                                                (txt "info")
                                                (hLimit 60 $ render_focusedAgentInfo st)
                                            , borderWithLabel
                                                (txt "tools")
                                                (hLimit 60 $ render_focusedAgentTools st)
                                -}
                -}
            ]
            <+> vBox
                [ borderWithLabel
                    (txt "prompt")
                    (render_promptEditor st)
                , borderWithLabel
                    (txt "conv")
                    ( hLimit 120 $
                        viewport FocusedConversation Both $
                            render_focusedConversation st
                    )
                ]

    render_promptEditor :: TuiState -> Widget N
    render_promptEditor st =
        renderEditor
            (txt . Text.unlines)
            (focusGetCurrent st._ui._focus == Just PromptEditor)
            st._ui._promptEditor

    render_unifiedList :: TuiState -> Widget N
    render_unifiedList st =
        let lst = st._ui._unifiedList
         in renderList render_unifiedList_Item True lst

    render_unifiedList_Item :: Bool -> ConversingEntryPoint -> Widget N
    render_unifiedList_Item active item =
        let
            activeFlag = if active then ">" else " "
         in
            case item of
                ChatEntryPoint (_, _, agent) ->
                    txt ("> " <> agent.slug)
                  where
                    flags = activeFlag <> " "
                ConversationEntryPoint conv ->
                    txt (flags <> conv.conversingAgent.slug)
                  where
                    flags = activeFlag <> modifiedFlag
                    modifiedFlag = if conv.historyChanged then "*" else " "

    render_agentsList :: TuiState -> Widget N
    render_agentsList st =
        let lst = st._ui._agentsList
         in renderList render_agentsList_Agent True lst

    render_agentsList_Agent :: Bool -> LoadedAgent -> Widget N
    render_agentsList_Agent True (_, _, agent) =
        txt ("> " <> agent.slug)
    render_agentsList_Agent False (_, _, agent) =
        txt ("  " <> agent.slug)

    render_conversationsList :: TuiState -> Widget N
    render_conversationsList st =
        let lst = st._ui._conversationsList
         in renderList render_conversationsList_Conversation True lst

    render_conversationsList_Conversation :: Bool -> OngoingConversation -> Widget N
    render_conversationsList_Conversation active conv =
        txt (flags <> conv.conversingAgent.slug)
      where
        flags = activeFlag <> modifiedFlag
        activeFlag = if active then ">" else " "
        modifiedFlag = if conv.historyChanged then "*" else " "

    render_focusedAgentInfo :: TuiState -> Widget N
    render_focusedAgentInfo st =
        case listSelectedElement st._ui._agentsList of
            Nothing ->
                txt "select an agent to show info"
            Just (_, (rt, _, oai)) ->
                txt oai.slug
                    <=> str (show rt.agentId)
                    <=> txt oai.announce
                    <=> txt (Text.unlines oai.systemPrompt)
                    <=> str oai.toolDirectory

    render_focusedAgentTools :: TuiState -> Widget N
    render_focusedAgentTools st =
        case listSelectedElement st._ui._agentsList of
            Nothing ->
                txt "select an agent to show tools"
            Just (_, (_, tools, _)) ->
                txt $ renderToolRegistry tools

    render_focusedConversation :: TuiState -> Widget N
    render_focusedConversation st =
        case listSelectedElement st._ui._conversationsList of
            Nothing ->
                txt "no history"
            Just (_, conv) ->
                vBox $ Maybe.mapMaybe (render_focusedConversation_HistoryItem st) conv.conversationHistory

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
