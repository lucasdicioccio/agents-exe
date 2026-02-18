{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Agents.TUI2.Core where

import Brick
import Brick.BChan (BChan, newBChan, readBChan, writeBChan)
import Brick.Focus (FocusRing, focusGetCurrent, focusSetCurrent, focusNext, focusPrev, focusRing, focusRingModify)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Edit
import Brick.Widgets.List
import qualified Brick.Util as BrickUtil
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Lens (makeLenses, to, use, (%=), (.=), (^.))
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.CircularList as CList
import Data.Foldable (toList, traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Zipper as TextZipper
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Input.Events as Vty

import System.Agents.AgentTree (AgentTree, LoadAgentResult (..), Props, agentRuntime, loadAgentTreeRuntime)
import System.Agents.Base (AgentId, ConversationId(..), newConversationId)
import System.Agents.OneShot (runtimeToAgent, storeSession)
import System.Agents.Runtime (Runtime (..))
import qualified System.Agents.Runtime as Runtime
import System.Agents.Session.Base 
import qualified System.Agents.Session.Loop as Loop
import System.Agents.ToolRegistration (ToolRegistration (..))
import qualified System.Agents.Tools as Tools
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.Tools.IO as IOTools
import qualified System.Agents.Tools.McpToolbox as McpTools
import qualified System.Agents.LLMs.OpenAI as OpenAI

-------------------------------------------------------------------------------
-- Data Types
-------------------------------------------------------------------------------

-- | Widget names for Brick
data WidgetName
    = AgentListWidget
    | ConversationListWidget
    | MessageEditorWidget
    | ConversationViewWidget
    | AgentInfoWidget
    | AgentToolsWidget
    deriving (Show, Eq, Ord)

type N = WidgetName

-- | Application events
data AppEvent
    = AppEvent_Heartbeat
    | AppEvent_AgentStepProgrress ConversationId Session
    | AppEvent_AgentNeedsInput ConversationId
    | AppEvent_AgentTrace Runtime.Trace
    -- ^ Result of an agent step: either an error message, or the LLM turn content and updated session
    deriving (Show)

-- | Wrapper for an agent with its tree structure
data TuiAgent = TuiAgent
    { sessionAgent :: Agent (LlmTurnContent, Session)
    , agentTree :: AgentTree
    }

-- | A conversation with an agent
data Conversation = Conversation
    { conversationId :: ConversationId
    , conversationAgent :: TuiAgent
    , conversationThreadId :: ThreadId
    , conversationSession :: Maybe Session
    , conversationName :: Text
    , conversationChan :: BChan (Maybe UserQuery)
    }

-- | The core state holding agents and conversations
data Core = Core
    { coreAgents :: [TuiAgent]
    , coreConversations :: [Conversation]
    }
makeLenses ''Core

-- | UI-related state
data UIState = UIState
    { _uiFocusRing :: FocusRing WidgetName
    , _zoomed :: Bool
    , _agentList :: List WidgetName TuiAgent
    , _conversationList :: List WidgetName Conversation
    , _messageEditor :: Editor Text WidgetName
    , _selectedAgentInfo :: Maybe TuiAgent
    , _unreadConversations :: Set ConversationId
    , _ongoingConversations :: Set ConversationId
    -- ^ Conversations currently being processed by an agent
    }
makeLenses ''UIState

-- | Main TUI state combining core and UI
data TuiState = TuiState
    { _tuiCore :: TVar Core
    , _tuiUI :: UIState
    , _eventChan :: BChan AppEvent
    }
makeLenses ''TuiState

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

-- | Create initial UI state
initUIState :: [TuiAgent] -> UIState
initUIState agents =
    UIState
        { _uiFocusRing = focusRing [AgentListWidget, ConversationListWidget, MessageEditorWidget, AgentInfoWidget, AgentToolsWidget]
        , _zoomed = False
        , _agentList = list AgentListWidget (Vector.fromList agents) 1
        , _conversationList = list ConversationListWidget Vector.empty 1
        , _messageEditor = editorText MessageEditorWidget (Just 1) ""
        , _selectedAgentInfo = listToMaybe agents
        , _unreadConversations = Set.empty
        , _ongoingConversations = Set.empty
        }

-- | Initialize the TUI with props
runTUI :: Maybe FilePath -> [Props] -> IO ()
runTUI mConvPrefix props = do
    let convPrefix = fromMaybe "./" mConvPrefix
    -- Load agent trees and create TuiAgents
    trees <- traverse loadAgentTreeRuntime props
    let itrees = [rt | Initialized rt <- trees]
    sessionAgents <- traverse (runtimeToAgent . agentRuntime) itrees
    let tuiAgents = zipWith TuiAgent sessionAgents itrees

    -- Create core state
    core0 <- newTVarIO (Core tuiAgents [])

    -- Create event channel
    eventChan <- newBChan 100

    -- Create UI state
    let ui0 = initUIState tuiAgents

    -- Create TUI state
    let st = TuiState core0 ui0 eventChan

    -- Build and run the app
    let app =
            App
                { appDraw = tui_appDraw
                , appChooseCursor = tui_appChooseCursor
                , appHandleEvent = tui_appHandleEvent convPrefix
                , appStartEvent = tui_appStartEvent
                , appAttrMap = tui_appAttrMap
                }

    void $ customMainWithDefaultVty (Just eventChan) app st

-------------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------------

tui_appDraw :: TuiState -> [Widget N]
tui_appDraw st = [render_ui st]

render_ui :: TuiState -> Widget N
render_ui st
    | st ^. tuiUI . zoomed =
        case focusGetCurrent (st ^. tuiUI . uiFocusRing) of
            Just MessageEditorWidget -> render_messageEditor st
            Just ConversationViewWidget -> render_conversationView st
            Just AgentInfoWidget -> render_agentInfo st
            Just AgentToolsWidget -> render_agentTools st
            _ -> render_mainLayout st
    | otherwise = render_mainLayout st

-- | Main layout with sidebar and content area
render_mainLayout :: TuiState -> Widget N
render_mainLayout st =
    hBox
        [ render_sidebar st
        , render_contentArea st
        ]

-- | Sidebar with agent and conversation lists
render_sidebar :: TuiState -> Widget N
render_sidebar st =
    hLimit 25 $
        vBox
            [ render_agentList st
            , hBorder
            , render_conversationList st
            ]

-- | Render the agent list
render_agentList :: TuiState -> Widget N
render_agentList st =
    borderWithFocus st AgentListWidget "Agents" $
        renderList render_agentItem hasFocus (st ^. tuiUI . agentList)
  where
    hasFocus = focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just AgentListWidget

render_agentItem :: Bool -> TuiAgent -> Widget N
render_agentItem _ agent =
    txt $ " " <> agentSlug
  where
    agentSlug = agent.agentTree.agentRuntime.agentSlug

-- | Render the conversation list
render_conversationList :: TuiState -> Widget N
render_conversationList st =
    borderWithFocus st ConversationListWidget "Conversations" $
        renderList (render_conversationItem st) hasFocus (st ^. tuiUI . conversationList)
  where
    hasFocus = focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just ConversationListWidget

render_conversationItem :: TuiState -> Bool -> Conversation -> Widget N
render_conversationItem st _ conv =
    let indicator = if isOngoing then "⟳ " else if isUnread then "● " else "  "
        baseText = indicator <> Text.take 20 (conversationName conv)
    in txt $ " " <> baseText
  where
    isUnread = Set.member (conversationId conv) (st ^. tuiUI . unreadConversations)
    isOngoing = Set.member (conversationId conv) (st ^. tuiUI . ongoingConversations)

-- | Content area showing either agent info or conversation
render_contentArea :: TuiState -> Widget N
render_contentArea st =
    case focusGetCurrent (st ^. tuiUI . uiFocusRing) of
        Just AgentListWidget -> render_agentDetail st
        Just AgentInfoWidget -> render_agentDetail st
        Just AgentToolsWidget -> render_agentDetail st
        _ -> render_conversationArea st

-- | Agent detail view with info and tools
render_agentDetail :: TuiState -> Widget N
render_agentDetail st =
    vBox
        [ render_agentInfo st
        , hBorder
        , render_agentTools st
        ]

-- | Render agent information panel
render_agentInfo :: TuiState -> Widget N
render_agentInfo st =
    borderWithFocus st AgentInfoWidget "Agent Info" $
        case st ^. tuiUI . selectedAgentInfo of
            Nothing -> txt "No agent selected"
            Just agent ->
                let rt = agent.agentTree.agentRuntime
                 in viewport AgentInfoWidget Both $ hLimit 60 $ vBox
                        [ txt $ "Slug: " <> rt.agentSlug
                        , txt $ "Announce: " <> rt.agentAnnounce
                        , txt ""
                        , txt "Model: " <=> txt (Text.pack $ show rt.agentModel.modelName)
                        , txt ""
                        , txt "System Prompt:"
                        , txt $ OpenAI.getSystemPrompt rt.agentModel.modelSystemPrompt
                        ]

-- | Render agent tools panel
render_agentTools :: TuiState -> Widget N
render_agentTools st =
  ( hLimit 60 $
    borderWithFocus st AgentToolsWidget "Tools" $
        case st ^. tuiUI . selectedAgentInfo of
            Nothing -> txt "No agent selected"
            Just agent ->
                -- Tools are loaded asynchronously, show placeholder for now
                txt "placeholder"
  )

-- | Conversation area with message input and conversation history
render_conversationArea :: TuiState -> Widget N
render_conversationArea st =
    case listSelectedElement (st ^. tuiUI . conversationList) of
        Nothing ->
            vBox
                [ txt "Select or create a conversation"
                , render_messageEditor st
                ]
        Just (_, conv) ->
            vBox
                [ hLimit 60 $ render_messageEditor st
                , hLimit 80 $ render_conversationView st
                ]

-- | Render the message input editor
render_messageEditor :: TuiState -> Widget N
render_messageEditor st =
    borderWithFocus st MessageEditorWidget "Message" $
        renderEditor
            (txt . Text.unlines)
            (focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just MessageEditorWidget)
            (st ^. tuiUI . messageEditor)

-- | Render the conversation history view
render_conversationView :: TuiState -> Widget N
render_conversationView st =
       borderWithFocus st ConversationViewWidget "Conversation" content
  where
    content =
        case listSelectedElement (st ^. tuiUI . conversationList) of
            Nothing -> txt "No conversation selected"
            Just (_, conv) ->
                viewport ConversationViewWidget Both $ render_session (conversationSession conv) (st ^. tuiUI . ongoingConversations)

-- | Render a session's turns
render_session :: Maybe Session -> Set ConversationId -> Widget N
render_session Nothing _ =
    vBox $ [ txt "session not started yet" ]
render_session (Just session) ongoingConvs =
    vBox $ map render_turn (Prelude.reverse (zip [0..] $ Prelude.reverse session.turns))

-- | Render a single turn
render_turn :: (Int,Turn) -> Widget N
render_turn (k,turn) =
    case turn of
        UserTurn userTurn ->
            withAttr userMessageAttr $
                vBox
                    [ if k == 0
                        then txt $ "> " <> getSystemPromptText (userPrompt userTurn)
                        else emptyWidget
                    , case userQuery userTurn of
                        Just (UserQuery q) -> txt $ "< " <> q
                        Nothing -> emptyWidget
                    ]
        LlmTurn llmTurn ->
            withAttr llmMessageAttr $
                vBox
                    [ case llmTurn.llmResponse.responseText of
                        Just txt0 -> txt $ "< " <> txt0
                        Nothing -> txt "< (no response)"
                    , if null llmTurn.llmToolCalls
                        then emptyWidget
                        else txt $ "  [tool calls: " <> Text.pack (show (length llmTurn.llmToolCalls)) <> "]"
                    ]

-- | Helper to extract text from SystemPrompt
getSystemPromptText :: SystemPrompt -> Text
getSystemPromptText (SystemPrompt txt) = txt

-- | Create a border that shows focus
borderWithFocus :: TuiState -> WidgetName -> Text -> Widget N -> Widget N
borderWithFocus st name label content =
    let labelWidget =
            if focusGetCurrent (st ^. tuiUI . uiFocusRing) == Just name
                then withAttr focusedAttr (txt label)
                else txt label
     in borderWithLabel labelWidget content

-------------------------------------------------------------------------------
-- Event Handling
-------------------------------------------------------------------------------

tui_appHandleEvent :: FilePath -> BrickEvent N AppEvent -> EventM N TuiState ()
tui_appHandleEvent convPrefix ev = do
    case ev of
        -- Application events
        AppEvent (AppEvent_Heartbeat) ->
            handleHeartbeat
        AppEvent (AppEvent_AgentStepProgrress convId sess) -> do
            handleConversationUpdated convId sess
        AppEvent (AppEvent_AgentNeedsInput convId) -> do
            handleConversationNeedsInput convId
        AppEvent (AppEvent_AgentTrace trace) ->
            handleAgentTrace trace

        -- VTY events
        VtyEvent (Vty.EvKey Vty.KEsc _) ->
            halt
        VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) ->
            halt
        VtyEvent (Vty.EvKey (Vty.KChar '\t') _) ->
            cycleFocusForward
        VtyEvent (Vty.EvKey Vty.KBackTab _) ->
            cycleFocusBackward
        VtyEvent (Vty.EvKey (Vty.KFun 5) _) ->
            handleRefreshTools
        VtyEvent (Vty.EvKey (Vty.KChar 'z') _) ->
            toggleZoom
        VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) ->
            handleNewConversationFromEditor convPrefix
        VtyEvent (Vty.EvKey Vty.KEnter [Vty.MMeta]) ->
            handleSendMessage

        -- Delegate to focused widget
        VtyEvent vtyEv -> do
            currentFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
            case currentFocus of
                Just AgentListWidget ->
                    handleAgentListEvent vtyEv
                Just ConversationListWidget ->
                    handleConversationListEvent vtyEv
                Just MessageEditorWidget ->
                    handleMessageEditorEvent ev
                Just ConversationViewWidget ->
                    handleConversationViewEvent vtyEv
                Just AgentInfoWidget ->
                    handleAgentInfoEvent vtyEv
                Just AgentToolsWidget ->
                    handleAgentToolsEvent vtyEv
                Nothing ->
                    pure ()

        _ -> pure ()

-- | Handle agent list navigation
handleAgentListEvent :: Vty.Event -> EventM N TuiState ()
handleAgentListEvent ev = do
    zoom (tuiUI . agentList) $ handleListEvent ev
    -- Update selected agent info when selection changes
    selected <- use (tuiUI . agentList . to listSelectedElement)
    case selected of
        Just (_, agent) -> do
            tuiUI . selectedAgentInfo .= Just agent
        Nothing -> pure ()

-- | Handle conversation list navigation
handleConversationListEvent :: Vty.Event -> EventM N TuiState ()
handleConversationListEvent ev = do
    zoom (tuiUI . conversationList) $ handleListEvent ev
    -- Mark conversation as read when selected
    selected <- use (tuiUI . conversationList . to listSelectedElement)
    case selected of
        Just (_, conv) ->
            tuiUI . unreadConversations %= Set.delete (conversationId conv)
        Nothing -> pure ()

-- | Handle message editor events
handleMessageEditorEvent :: BrickEvent N AppEvent -> EventM N TuiState ()
handleMessageEditorEvent ev = do
    zoom (tuiUI . messageEditor) $ handleEditorEvent ev
    -- Check for special key combinations
    case ev of
        VtyEvent (Vty.EvKey Vty.KEnter mods)
            | Vty.MCtrl `elem` mods -> handleSendMessage
        _ -> pure ()

-- | Handle conversation view scrolling
handleConversationViewEvent :: Vty.Event -> EventM N TuiState ()
handleConversationViewEvent ev =
    case ev of
        Vty.EvKey (Vty.KUp) _ ->
            vScrollBy (viewportScroll ConversationViewWidget) (-1)
        Vty.EvKey (Vty.KDown) _ ->
            vScrollBy (viewportScroll ConversationViewWidget) 1
        Vty.EvKey (Vty.KLeft) _ ->
            hScrollBy (viewportScroll ConversationViewWidget) (-1)
        Vty.EvKey (Vty.KRight) _ ->
            hScrollBy (viewportScroll ConversationViewWidget) 1
        Vty.EvKey (Vty.KPageUp) _ ->
            vScrollPage (viewportScroll ConversationViewWidget) Up
        Vty.EvKey (Vty.KPageDown) _ ->
            vScrollPage (viewportScroll ConversationViewWidget) Down
        _ -> pure ()

-- | Handle agent info scrolling
handleAgentInfoEvent :: Vty.Event -> EventM N TuiState ()
handleAgentInfoEvent ev =
    case ev of
        Vty.EvKey (Vty.KUp) _ ->
            vScrollBy (viewportScroll AgentInfoWidget) (-1)
        Vty.EvKey (Vty.KDown) _ ->
            vScrollBy (viewportScroll AgentInfoWidget) 1
        Vty.EvKey (Vty.KLeft) _ ->
            hScrollBy (viewportScroll AgentInfoWidget) (-1)
        Vty.EvKey (Vty.KRight) _ ->
            hScrollBy (viewportScroll AgentInfoWidget) 1
        _ -> pure ()

-- | Handle agent tools scrolling
handleAgentToolsEvent :: Vty.Event -> EventM N TuiState ()
handleAgentToolsEvent ev =
    case ev of
        Vty.EvKey (Vty.KUp) _ ->
            vScrollBy (viewportScroll AgentToolsWidget) (-1)
        Vty.EvKey (Vty.KDown) _ ->
            vScrollBy (viewportScroll AgentToolsWidget) 1
        Vty.EvKey (Vty.KLeft) _ ->
            hScrollBy (viewportScroll AgentToolsWidget) (-1)
        Vty.EvKey (Vty.KRight) _ ->
            hScrollBy (viewportScroll AgentToolsWidget) 1
        _ -> pure ()

-- | Cycle focus forward through widgets
cycleFocusForward :: EventM N TuiState ()
cycleFocusForward = do
    tuiUI . uiFocusRing %= focusNext
    tuiUI . zoomed .= False

-- | Cycle focus backward through widgets
cycleFocusBackward :: EventM N TuiState ()
cycleFocusBackward = do
    tuiUI . uiFocusRing %= focusPrev
    tuiUI . zoomed .= False

-- | Toggle zoom mode for current widget
toggleZoom :: EventM N TuiState ()
toggleZoom = tuiUI . zoomed %= not

-- | Handle heartbeat - refresh UI state
handleHeartbeat :: EventM N TuiState ()
handleHeartbeat = do
    -- Refresh conversations from core
    coreRef <- use tuiCore
    coreState <- liftIO $ readTVarIO coreRef
    let convs = coreConversations coreState
    tuiUI . conversationList .= list ConversationListWidget (Vector.fromList convs) 1

-- | Handle new conversation event
handleNewConversation :: ConversationId -> EventM N TuiState ()
handleNewConversation convId = do
    -- Find and select the new conversation
    convs <- use (tuiUI . conversationList . to listElements)
    case Vector.findIndex (\c -> conversationId c == convId) convs of
        Just idx -> do
            tuiUI . conversationList . listSelectedL .= Just idx
            tuiUI . unreadConversations %= Set.delete convId
        Nothing -> pure ()

-- | Handle needs input update event
handleConversationNeedsInput :: ConversationId -> EventM N TuiState ()
handleConversationNeedsInput convId = do
    tuiUI . ongoingConversations %= Set.delete convId

-- | Handle conversation update event
handleConversationUpdated :: ConversationId -> Session -> EventM N TuiState ()
handleConversationUpdated convId sess = do
    -- Updates the conversation's view of the Session
    coreRef <- use tuiCore
    liftIO $ atomically $ modifyTVar coreRef $ \c ->
        c{coreConversations = updateConversationSession convId sess (coreConversations c)}

    -- Mark as unread if not currently selected
    selected <- use (tuiUI . conversationList . to listSelectedElement)
    case selected of
        Just (_, conv)
            | conversationId conv /= convId ->
                tuiUI . unreadConversations %= Set.insert convId
        _ -> pure ()
    -- Refresh from core
    handleHeartbeat

-- | Handle agent trace events
-- TODO: remove
handleAgentTrace :: Runtime.Trace -> EventM N TuiState ()
handleAgentTrace trace = do
    case trace of
        _ -> pure ()

-- | Update a conversation's session in the list
updateConversationSession :: ConversationId -> Session -> [Conversation] -> [Conversation]
updateConversationSession convId newSession = map (\c -> if conversationId c == convId then c{conversationSession = Just newSession} else c)

-- | Refresh tools for selected agent
handleRefreshTools :: EventM N TuiState ()
handleRefreshTools = do
    selected <- use (tuiUI . selectedAgentInfo)
    case selected of
        Just agent ->
            liftIO $ void $ atomically $ agent.agentTree.agentRuntime.agentTriggerRefreshTools
        Nothing -> pure ()

-- | Create a new conversation from the selected agent
handleNewConversationFromEditor :: FilePath -> EventM N TuiState ()
handleNewConversationFromEditor convPrefix = do
    selected <- use (tuiUI . agentList . to listSelectedElement)
    case selected of
        Just (_, baseTuiAgent) -> do
            -- * agent will have prompt, tools from the base agent, but will communicate via a pair of chans
            agent0 <- liftIO $ runtimeToAgent (baseTuiAgent.agentTree.agentRuntime)
            convId@(ConversationId cId) <- liftIO $ newConversationId
            let convFilePath = convPrefix <> "conv." <> (show cId) <> ".json"
            outChan <- use eventChan
            let notifyProgress sess = writeBChan outChan (AppEvent_AgentStepProgrress convId sess)
            let notifyNeedInput = writeBChan outChan (AppEvent_AgentNeedsInput convId)
            inChan <- liftIO $ newBChan 100
            let a = agent0 {
                step = \sess -> do
                   storeSession sess convFilePath
                   notifyProgress sess
                   ret <- agent0.step sess
                   case ret of
                      Stop r -> do -- smoll hack to reuse the naive step from runtimeToAgent
                        pure $ AskUserPrompt (MissingUserPrompt True [])
                      _ -> pure ret
              , usrQuery = notifyNeedInput >> readBChan inChan
              }

            -- * wrap in Conversation
            let tuiAgent = TuiAgent a baseTuiAgent.agentTree
            session <- liftIO (Session [] <$> newSessionId <*> newTurnId)
            threadId <- liftIO $ forkIO $ void $ Loop.run a session
            let conv =
                    Conversation
                        { conversationId = convId
                        , conversationAgent = tuiAgent
                        , conversationThreadId = threadId
                        , conversationSession = Nothing
                        , conversationName = "@" <> tuiAgent.agentTree.agentRuntime.agentSlug
                        , conversationChan = inChan
                        }


            -- Add to core
            coreRef <- use tuiCore
            liftIO $ atomically $ modifyTVar coreRef $ \c ->
                c{coreConversations = conv : coreConversations c}
            -- Update UI
            tuiUI . conversationList %= listInsert 0 conv
            tuiUI . conversationList . listSelectedL .= Just 0
            tuiUI . uiFocusRing %= focusRingModify (CList.insertR ConversationViewWidget)
            tuiUI . uiFocusRing %= focusSetCurrent MessageEditorWidget
        Nothing -> pure ()

-- | Send a message in the current conversation
handleSendMessage :: EventM N TuiState ()
handleSendMessage = do
    -- Get message text
    msgLines <- use (tuiUI . messageEditor . to editorGetContents)
    let msgText = Text.strip $ Text.unlines msgLines

    -- Only send non-empty messages
    when (not $ Text.null msgText) $ do
        selected <- use (tuiUI . conversationList . to listSelectedElement)
        case selected of
            Just (idx, conv) -> do
                -- Write message unconditionally
                liftIO $ writeBChan conv.conversationChan (Just $ UserQuery msgText)

                -- Check if conversation is already being processed
                ongoing <- use (tuiUI . ongoingConversations)
                when (not $ Set.member (conversationId conv) ongoing) $ do
                    -- Mark as ongoing
                    tuiUI . ongoingConversations %= Set.insert (conversationId conv)
                    -- Clear editor
                    tuiUI . messageEditor . editContentsL .= TextZipper.textZipper [] Nothing

            Nothing -> pure ()

-- | Update a conversation in the list
updateConversation :: Conversation -> [Conversation] -> [Conversation]
updateConversation conv = map (\c -> if conversationId c == conversationId conv then conv else c)

-- | Replace an item in a list at given index
listReplaceItem :: Int -> a -> List n a -> List n a
listReplaceItem idx item lst =
    let items = listElements lst
     in if idx >= 0 && idx < Vector.length items
            then lst{listElements = items Vector.// [(idx, item)]}
            else lst

-------------------------------------------------------------------------------
-- Cursor and Attributes
-------------------------------------------------------------------------------

tui_appChooseCursor :: TuiState -> [CursorLocation N] -> Maybe (CursorLocation N)
tui_appChooseCursor st locs =
    case focusGetCurrent (st ^. tuiUI . uiFocusRing) of
        Just MessageEditorWidget -> showCursorNamed MessageEditorWidget locs
        _ -> Nothing

tui_appStartEvent :: EventM N TuiState ()
tui_appStartEvent = pure ()

-- | Attribute map for styling
tui_appAttrMap :: TuiState -> AttrMap
tui_appAttrMap _ =
    attrMap
        Vty.defAttr
        [ (focusedAttr, BrickUtil.bg Vty.blue)
        , (listSelectedAttr, Vty.defAttr `Vty.withForeColor` Vty.blue)
        , (userMessageAttr, BrickUtil.fg Vty.green)
        , (llmMessageAttr, BrickUtil.fg Vty.cyan)
        ]

-- | Attribute for focused widgets
focusedAttr :: AttrName
focusedAttr = attrName "focused"

-- | Attribute for user messages
userMessageAttr :: AttrName
userMessageAttr = attrName "userMessage"

-- | Attribute for LLM messages
llmMessageAttr :: AttrName
llmMessageAttr = attrName "llmMessage"

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Get text content from an editor
editorGetContents :: Editor Text n -> [Text]
editorGetContents ed = TextZipper.getText $ ed ^. editContentsL

