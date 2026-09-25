{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pending-calls event handlers for the TUI (Phase 3c,
@todos/os-as-standalone-server.md@ Design §4).

A conversation's 'conversationPending' (set from @calls.deferred@,
cleared on @run.started@) is the Pending panel's source of truth. This
module covers answering one of those calls: 'handleAnswerPending' puts the
message editor into "answer mode" for the oldest pending call with a
continuation token, and 'handleSendOrAnswer' -- installed in place of
'System.Agents.TUI.Event.Conversation.handleSendMessage' on the
@send-message@ keybinding -- completes that call
('System.Agents.Host.Client.completeCall', with @autoResume = True@ so the
run continues the way the server's own @/v1/sessions/:id/pending@ workers
do) instead of posting a normal message, whenever that mode is active.
-}
module System.Agents.TUI.Event.Pending (
    handleAnswerPending,
    handleSelectPending,
    handleFailPending,
    handleSendOrAnswer,
) where

import Brick
import Brick.Widgets.Edit (getEditContents)
import Control.Lens (to, use, (.=), (^.))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import Control.Monad (forM_, when)

import qualified Brick.Widgets.List as List
import qualified System.Agents.Host.Client as Client
import System.Agents.Base (ConversationId)
import System.Agents.Session.Base (ContinuationToken, DeferredCallView (..), UserToolResponse (..))
import System.Agents.TUI.Event.Conversation (
    clearEditorAndAttachments,
    handleSendMessage,
    reportRunnerResult,
    readCore,
    setConversationPending,
 )
import System.Agents.TUI.Types (
    AppEvent (..),
    Conversation (..),
    N,
    StatusSeverity (..),
    TuiState,
    answeringPendingCall,
    conversationId,
    conversationList,
    conversationName,
    coreClient,
    coreConversations,
    coreParams,
    eventChan,
    failedCallText,
    messageEditor,
    nextPendingToken,
    selectedPendingCall,
    selectedPendingToken,
    tuiUI,
 )
import Brick.BChan (writeBChan)

-- | Show a status message in the TUI.
showStatus :: StatusSeverity -> Text.Text -> EventM N TuiState ()
showStatus severity text = do
    chan <- use eventChan
    liftIO $ writeBChan chan (AppEvent_ShowStatus severity text)

-- | Get the currently focused conversation, if any.
getFocusedConversation :: EventM N TuiState (Maybe Conversation)
getFocusedConversation = do
    mConv <- use (tuiUI . conversationList . to List.listSelectedElement)
    pure $ fmap snd mConv

{- | Put the message editor into "answer mode" for the focused
conversation's selected deferred call ('handleSelectPending'; the first one
that carries a continuation token unless another was selected; a call
deferred with no token, if that ever happens, cannot be completed from here). The next @send-message@ ('handleSendOrAnswer') completes it.
-}
handleAnswerPending :: EventM N TuiState ()
handleAnswerPending = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv -> do
            selected <- use (tuiUI . selectedPendingToken)
            case selectedPendingCall selected conv.conversationPending of
                Nothing -> showStatus StatusWarning "No pending call with a continuation token"
                Just call -> do
                    tuiUI . answeringPendingCall .= Just (conv.conversationId, call)
                    showStatus StatusInfo $
                        "Answering " <> call.dcvToolName <> ": type the result, then send"

{- | Forget a call the runner has just accepted a result for. The panel's list
is otherwise only replaced by the next @calls.deferred@, which never comes
while other calls are still pending (nothing can progress, so no run starts):
without this the completed call would stay listed, and selected.
-}
dropPendingCall :: ConversationId -> ContinuationToken -> EventM N TuiState ()
dropPendingCall convId token = do
    core <- readCore
    forM_ [c | c <- core ^. coreConversations, conversationId c == convId] $ \conv ->
        setConversationPending convId (filter ((/= Just token) . dcvToken) conv.conversationPending)
    tuiUI . selectedPendingToken .= Nothing

{- | Move the Pending panel's selection to the next call that can be
completed, wrapping around; answer-pending and fail-pending act on it.
-}
handleSelectPending :: EventM N TuiState ()
handleSelectPending = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv -> do
            selected <- use (tuiUI . selectedPendingToken)
            case nextPendingToken selected conv.conversationPending of
                Nothing -> showStatus StatusWarning "No pending call with a continuation token"
                Just tok -> do
                    tuiUI . selectedPendingToken .= Just tok
                    tuiUI . answeringPendingCall .= Nothing
                    case selectedPendingCall (Just tok) conv.conversationPending of
                        Just call -> showStatus StatusInfo ("Selected pending call: " <> call.dcvToolName)
                        Nothing -> pure ()

{- | Fail the selected pending call: complete it with an error text
('failedCallText') the model reads as the call's result, the same way a
tool's own error reaches it, so no new response type is needed. The reason
is the message editor's text, if any (which is then cleared); otherwise a
generic one. Like answering, it goes through 'Client.completeCall' with
@autoResume = True@, so it works over an embedded runner and with @--attach@.
-}
handleFailPending :: EventM N TuiState ()
handleFailPending = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv -> do
            selected <- use (tuiUI . selectedPendingToken)
            case selectedPendingCall selected conv.conversationPending of
                Nothing -> showStatus StatusWarning "No pending call with a continuation token"
                Just call -> case call.dcvToken of
                    Nothing -> showStatus StatusError "That pending call has no continuation token"
                    Just token -> do
                        msgLines <- use (tuiUI . messageEditor . to getEditContents)
                        core <- readCore
                        result <-
                            liftIO $
                                Client.completeCall
                                    (core ^. coreClient)
                                    token
                                    (TextResponse (failedCallText (Text.intercalate "\n" msgLines)))
                                    True
                                    (core ^. coreParams)
                        tuiUI . answeringPendingCall .= Nothing
                        when (either (const False) (const True) result) $ dropPendingCall conv.conversationId token
                        clearEditorAndAttachments conv
                        reportRunnerResult ("Failed " <> call.dcvToolName <> " for " <> conversationName conv) result

{- | @send-message@'s actual behaviour: complete the pending call
'handleAnswerPending' selected, if the editor is in that mode; otherwise
the ordinary 'handleSendMessage'.
-}
handleSendOrAnswer :: EventM N TuiState ()
handleSendOrAnswer = do
    mAnswering <- use (tuiUI . answeringPendingCall)
    case mAnswering of
        Nothing -> handleSendMessage
        Just (convId, call) -> case call.dcvToken of
            Nothing -> do
                tuiUI . answeringPendingCall .= Nothing
                showStatus StatusError "That pending call lost its continuation token; nothing sent"
            Just token -> do
                msgLines <- use (tuiUI . messageEditor . to getEditContents)
                let msgText = Text.strip (Text.intercalate "\n" msgLines)
                core <- readCore
                result <-
                    liftIO $
                        Client.completeCall
                            (core ^. coreClient)
                            token
                            (TextResponse msgText)
                            True
                            (core ^. coreParams)
                tuiUI . answeringPendingCall .= Nothing
                when (either (const False) (const True) result) $ dropPendingCall convId token
                mConv <- getFocusedConversation
                mapM_ clearEditorAndAttachments mConv
                reportRunnerResult ("Answered " <> call.dcvToolName <> " for " <> convName convId mConv) result
  where
    convName _ (Just conv) = conversationName conv
    convName convId Nothing = Text.pack (show convId)
