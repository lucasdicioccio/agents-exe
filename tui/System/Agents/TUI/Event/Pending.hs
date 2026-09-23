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
    handleSendOrAnswer,
) where

import Brick
import Brick.Widgets.Edit (getEditContents)
import Control.Lens (to, use, (.=), (^.))
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import qualified Data.Text as Text

import qualified Brick.Widgets.List as List
import qualified System.Agents.Host.Client as Client
import System.Agents.Session.Base (DeferredCallView (..), UserToolResponse (..))
import System.Agents.TUI.Event.Conversation (
    clearEditorAndAttachments,
    handleSendMessage,
    reportRunnerResult,
    readCore,
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
    coreParams,
    eventChan,
    messageEditor,
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
conversation's oldest deferred call that carries a continuation token (a
call deferred with no token, if that ever happens, cannot be completed
from here). The next @send-message@ ('handleSendOrAnswer') completes it.
-}
handleAnswerPending :: EventM N TuiState ()
handleAnswerPending = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv -> case find (\c -> dcvToken c /= Nothing) conv.conversationPending of
            Nothing -> showStatus StatusWarning "No pending call with a continuation token"
            Just call -> do
                tuiUI . answeringPendingCall .= Just (conv.conversationId, call)
                showStatus StatusInfo $
                    "Answering " <> call.dcvToolName <> ": type the result, then send"

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
                mConv <- getFocusedConversation
                mapM_ clearEditorAndAttachments mConv
                reportRunnerResult ("Answered " <> call.dcvToolName <> " for " <> convName convId mConv) result
  where
    convName _ (Just conv) = conversationName conv
    convName convId Nothing = Text.pack (show convId)
