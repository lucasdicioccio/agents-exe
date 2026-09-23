{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Draft buffer event handlers for the TUI (@todos/os-as-standalone-server.md@
§5, D3).

The old discrete "queued messages" list is gone: a conversation keeps one
editable 'System.Agents.TUI.Types.Conversation.Draft', appended to by
'System.Agents.TUI.Event.Conversation.sendMessageTo' while the session is
busy and shipped as one 'System.Agents.Protocol.NewMessage' on
@run.stopped@ ('System.Agents.TUI.Event.Conversation.handleRunStopped').
This module covers the remaining user actions on that draft: clearing it,
loading it into the message editor for editing (the Draft tab's
"expanded" view), and "send now".
-}
module System.Agents.TUI.Event.Draft (
    -- * Draft Management
    handleClearDraft,
    handleEditDraft,
    handleSendDraftNow,
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.Edit (editContentsL)
import Control.Lens (to, use, (%=), (.=))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Zipper as TextZipper

import qualified Brick.Widgets.List as List
import System.Agents.TUI.Event.Conversation (setConversationDraft, shipDraftIfAny)
import System.Agents.TUI.Types (
    AppEvent (..),
    Conversation (..),
    N,
    StatusSeverity (..),
    TuiState,
    attachedFiles,
    conversationDraft,
    conversationId,
    conversationList,
    draftIsEmpty,
    draftMedia,
    draftText,
    emptyDraft,
    eventChan,
    messageEditor,
    tuiUI,
 )

-- | Show a status message in the TUI.
showStatus :: StatusSeverity -> Text.Text -> EventM N TuiState ()
showStatus severity text = do
    chan <- use eventChan
    liftIO $ writeBChan chan (AppEvent_ShowStatus severity text)

-- | Get the currently focused conversation in EventM context.
getCurrentConversation :: EventM N TuiState (Maybe Conversation)
getCurrentConversation = do
    mElem <- use (tuiUI . conversationList . to List.listSelectedElement)
    pure $ fmap snd mElem

-------------------------------------------------------------------------------
-- Draft Management
-------------------------------------------------------------------------------

-- | Clear the focused conversation's draft.
handleClearDraft :: EventM N TuiState ()
handleClearDraft = do
    mConv <- getCurrentConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv ->
            if draftIsEmpty (conversationDraft conv)
                then showStatus StatusWarning "No draft to clear"
                else do
                    setConversationDraft (conversationId conv) emptyDraft
                    showStatus StatusInfo "Draft cleared"

{- | Load the focused conversation's draft into the message editor for
editing (the Draft tab's expanded view): its media joins the composer's
attachments, and the draft itself is cleared -- the editor is the single
source of truth for it while being edited, and 'sendMessageTo' folds it
right back into a fresh draft, or posts it, on the next send.
-}
handleEditDraft :: EventM N TuiState ()
handleEditDraft = do
    mConv <- getCurrentConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv -> do
            let d = conversationDraft conv
            if draftIsEmpty d
                then showStatus StatusWarning "No draft to edit"
                else do
                    tuiUI . messageEditor . editContentsL .= TextZipper.textZipper (Text.lines (draftText d)) Nothing
                    tuiUI . attachedFiles %= Map.insertWith (++) (conversationId conv) (draftMedia d)
                    setConversationDraft (conversationId conv) emptyDraft
                    showStatus StatusInfo "Draft loaded into the editor"

-- | Post the focused conversation's draft right away ("Send now", §5).
handleSendDraftNow :: EventM N TuiState ()
handleSendDraftNow = do
    mConv <- getCurrentConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv ->
            if draftIsEmpty (conversationDraft conv)
                then showStatus StatusWarning "No draft to send"
                else shipDraftIfAny (conversationId conv)
