{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pure tests for the TUI's draft buffer state machine
(@todos/os-as-standalone-server.md@ §5, D3): 'appendDraft', 'shouldShipDraft',
'draftToMessage'. None of this touches a runner or the wire -- a draft
never crosses it (D3) -- so it is exercised directly, without a fixture.
-}
module TuiDraftTests (tests) where

import qualified Data.UUID as UUID
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Base (ConversationId (..))
import System.Agents.SessionStore (conversationIdToSessionId)
import System.Agents.TUI.Render (addThousandSeparators)

import System.Agents.Media.Types (MediaAttachment (..))
import System.Agents.Protocol (NewMessage (..))
import System.Agents.Session.Base (SessionStatus (..))
import System.Agents.TUI.Types (
    Conversation (..),
    ConversationStatus (..),
    Draft (..),
    appendDraft,
    currentConversation,
    draftFirstLine,
    draftIsEmpty,
    draftParagraphCount,
    draftToMessage,
    emptyDraft,
    shouldShipDraft,
 )

tests :: TestTree
tests =
    testGroup
        "TUI draft buffer (§5, D3)"
        [ testCase "emptyDraft is empty" $ draftIsEmpty emptyDraft @?= True
        , testCase "appendDraft on an empty draft sets the text" $
            draftText (appendDraft "hello" [] emptyDraft) @?= "hello"
        , testCase "appendDraft joins successive paragraphs with a blank line" $ do
            let d1 = appendDraft "first" [] emptyDraft
                d2 = appendDraft "second" [] d1
                d3 = appendDraft "third" [] d2
            draftText d3 @?= "first\n\nsecond\n\nthird"
            draftParagraphCount d3 @?= 3
        , testCase "appendDraft strips surrounding whitespace from the new paragraph" $
            draftText (appendDraft "  padded  " [] emptyDraft) @?= "padded"
        , testCase "appendDraft with empty text is a no-op on the text, but still appends media" $ do
            let att = MediaAttachment{mediaMimeType = "image/png", mediaBase64Data = "AA==", mediaFilename = Nothing}
                d = appendDraft "" [att] emptyDraft
            draftText d @?= ""
            draftMedia d @?= [att]
            draftIsEmpty d @?= False
        , testCase "appendDraft concatenates media across calls" $ do
            let att1 = MediaAttachment{mediaMimeType = "image/png", mediaBase64Data = "AA==", mediaFilename = Just "a.png"}
                att2 = MediaAttachment{mediaMimeType = "image/png", mediaBase64Data = "BB==", mediaFilename = Just "b.png"}
                d = appendDraft "second" [att2] (appendDraft "first" [att1] emptyDraft)
            draftMedia d @?= [att1, att2]
        , testCase "draftFirstLine is the first line of a multi-paragraph draft" $
            draftFirstLine (appendDraft "line one\nmore" [] (appendDraft "line zero" [] emptyDraft)) @?= "line zero"
        , testCase "draftToMessage of an empty draft is Nothing" $
            draftToMessage emptyDraft @?= Nothing
        , testCase "draftToMessage joins paragraphs, carries media, never interrupts" $ do
            let att = MediaAttachment{mediaMimeType = "text/plain", mediaBase64Data = "aGk=", mediaFilename = Nothing}
                d = appendDraft "second" [att] (appendDraft "first" [] emptyDraft)
                Just nm = draftToMessage d
            nmText nm @?= "first\n\nsecond"
            nmMedia nm @?= [att]
            nmInterrupt nm @?= False
        , testCase "shouldShipDraft is True only for idle/ready statuses" $ do
            shouldShipDraft StatusIdle @?= True
            shouldShipDraft StatusReady @?= True
            shouldShipDraft StatusRunning @?= False
            shouldShipDraft StatusPaused @?= False
            shouldShipDraft StatusWaitingExternal @?= False
            shouldShipDraft StatusFailed @?= False
        , testCase "currentConversation prefers the stored conversation over a stale copy" $ do
            let stale = mkConv 1
                fresh = (mkConv 1){conversationDraft = appendDraft "kept" [] emptyDraft, conversationStatus = ConversationStatus_Active}
                found = currentConversation [mkConv 2, fresh] stale
            draftText (conversationDraft found) @?= "kept"
            conversationStatus found @?= ConversationStatus_Active
        , testCase "currentConversation keeps the given conversation when it is not stored" $
            conversationId (currentConversation [mkConv 2] (mkConv 1)) @?= conversationId (mkConv 1)
        , testCase "addThousandSeparators keeps the digits in order" $ do
            addThousandSeparators "7" @?= "7"
            addThousandSeparators "10" @?= "10"
            addThousandSeparators "999" @?= "999"
            addThousandSeparators "1234" @?= "1,234"
            addThousandSeparators "1234567" @?= "1,234,567"
        ]

mkConv :: Int -> Conversation
mkConv n =
    Conversation
        { conversationId = cid
        , conversationSessionId = conversationIdToSessionId cid
        , conversationAgentSlug = "agent"
        , conversationSession = Nothing
        , conversationMeta = Nothing
        , conversationName = "@agent"
        , conversationStatus = ConversationStatus_WaitingForInput
        , conversationIsSubcall = False
        , conversationParentId = Nothing
        , conversationSubcallDepth = 0
        , conversationDraft = emptyDraft
        , conversationPending = []
        }
  where
    cid = ConversationId (UUID.fromWords 0 0 0 (fromIntegral n))
