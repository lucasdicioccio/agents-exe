{-# LANGUAGE OverloadedStrings #-}

{- | Round-trip JSON tests for "System.Agents.Protocol": every 'EventBody'
constructor, plus 'RunMode', 'NewMessage', 'RunnerError', 'DeleteMode',
'DeletionPlan' and 'SubscribeScope'.
-}
module ProtocolTests (tests) where

import qualified Data.Aeson as Aeson
import Data.Time (getCurrentTime)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Protocol
import System.Agents.Session.Types (
    DeferredCallView (..),
    LlmToolCall (..),
    Reason (..),
    SessionStatus (..),
    SystemPrompt (..),
    ToolCallDisposition (..),
    ToolCallId (..),
    Turn (..),
    UserTurnContent (..),
    newContinuationToken,
    newToolCallId,
 )
import System.Agents.Session.Base (SessionId (..), newSessionId)
import System.Agents.SessionStore (SessionMeta (..), freshSessionMeta)
import qualified Data.UUID as UUID

tests :: TestTree
tests =
    testGroup
        "Protocol JSON round trips"
        [ testCase "RunMode: StepOnce" (roundTrip StepOnce)
        , testCase "RunMode: UntilBlocked" (roundTrip UntilBlocked)
        , testCase "NewMessage" (roundTrip (NewMessage "hi" [] False))
        , testCase "NewMessage: interrupt" (roundTrip (NewMessage "hi" [] True))
        , testCase "DeleteMode: DryRun" (roundTrip DryRun)
        , testCase "DeleteMode: DeleteForReal" (roundTrip DeleteForReal)
        , testCase "DeletionPlan" $ do
            sid <- newSessionId
            roundTrip (DeletionPlan [sid] 2 True)
        , testCase "SubscribeScope: OneSession" $ do
            sid <- newSessionId
            roundTrip (OneSession sid)
        , testCase "SubscribeScope: Owner" (roundTrip (Owner (Just "alice")))
        , testCase "SubscribeScope: Owner Nothing" (roundTrip (Owner Nothing))
        , testCase "SubscribeScope: AllSessions" (roundTrip AllSessions)
        , testCase "RunnerError codes survive re-encoding" runnerErrorCodeTest
        , testCase "Event: RunStarted" (eventRoundTrip (RunStarted StepOnce))
        , testCase "Event: SessionUpdated" sessionUpdatedTest
        , testCase "Event: CallsDeferred" callsDeferredTest
        , testCase "Event: RunStopped" (eventRoundTrip (RunStopped StatusIdle))
        , testCase "Event: SessionFailed" (eventRoundTrip (SessionFailed "boom"))
        , testCase "Event: TextDelta" (eventRoundTrip (TextDelta "chunk"))
        , testCase "Event: ToolCallStarted" $ do
            callId <- newToolCallId
            eventRoundTrip (ToolCallStarted callId "search")
        , testCase "Event: ToolCallCompleted" $ do
            callId <- newToolCallId
            eventRoundTrip (ToolCallCompleted callId "search" True)
        , testCase "Event: SessionCreated" sessionCreatedTest
        , testCase "Event: SessionDeleted" $ do
            sid <- newSessionId
            roundTrip (Event (EventSeq 42) (Just sid) (Just "alice") (SessionDeleted sid))
        , testCase "eventKind matches every kind string" eventKindTest
        ]

roundTrip :: (Eq a, Show a, Aeson.ToJSON a, Aeson.FromJSON a) => a -> Assertion
roundTrip x = Aeson.decode (Aeson.encode x) @?= Just x

-- | An 'Event' round trip: build one with a fixed seq/session/owner around
-- the given body, encode, decode, compare.
eventRoundTrip :: EventBody -> Assertion
eventRoundTrip body = do
    sid <- newSessionId
    let ev = Event (EventSeq 42) (Just sid) (Just "alice") body
    roundTrip ev

{- | 'SessionUpdated'\/'SessionCreated' carry a full 'SessionMeta', whose own
JSON already has @session_id@\/@owner@ fields; the 'Event' wrapper's
'evSession'\/'evOwner' merge over the same keys (see 'withFields'), so a
round trip is only meaningful with matching values -- exactly what the
runner's 'emit' always produces (the event and the session it is about are
the same session). A mismatched wrapper is not a shape this module ever
constructs.
-}
sessionUpdatedTest :: Assertion
sessionUpdatedTest = do
    sid <- newSessionId
    now <- getCurrentTime
    let meta = (freshSessionMeta sid now){smOwner = Just "alice"}
        turn = UserTurn (UserTurnContent (SystemPrompt "sys") [] Nothing [] []) Nothing
        ev withTurn = Event (EventSeq 42) (Just sid) (Just "alice") (SessionUpdated meta withTurn)
    roundTrip (ev (Just turn))
    roundTrip (ev Nothing)

sessionCreatedTest :: Assertion
sessionCreatedTest = do
    sid <- newSessionId
    now <- getCurrentTime
    let meta = (freshSessionMeta sid now){smOwner = Just "alice"}
    roundTrip (Event (EventSeq 42) (Just sid) (Just "alice") (SessionCreated meta))

callsDeferredTest :: Assertion
callsDeferredTest = do
    callId <- newToolCallId
    token <- newContinuationToken
    let call = DeferredCallView callId (Just token) "search" (Defer (Reason "external")) (LlmToolCall Aeson.Null)
    eventRoundTrip (CallsDeferred [call])
    eventRoundTrip (CallsDeferred [])

-- | Every 'RunnerError' constructor, re-encoded: the code must survive even
-- though the structured payload (session id, param names, ...) does not --
-- see the note on 'Aeson.FromJSON' 'RunnerError'.
runnerErrorCodeTest :: Assertion
runnerErrorCodeTest = do
    sid <- newSessionId
    token <- newContinuationToken
    mapM_
        checkCode
        [ UnknownAgent "slug"
        , UnknownSession sid
        , UnknownToken token
        , TokenAlreadyCompleted token
        , RunInProgress sid
        , NoActiveRun sid
        , NotAcceptingMessages sid StatusRunning
        , UnknownParams ["p"]
        , ForbiddenParams ["p"]
        , InvalidParams ["p"]
        , MissingRequiredParams ["p"]
        , MailboxRejected sid
        ]
  where
    checkCode e = do
        let encoded = Aeson.encode e
            decoded = Aeson.decode encoded :: Maybe RunnerError
        fmap runnerErrorCode decoded @?= Just (runnerErrorCode e)

-- | Every 'EventBody' constructor's 'eventKind' matches the SSE @event:@
-- name the server has always used (see @docs/agents-server.md@).
eventKindTest :: Assertion
eventKindTest = do
    let sid = SessionId UUID.nil
    eventKind (RunStarted StepOnce) @?= "run.started"
    eventKind (RunStopped StatusIdle) @?= "run.stopped"
    eventKind (CallsDeferred []) @?= "calls.deferred"
    eventKind (SessionFailed "x") @?= "session.failed"
    eventKind (TextDelta "x") @?= "text.delta"
    eventKind (SessionDeleted sid) @?= "session.deleted"
