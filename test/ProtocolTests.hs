{-# LANGUAGE OverloadedStrings #-}

{- | Round-trip JSON tests for "System.Agents.Protocol": every 'EventBody'
constructor, plus 'RunMode', 'NewMessage', 'RunnerError', 'DeleteMode',
'DeletionPlan' and 'SubscribeScope'.
-}
module ProtocolTests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Time (NominalDiffTime, getCurrentTime)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Base (ConversationId (..))
import System.Agents.OS.Events (ToolCallActivity (..))
import qualified System.Agents.OS.Events as OSEvents
import System.Agents.Protocol
import System.Agents.Session.Types (
    ContinuationToken,
    ControlMsg (StopRun),
    DeferredCallView (..),
    Envelope (..),
    LlmToolCall (..),
    MailBody (..),
    Priority (..),
    Reason (..),
    Receipt (..),
    Sender (..),
    SessionStatus (..),
    SystemPrompt (..),
    ToolCallDisposition (..),
    ToolCallId (..),
    Turn (..),
    UserQuery (..),
    UserToolResponse (..),
    UserTurnContent (..),
    newContinuationToken,
    newMessageId,
    newToolCallId,
 )
import System.Agents.Session.Base (Session (..), SessionId (..), TurnId (..), newSessionId)
import System.Agents.SessionStore (SessionMeta (..), SessionQuery (..), allSessionsQuery, freshSessionMeta)
import System.Agents.Tools.Activation (Activation (..))
import System.Agents.Tools.Params.Types (ParamScope (..))
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
        , testCase "Event: SubcallStarted" $ do
            parent <- newSessionId
            child <- newSessionId
            eventRoundTripAt parent (SubcallStarted parent child "helper" 1)
        , testCase "Event: SubcallCompleted" $ do
            parent <- newSessionId
            child <- newSessionId
            eventRoundTripAt parent (SubcallCompleted child (Just "done"))
        , testCase "Event: SubcallCompleted, no result" $ do
            parent <- newSessionId
            child <- newSessionId
            eventRoundTripAt parent (SubcallCompleted child Nothing)
        , testCase "Event: SubcallFailed" $ do
            parent <- newSessionId
            child <- newSessionId
            eventRoundTripAt parent (SubcallFailed child "boom")
        , testCase "Event: ToolCallProgressed" toolCallProgressedTest
        , testCase "eventKind matches every kind string" eventKindTest
        , testCase "RunnerStats" (roundTrip (RunnerStats 3 1))
        , testCase "AgentParameter" $ do
            roundTrip (AgentParameter "tenant" (Just "which tenant") True ScopeSession True False True)
            roundTrip (AgentParameter "tenant" Nothing False ScopeProcess False True True)
        , testCase "ToolDescriptor: default activation" (roundTrip (ToolDescriptor "search" "search the web" Nothing))
        , testCase "ToolDescriptor: always activated" (roundTrip (ToolDescriptor "search" "search the web" (Just AlwaysActivated)))
        , testCase "ToolDescriptor: on-demand" (roundTrip (ToolDescriptor "search" "search the web" (Just (OnDemandActivated "web"))))
        , testCase "AgentDescriptor: file agent" (roundTrip =<< fileAgentDescriptor)
        , testCase "AgentDescriptor: database agent" (roundTrip =<< databaseAgentDescriptor)
        , testCase "Command: every constructor" commandRoundTripTest
        , testCase "Reply: every constructor" replyRoundTripTest
        , testCase "RunnerError: UnexpectedReply code survives" $ do
            let decoded = Aeson.decode (Aeson.encode UnexpectedReply) :: Maybe RunnerError
            fmap runnerErrorCode decoded @?= Just (runnerErrorCode UnexpectedReply)
        ]

-- | A minimal 'AgentDescriptor' for a file-based agent.
fileAgentDescriptor :: IO AgentDescriptor
fileAgentDescriptor =
    pure $
        AgentDescriptor
            { adSlug = "helper"
            , adDescription = "a helper agent"
            , adModel = "gpt-4"
            , adSystemPrompt = ["You are a helper"]
            , adSource = "file"
            , adTools = [ToolDescriptor "search" "search the web" (Just AlwaysActivated)]
            , adParameters = [AgentParameter "tenant" (Just "which tenant") True ScopeSession True False True]
            , adHelpers = ["sub-helper"]
            , adUpdatedAt = Nothing
            , adUpdatedBy = Nothing
            , adConfig = Nothing
            }

-- | Like 'fileAgentDescriptor', for a database-stored agent (with the
-- extra @updated_at@\/@updated_by@\/@config@ fields).
databaseAgentDescriptor :: IO AgentDescriptor
databaseAgentDescriptor = do
    now <- getCurrentTime
    base <- fileAgentDescriptor
    pure
        base
            { adSource = "database"
            , adUpdatedAt = Just now
            , adUpdatedBy = Just "alice"
            , adConfig = Just (Aeson.object ["slug" Aeson..= Aeson.String "helper"])
            }


roundTrip :: (Eq a, Show a, Aeson.ToJSON a, Aeson.FromJSON a) => a -> Assertion
roundTrip x = Aeson.decode (Aeson.encode x) @?= Just x

-- | An 'Event' round trip: build one with a fixed seq/session/owner around
-- the given body, encode, decode, compare.
eventRoundTrip :: EventBody -> Assertion
eventRoundTrip body = do
    sid <- newSessionId
    let ev = Event (EventSeq 42) (Just sid) (Just "alice") body
    roundTrip ev

{- | Like 'eventRoundTrip', but with an explicit 'evSession' -- needed for
'SubcallStarted'\/'SubcallCompleted'\/'SubcallFailed', whose own payload
names a *child* session distinct from 'evSession' (the parent the runner
actually 'emit's on; see 'System.Agents.Protocol.bodyPairs'\'s note on
"child_session_id" vs. the top-level "session_id").
-}
eventRoundTripAt :: SessionId -> EventBody -> Assertion
eventRoundTripAt sid body = roundTrip (Event (EventSeq 42) (Just sid) (Just "alice") body)

-- | 'ToolCallProgressed' carries a 'ToolCallActivity' whose own
-- 'tcaSessionId' is the same session the event is about, so (unlike the
-- subcall events) there is no 'evSession' collision to route around.
toolCallProgressedTest :: Assertion
toolCallProgressedTest = do
    sid <- newSessionId
    callId <- newToolCallId
    now <- getCurrentTime
    let mkActivity phase =
            ToolCallActivity
                { tcaSessionId = sid
                , tcaConversationId = ConversationId (UUID.fromWords 1 2 3 4)
                , tcaToolCallId = callId
                , tcaProviderCallId = Just "call_1"
                , tcaToolName = "search"
                , tcaPhase = phase
                , tcaAt = now
                }
    mapM_
        (\phase -> eventRoundTripAt sid (ToolCallProgressed (mkActivity phase)))
        [ OSEvents.ToolCallStarted
        , OSEvents.ToolCallProgressed (Aeson.object ["pct" Aeson..= (50 :: Int)])
        , OSEvents.ToolCallCompleted
        , OSEvents.ToolCallFailed "timed out"
        , OSEvents.ToolCallCancelled
        ]

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
        , UnknownTurn sid 3
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

-------------------------------------------------------------------------------
-- Command / Reply
-------------------------------------------------------------------------------

-- | Every 'Command' constructor, round-tripped through JSON.
commandRoundTripTest :: Assertion
commandRoundTripTest = do
    sid <- newSessionId
    parent <- newSessionId
    token <- newContinuationToken
    let msg = NewMessage "hi" [] False
        params = Map.fromList [("tenant", Aeson.String "acme")]
    mapM_
        roundTrip
        [ CreateSession Nothing "helper" (Just msg) (Just StepOnce) params
        , CreateSession (Just parent) "helper" Nothing Nothing Map.empty
        , PostMessage sid msg (Just UntilBlocked) params
        , Resume sid StepOnce params
        , CompleteCall token (TextResponse "42") True params
        , CancelRun sid
        , CancelAttached sid
        , Pause sid
        , SendMail sid Normal (Control StopRun)
        , SendMail sid Interrupt (UserMessage (UserQuery "hi" []))
        , ListMail sid True
        , ForkSession sid (Just 2) (Just "other-agent")
        , ForkSession sid Nothing Nothing
        , ListSessions allSessionsQuery{sqOwner = Just "alice"}
        , GetSession sid
        , ListAgents
        , GetAgent "helper"
        , DeleteSession sid DryRun
        , AwaitRun sid (5 :: NominalDiffTime)
        , Stats
        ]
    roundTrip (SpawnSession parent "helper" msg)

-- | Every 'Reply' constructor, round-tripped through JSON.
replyRoundTripTest :: Assertion
replyRoundTripTest = do
    sid <- newSessionId
    now <- getCurrentTime
    mid <- newMessageId
    let meta = (freshSessionMeta sid now){smOwner = Just "alice"}
        session = testSession sid
        envelope = Envelope mid 1 (FromUser (Just "alice")) Normal 0 now (Control StopRun)
        receipt = Receipt mid 1 False
    agent <- fileAgentDescriptor
    mapM_
        roundTrip
        [ RSessionMeta meta
        , RSessions [meta]
        , RMail [envelope]
        , RReceipt receipt
        , RAgents [agent]
        , RAgent agent
        , RDeletion (DeletionPlan [sid] 1 False)
        , RUnit
        , RStats (RunnerStats 2 1)
        , RAwait meta True
        ]
    roundTrip (RSession session meta)
  where
    testSession sid =
        Session
            { turns = []
            , sessionId = sid
            , forkedFromSessionId = Nothing
            , turnId = TurnId UUID.nil
            , sessionVersion = Just 2
            , sessionExecutionMode = Nothing
            , mailCursor = 0
            }
