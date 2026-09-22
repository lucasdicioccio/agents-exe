{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Unit tests for the in-memory session mailbox (Phase 1 of
@todos/session-mailbox.md@): 'System.Agents.Session.Mailbox'.

Covers the three things "transactional" means for this mailbox (§1):
accept is atomic and idempotent, reading never consumes, and 'awaitMail'
blocks/filters/composes as an ordinary STM action.
-}
module MailboxTests where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Monad (forM_, replicateM)
import Data.List (sort)
import Data.UUID (nil)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson (decode, encode)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Database.SQLite.Simple (open)

import System.Agents.Session.Base
import System.Agents.Session.MailStore (mkSqliteMailStore)
import System.Agents.Session.Types (ToolCallId (..))

tests :: TestTree
tests =
    testGroup
        "Session Mailbox"
        [ sendIsIdempotentTest
        , sendWithoutIdGeneratesFreshOnesTest
        , unreadRespectsCursorTest
        , unreadNeverConsumesTest
        , awaitMailBlocksUntilMatchTest
        , awaitMailFiltersByPredicateTest
        , mailboxFullRejectsNormalMailTest
        , mailboxFullExemptsControlAndToolCallFinishedTest
        , trimRemovesAtOrBelowCursorTest
        , durableMailboxHydratesFromStoreTest
        , durableMailboxWritesThroughTest
        , sqliteMailStoreRoundTripsTest
        , wakeOnKindJsonRoundTripTest
        , senderWakeKindClassifiesSendersTest
        ]

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | A minimal 'UserMessage' envelope from the user, with no explicit id.
userOutgoing :: Outgoing
userOutgoing =
    Outgoing
        { outId = Nothing
        , outFrom = FromUser Nothing
        , outPriority = Normal
        , outHops = 0
        , outBody = UserMessage (UserQuery "hi" [])
        }

expectRight :: (Show e) => Either e a -> IO a
expectRight (Right a) = pure a
expectRight (Left e) = assertFailure ("expected Right, got Left " <> show e) >> fail "unreachable"

-------------------------------------------------------------------------------
-- Accept is atomic and idempotent
-------------------------------------------------------------------------------

sendIsIdempotentTest :: TestTree
sendIsIdempotentTest =
    testCase "resending the same envId returns the original receipt" $ do
        mb <- newInMemoryMailbox
        mid <- newMessageId
        let outgoing = userOutgoing{outId = Just mid}
        r1 <- expectRight =<< mb.mbSend outgoing
        r2 <- expectRight =<< mb.mbSend outgoing
        r1.rcptDuplicate @?= False
        r2.rcptDuplicate @?= True
        r1.rcptId @?= mid
        r2.rcptId @?= mid
        r1.rcptSeq @?= r2.rcptSeq
        unread <- atomically (mb.mbUnread 0)
        length unread @?= 1

sendWithoutIdGeneratesFreshOnesTest :: TestTree
sendWithoutIdGeneratesFreshOnesTest =
    testCase "sends without an explicit id are never treated as duplicates" $ do
        mb <- newInMemoryMailbox
        _ <- expectRight =<< mb.mbSend userOutgoing
        _ <- expectRight =<< mb.mbSend userOutgoing
        unread <- atomically (mb.mbUnread 0)
        length unread @?= 2

-------------------------------------------------------------------------------
-- Reading does not consume
-------------------------------------------------------------------------------

unreadRespectsCursorTest :: TestTree
unreadRespectsCursorTest =
    testCase "mbUnread returns only envelopes past the given cursor" $ do
        mb <- newInMemoryMailbox
        receipts <- replicateM 3 (expectRight =<< mb.mbSend userOutgoing)
        let seqs = sort (map rcptSeq receipts)
        allUnread <- atomically (mb.mbUnread 0)
        length allUnread @?= 3
        laterUnread <- atomically (mb.mbUnread (seqs !! 1))
        length laterUnread @?= 1
        noneUnread <- atomically (mb.mbUnread (last seqs))
        noneUnread @?= []

unreadNeverConsumesTest :: TestTree
unreadNeverConsumesTest =
    testCase "reading mbUnread twice with the same cursor returns the same envelopes" $ do
        mb <- newInMemoryMailbox
        _ <- expectRight =<< mb.mbSend userOutgoing
        first <- atomically (mb.mbUnread 0)
        second <- atomically (mb.mbUnread 0)
        map envId first @?= map envId second

-------------------------------------------------------------------------------
-- awaitMail: blocking, filtering, composing
-------------------------------------------------------------------------------

awaitMailBlocksUntilMatchTest :: TestTree
awaitMailBlocksUntilMatchTest =
    testCase "awaitMail blocks (via STM retry) until mail arrives" $ do
        mb <- newInMemoryMailbox
        started <- newEmptyMVar
        result <- newEmptyMVar
        _ <- forkIO $ do
            putMVar started ()
            envelopes <- atomically (awaitMail mb 0 (const True))
            putMVar result envelopes
        takeMVar started
        -- Nothing has been sent yet: the waiter must still be blocked.
        stillWaiting <- timeout (200 * 1000) (takeMVar result)
        stillWaiting @?= Nothing
        _ <- expectRight =<< mb.mbSend userOutgoing
        delivered <- timeout (5 * 1000 * 1000) (takeMVar result)
        case delivered of
            Nothing -> assertFailure "awaitMail never woke up after mail arrived"
            Just envelopes -> length envelopes @?= 1

awaitMailFiltersByPredicateTest :: TestTree
awaitMailFiltersByPredicateTest =
    testCase "awaitMail only wakes on envelopes matching its predicate" $ do
        mb <- newInMemoryMailbox
        let isUserMessage :: Envelope -> Bool
            isUserMessage e = case e.envBody of
                UserMessage _ -> True
                _ -> False
        let controlOutgoing =
                Outgoing
                    { outId = Nothing
                    , outFrom = FromSystem "test"
                    , outPriority = Normal
                    , outHops = 0
                    , outBody = Control Pause
                    }
        result <- newEmptyMVar
        _ <- forkIO $ do
            envelopes <- atomically (awaitMail mb 0 isUserMessage)
            putMVar result envelopes
        -- Only a non-matching Control envelope so far: the waiter must not wake.
        _ <- expectRight =<< mb.mbSend controlOutgoing
        threadDelay (200 * 1000)
        notYet <- timeout (100 * 1000) (takeMVar result)
        notYet @?= Nothing
        -- Now send a matching envelope; only it should be delivered.
        _ <- expectRight =<< mb.mbSend userOutgoing
        delivered <- timeout (5 * 1000 * 1000) (takeMVar result)
        case delivered of
            Nothing -> assertFailure "awaitMail never woke up for the matching envelope"
            Just [e] -> isUserMessage e @?= True
            Just other -> assertFailure ("expected exactly one matching envelope, got " <> show (length other))

-------------------------------------------------------------------------------
-- Bounded mailbox / backpressure
-------------------------------------------------------------------------------

mailboxFullRejectsNormalMailTest :: TestTree
mailboxFullRejectsNormalMailTest =
    testCase "a full mailbox refuses further normal mail with MailboxFull" $ do
        mb <- newInMemoryMailbox
        forM_ [1 .. mailboxMaxUnread] $ \_ -> do
            r <- mb.mbSend userOutgoing
            case r of
                Right _ -> pure ()
                Left err -> assertFailure ("unexpected refusal while filling the mailbox: " <> show err)
        overflow <- mb.mbSend userOutgoing
        overflow @?= Left MailboxFull

mailboxFullExemptsControlAndToolCallFinishedTest :: TestTree
mailboxFullExemptsControlAndToolCallFinishedTest =
    testCase "Control and ToolCallFinished mail bypass the backpressure bound" $ do
        mb <- newInMemoryMailbox
        forM_ [1 .. mailboxMaxUnread] $ \_ -> do
            _ <- expectRight =<< mb.mbSend userOutgoing
            pure ()
        let tcid = ToolCallId nil
        let controlOutgoing =
                Outgoing
                    { outId = Nothing
                    , outFrom = FromSystem "test"
                    , outPriority = Normal
                    , outHops = 0
                    , outBody = Control StopRun
                    }
        let finishedOutgoing =
                Outgoing
                    { outId = Nothing
                    , outFrom = FromToolCall tcid
                    , outPriority = Normal
                    , outHops = 0
                    , outBody = ToolCallFinished tcid Completed (TextResponse "done")
                    }
        _ <- expectRight =<< mb.mbSend controlOutgoing
        _ <- expectRight =<< mb.mbSend finishedOutgoing
        unread <- atomically (mb.mbUnread 0)
        length unread @?= mailboxMaxUnread + 2

-------------------------------------------------------------------------------
-- Consumption commits with its effect (mbTrim is the GC half of that)
-------------------------------------------------------------------------------

trimRemovesAtOrBelowCursorTest :: TestTree
trimRemovesAtOrBelowCursorTest =
    testCase "mbTrim garbage-collects envelopes at or below the cursor" $ do
        mb <- newInMemoryMailbox
        receipts <- replicateM 3 (expectRight =<< mb.mbSend userOutgoing)
        let seqs = sort (map rcptSeq receipts)
        mb.mbTrim (seqs !! 1)
        remaining <- atomically (mb.mbUnread 0)
        map envSeq remaining @?= [seqs !! 2]

-------------------------------------------------------------------------------
-- Durable mailbox (Phase 3): fronts a 'MailStore', writes through
-------------------------------------------------------------------------------

testSessionId :: SessionId
testSessionId = SessionId nil

-- | An in-memory 'MailStore' fake, for tests that don't need real SQLite.
newFakeMailStore :: IO MailStore
newFakeMailStore = do
    ref <- newIORef []
    pure
        MailStore
            { msAppend = \_sid envelope -> atomicModifyIORef' ref (\es -> (es ++ [envelope], ()))
            , msLoad = \_sid -> readIORef ref
            }

durableMailboxHydratesFromStoreTest :: TestTree
durableMailboxHydratesFromStoreTest =
    testCase "a durable mailbox is seeded from what the store already has" $ do
        store <- newFakeMailStore
        seedMb <- newDurableMailbox store testSessionId
        _ <- expectRight =<< seedMb.mbSend userOutgoing
        -- A second mailbox over the same store picks up what the first wrote.
        mb <- newDurableMailbox store testSessionId
        unread <- atomically (mb.mbUnread 0)
        length unread @?= 1
        -- And keeps assigning fresh seqs on top of the hydrated ones.
        r2 <- expectRight =<< mb.mbSend userOutgoing
        r2.rcptSeq @?= 2

durableMailboxWritesThroughTest :: TestTree
durableMailboxWritesThroughTest =
    testCase "a durable mailbox persists every accepted envelope to its store" $ do
        store <- newFakeMailStore
        mb <- newDurableMailbox store testSessionId
        _ <- expectRight =<< mb.mbSend userOutgoing
        _ <- expectRight =<< mb.mbSend userOutgoing
        persisted <- store.msLoad testSessionId
        length persisted @?= 2

-- | The real SQLite-backed 'MailStore', round-tripping through a fresh
-- durable mailbox (mirrors how the runner hydrates on session load).
sqliteMailStoreRoundTripsTest :: TestTree
sqliteMailStoreRoundTripsTest =
    testCase "the SQLite mail store round-trips envelopes across mailboxes" $ do
        conn <- open ":memory:"
        store <- mkSqliteMailStore conn
        mb1 <- newDurableMailbox store testSessionId
        _ <- expectRight =<< mb1.mbSend userOutgoing
        _ <- expectRight =<< mb1.mbSend userOutgoing{outBody = UserMessage (UserQuery "second" [])}
        -- Simulate the session being reloaded: a fresh mailbox over the store.
        mb2 <- newDurableMailbox store testSessionId
        unread <- atomically (mb2.mbUnread 0)
        length unread @?= 2
        map envSeq unread @?= [1, 2]

-- | 'WakeOnKind' round-trips through its tag-based JSON (§5 "Scheduling rule").
wakeOnKindJsonRoundTripTest :: TestTree
wakeOnKindJsonRoundTripTest =
    testCase "WakeOnKind round-trips through JSON" $ do
        forM_ [WakeOnUser, WakeOnTool, WakeOnParent, WakeOnChild, WakeOnPeer] $ \kind ->
            decode (encode kind) @?= Just kind
        defaultWakeOn @?= [WakeOnUser, WakeOnTool, WakeOnParent, WakeOnChild]

-- | 'senderWakeKind' classifies each 'Sender' constructor it can tell apart
-- without a lineage lookup.
senderWakeKindClassifiesSendersTest :: TestTree
senderWakeKindClassifiesSendersTest =
    testCase "senderWakeKind classifies user/tool/system senders" $ do
        senderWakeKind (FromUser Nothing) @?= WakeOnUser
        senderWakeKind (FromToolCall (ToolCallId nil)) @?= WakeOnTool
        senderWakeKind (FromSystem "completeCall") @?= WakeOnTool
        senderWakeKind (FromSession testSessionId Nothing) @?= WakeOnPeer
