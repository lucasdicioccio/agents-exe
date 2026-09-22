{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The session mailbox: one mechanism through which everything that
happens *to* a running session reaches it (Phase 1 of
@todos/session-mailbox.md@, in-memory only — the durable, SQLite-backed
mailbox is Phase 3).

Transactional means three things, and only these (see the spec, §1):

1. Accept is atomic and idempotent: one STM transaction assigns 'envSeq';
   resending the same 'envId' returns the original 'Receipt' with
   'rcptDuplicate' set.
2. Reading does not consume: there is no @pop@; a reader is a cursor.
3. Consumption commits with its effect: the caller advances a session's
   'mailCursor' itself, once it has folded the envelopes into a turn.
-}
module System.Agents.Session.Mailbox (
    Mailbox (..),
    newInMemoryMailbox,
    awaitMail,
    mailboxMaxUnread,
) where

import Control.Monad (when)
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import qualified Data.Foldable as Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Time (getCurrentTime)

import System.Agents.Session.Types (
    Cursor,
    Envelope (..),
    MailBody (..),
    Outgoing (..),
    Receipt (..),
    SendError (..),
    newMessageId,
 )

{- | A session's mailbox: any thread, in any front-end, can 'mbSend' to it;
the stepper reads it at the receive points (R1-R4) via 'mbUnread' and
'awaitMail'.
-}
data Mailbox = Mailbox
    { mbSend :: Outgoing -> IO (Either SendError Receipt)
    -- ^ Accept an envelope. Any thread, any session. Atomic and idempotent
    -- on 'outId'.
    , mbUnread :: Cursor -> STM [Envelope]
    -- ^ Everything with 'envSeq' greater than the given cursor. Never
    -- removes anything; a reader is a cursor, not a consumer.
    , mbTrim :: Cursor -> IO ()
    -- ^ Garbage-collect envelopes at or below the given cursor.
    }

-- | Default bound on unread envelopes a mailbox holds before refusing new
-- mail with 'MailboxFull'. 'Control' and 'ToolCallFinished' are exempt (the
-- backpressure must never itself block cancellation or a completed call).
mailboxMaxUnread :: Int
mailboxMaxUnread = 256

-- | Whether an envelope's body is exempt from the 'mailboxMaxUnread' bound.
exemptFromBound :: MailBody -> Bool
exemptFromBound body = case body of
    Control _ -> True
    ToolCallFinished{} -> True
    _ -> False

{- | An in-memory mailbox backed by a single 'TVar'. Used by @run@, the TUI,
sub-agent sessions, and tests. The durable, SQLite-backed mailbox (Phase 3)
fronts itself with the same shape so 'mbUnread' stays STM.
-}
newInMemoryMailbox :: IO Mailbox
newInMemoryMailbox = do
    envelopesVar <- newTVarIO Seq.empty
    nextSeqVar <- newTVarIO 1
    pure
        Mailbox
            { mbSend = sendImpl envelopesVar nextSeqVar
            , mbUnread = unreadImpl envelopesVar
            , mbTrim = trimImpl envelopesVar
            }

sendImpl :: TVar (Seq Envelope) -> TVar Int -> Outgoing -> IO (Either SendError Receipt)
sendImpl envelopesVar nextSeqVar outgoing = do
    mid <- maybe newMessageId pure outgoing.outId
    now <- getCurrentTime
    atomically $ do
        es <- readTVar envelopesVar
        case Foldable.find ((== mid) . envId) es of
            Just existing -> pure $ Right $ Receipt mid existing.envSeq True
            Nothing ->
                if Seq.length es >= mailboxMaxUnread && not (exemptFromBound outgoing.outBody)
                    then pure $ Left MailboxFull
                    else do
                        n <- readTVar nextSeqVar
                        writeTVar nextSeqVar (n + 1)
                        let envelope =
                                Envelope
                                    { envId = mid
                                    , envSeq = n
                                    , envFrom = outgoing.outFrom
                                    , envPriority = outgoing.outPriority
                                    , envHops = outgoing.outHops
                                    , envSentAt = now
                                    , envBody = outgoing.outBody
                                    }
                        writeTVar envelopesVar (es Seq.|> envelope)
                        pure $ Right $ Receipt mid n False

unreadImpl :: TVar (Seq Envelope) -> Cursor -> STM [Envelope]
unreadImpl envelopesVar cur = do
    es <- readTVar envelopesVar
    pure $ Foldable.toList $ Seq.filter (\e -> e.envSeq > cur) es

trimImpl :: TVar (Seq Envelope) -> Cursor -> IO ()
trimImpl envelopesVar cur =
    atomically $ do
        es <- readTVar envelopesVar
        writeTVar envelopesVar (Seq.filter (\e -> e.envSeq > cur) es)

{- | Block (via STM 'retry') until at least one unread envelope matching the
predicate exists, then return all matches. Composes with 'orElse' for a
receive with alternatives (e.g. R3a: interrupts vs. attached calls) and with
plain 'STM' actions for a blocking wait with no IO race to cancel.
-}
awaitMail :: Mailbox -> Cursor -> (Envelope -> Bool) -> STM [Envelope]
awaitMail mb cur p = do
    es <- filter p <$> mb.mbUnread cur
    when (null es) retry
    pure es
