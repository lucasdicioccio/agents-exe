{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The session mailbox: one mechanism through which everything that
happens *to* a running session reaches it. Phase 1 of
@todos/session-mailbox.md@ built the in-memory 'newInMemoryMailbox'; Phase 3
adds 'newDurableMailbox', which fronts a 'MailStore' (SQLite or Postgres)
with the same in-memory shape so 'mbUnread' stays STM.

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
    MailStore (..),
    newDurableMailbox,
    MailboxInfo (..),
    MailScope (..),
    MailRouter (..),
    newMailRouter,
    SpawnSession,
    RunSubagent,
    WatchRequest (..),
    WatchSession,
    UnwatchSession,
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (when)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Time (getCurrentTime)

import System.Agents.Session.Types (
    Cursor,
    Envelope (..),
    MailBody (..),
    MailScope (..),
    Outgoing (..),
    Receipt (..),
    SendError (..),
    SessionId,
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

{- | Default bound on 'envHops'\/'outHops' (§5): together with
'mailboxMaxUnread' and the @wait@ tool's timeout cap, this is what keeps two
agents replying to each other from ping-ponging or deadlocking forever. A
sender computes its own 'outHops' (a reply is the hops of the mail it
answers, plus one; fresh user mail is 0); 'mbSend' only enforces the cap.
-}
maxMailHops :: Int
maxMailHops = 16

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
            Nothing
                | outgoing.outHops > maxMailHops -> pure $ Left TooManyHops
                | Seq.length es >= mailboxMaxUnread && not (exemptFromBound outgoing.outBody) ->
                    pure $ Left MailboxFull
                | otherwise -> do
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

{- | A durable backend for one session's mail (Phase 3): SQLite or Postgres,
behind a table shaped @(session_id, seq, id UNIQUE, from_json, priority,
hops, body_json, accepted_at)@ per the spec.

'msAppend' persists one already-sequenced envelope (idempotent on 'envId',
mirroring 'sendImpl' — a backend can implement this with an upsert keyed on
@(session_id, id)@ that does nothing on conflict); 'msLoad' returns a
session's envelopes in ascending 'envSeq' order, to hydrate the in-memory
front on session load.
-}
data MailStore = MailStore
    { msAppend :: SessionId -> Envelope -> IO ()
    , msLoad :: SessionId -> IO [Envelope]
    }

{- | A durable mailbox (Phase 3): 'msLoad's a session's mail once to seed an
in-memory 'TVar', then writes through 'msAppend' on every 'mbSend' so
'mbUnread' \/ 'awaitMail' stay plain STM, per §1's "fronted by the same
'TVar'".

Writes are serialised by an 'MVar' rather than folded into the STM
transaction, since persisting is 'IO'. The spec's "second writers are
detected" already assumes one writer per session at a time, so this does
not add a new race: a concurrent 'mbSend' merely waits for the lock instead
of racing a transaction.
-}
newDurableMailbox :: MailStore -> SessionId -> IO Mailbox
newDurableMailbox store sid = do
    persisted <- store.msLoad sid
    envelopesVar <- newTVarIO (Seq.fromList persisted)
    let nextSeq = 1 + Foldable.foldl' (\acc e -> max acc e.envSeq) 0 persisted
    nextSeqVar <- newTVarIO nextSeq
    lock <- newMVar ()
    pure
        Mailbox
            { mbSend = durableSendImpl lock store sid envelopesVar nextSeqVar
            , mbUnread = unreadImpl envelopesVar
            , mbTrim = trimImpl envelopesVar
            }

durableSendImpl :: MVar () -> MailStore -> SessionId -> TVar (Seq Envelope) -> TVar Int -> Outgoing -> IO (Either SendError Receipt)
durableSendImpl lock store sid envelopesVar nextSeqVar outgoing = withMVar lock $ \() -> do
    mid <- maybe newMessageId pure outgoing.outId
    now <- getCurrentTime
    es <- readTVarIO envelopesVar
    case Foldable.find ((== mid) . envId) es of
        Just existing -> pure $ Right $ Receipt mid existing.envSeq True
        Nothing
            | outgoing.outHops > maxMailHops -> pure $ Left TooManyHops
            | Seq.length es >= mailboxMaxUnread && not (exemptFromBound outgoing.outBody) ->
                pure $ Left MailboxFull
            | otherwise -> do
                n <- readTVarIO nextSeqVar
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
                store.msAppend sid envelope
                atomically $ do
                    writeTVar nextSeqVar (n + 1)
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

-------------------------------------------------------------------------------
-- MailRouter (Phase 4, §5)
-------------------------------------------------------------------------------

{- | What 'MailRouter.mrList' \/ 'MailRouter.mrLookup' report about a
registered session, for addressing and discovery: its agent slug (when
known), its parent session (lineage), and a short status string (e.g.
@"running"@, @"idle"@, @"paused"@) that a front-end derives however it
already tracks session status.
-}
data MailboxInfo = MailboxInfo
    { miAgentSlug :: Maybe Text
    , miParent :: Maybe SessionId
    , miStatus :: Text
    , miMailScope :: MailScope
    -- ^ How far this session's own mail may reach (§5 Permissions,
    -- default 'MailScopeSubtree'). Read from the sender's own registration
    -- by a scope check, e.g. 'System.Agents.Tools.SystemToolbox.Mail'\'s
    -- @isWithinScope@, not the recipient's.
    , miInterruptScope :: MailScope
    -- ^ How far this session's 'Interrupt' priority may reach (default
    -- 'MailScopeChildren').
    }
    deriving (Show, Eq)

{- | The process-wide table of live mailboxes (§5): "an agent can write to
another session ... across sessions". Every front-end (the server, the TUI,
@run@) keeps one and registers a session's mailbox on it while that session
is live, so a sibling or ancestor session can address it by 'SessionId'
without knowing anything about where it runs.

This is deliberately the same shape in every front-end (D12): only where it
lives, and how long an entry stays registered, differs.
-}
data MailRouter = MailRouter
    { mrRegister :: SessionId -> MailboxInfo -> Mailbox -> IO (IO ())
    -- ^ Register a session's mailbox; returns the action that unregisters it.
    , mrLookup :: SessionId -> IO (Maybe (MailboxInfo, Mailbox))
    , mrList :: IO [(SessionId, MailboxInfo)]
    }

-- | A fresh, empty, in-memory 'MailRouter'.
newMailRouter :: IO MailRouter
newMailRouter = do
    tableVar <- newTVarIO Map.empty
    pure
        MailRouter
            { mrRegister = \sid info mb -> do
                atomically $ modifyTVar' tableVar (Map.insert sid (info, mb))
                pure $ atomically $ modifyTVar' tableVar (Map.delete sid)
            , mrLookup = \sid -> Map.lookup sid <$> readTVarIO tableVar
            , mrList = fmap (fmap fst) . Map.toList <$> readTVarIO tableVar
            }

{- | A @spawn-session@ hook (@todos/session-mailbox.md@, Phase 4, §5): a
helper's agent slug, an initial message -> the new session's id, or an
error to report to the LLM. Defined here (rather than next to 'Agent' in
"System.Agents.Session.Base") because it is also a field of
'System.Agents.Tools.Context.ToolExecutionContext', and that module cannot
import "System.Agents.Session.Base" without a cycle.
-}
type SpawnSession = Text -> Text -> IO (Either Text SessionId)

{- | A @prompt_agent_\<slug\>@ hook (@todos/os-as-standalone-server.md@ Phase
5, G10): when the calling agent was built by 'System.Agents.Host.Runner',
this hook lets a sub-agent call run as a real, durable, cancellable child
session instead of executing in-tool. Defined here for the same reason as
'SpawnSession': it is also a field of 'ToolExecutionContext', and that
module cannot import "System.Agents.Session.Base" (or the agent tree)
without a cycle.

Given the calling session ('SessionId', so nested sub-calls parent onto the
session actually making the call, not the root), the sub-agent's own slug
and the prompt: either an error to report to the calling LLM, or the new
child's 'SessionId' (already visible through 'SessionCreated'\/'smParent')
paired with an action that waits for the child's run to stop and reports
its final text, exactly as an in-tool call would, and cancels the child
(the runner's own @cancelRun@) if interrupted while waiting.

Only installed for a helper reachable from the calling agent's own
declared tree with no per-call narrowing ('bindings'\/'with'\/'as'):
'System.Agents.AgentTree.OneShotTool' falls back to in-tool execution
otherwise, since reproducing a narrowed node through a fresh session build
is not yet supported.
-}
type RunSubagent = SessionId -> Text -> Text -> IO (Either Text (SessionId, IO (Either Text Text)))

{- | A @watch-session@ request (@todos/session-mailbox.md@, Phase 6, §7): the
target session, an optional filter on 'SessionEvent' kinds (as their
@sessionEventKind@ text, e.g. @"run.stopped"@), an optional glob on the tool
name (for @tool.*@ events only), and an optional TTL in seconds.
-}
data WatchRequest = WatchRequest
    { wrTarget :: SessionId
    , wrEvents :: Maybe [Text]
    , wrTool :: Maybe Text
    , wrTtlSeconds :: Maybe Int
    }

{- | A @watch-session@ hook (Phase 6, §7): given a request, either an error
to report to the LLM, or an id to later pass to 'UnwatchSession'. Defined
here for the same reason as 'SpawnSession': it is also a field of
'System.Agents.Tools.Context.ToolExecutionContext'.
-}
type WatchSession = WatchRequest -> IO (Either Text Text)

-- | An @unwatch-session@ hook (Phase 6, §7): a watch id -> whether a matching, still-active watch was found and stopped.
type UnwatchSession = Text -> IO Bool
