{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The @send-message@ System Toolbox capability (@todos/session-mailbox.md@,
Phase 4, §5): agent-to-agent mail, addressed by 'SessionId'.

Requires 'ctxMailRouter' to resolve the recipient's mailbox; without one,
the call fails the same way any other capability fails when its supporting
context is missing.

__Scope__: enforces §5's own defaults -- @mailScope: subtree@ (a session
may mail itself or any descendant, walking 'MailboxInfo.miParent' up from
the recipient) and @interruptScope: children@ (only a direct child may be
sent 'Interrupt' priority; a disallowed interrupt request is silently sent
as 'Normal' rather than refused, since the sender was otherwise entitled to
send). These are not yet a per-agent @mailScope@\/@interruptScope@ config
knob (§5 lists them as knobs "we expect to revisit"); wiring one through
requires threading two more fields across 'Agent', 'ToolExecutionContext',
and every context constructor the way 'ctxMailRouter' was, which is
deferred to a follow-up.
-}
module System.Agents.Tools.SystemToolbox.Mail (
    sendMessageToSession,
    spawnSession,
) where

import Control.Concurrent.STM (atomically)
import Data.List (find)
import Data.Text (Text)
import qualified Data.UUID as UUID

import System.Agents.Session.Mailbox (MailRouter (..), Mailbox (..), MailboxInfo (..))
import System.Agents.Session.Types (
    Envelope (..),
    MailBody (..),
    MessageId (..),
    Outgoing (..),
    Priority (..),
    Receipt (..),
    Sender (..),
    SendError (..),
    SessionId (..),
    messageIdText,
 )
import System.Agents.Tools.Context (ToolExecutionContext (..))
import System.Agents.Tools.SystemToolbox.Types (
    QueryError (..),
    SendMessageParams (..),
    SendMessageResult (..),
    SpawnSessionParams (..),
    SpawnSessionResult (..),
 )

{- | Send agent-to-agent mail to another session by id.

Looks the recipient up via 'ctxMailRouter'; posts an 'AgentMessage'
envelope from 'FromSession' (this session, and its agent slug when the
router knows it via its own registration); reports back the recipient's
status alongside the receipt.
-}
sendMessageToSession :: ToolExecutionContext -> SendMessageParams -> IO (Either QueryError SendMessageResult)
sendMessageToSession ctx params = case ctxMailRouter ctx of
    Nothing -> pure $ Left $ SystemInfoError "agent-to-agent mail is not available in this context"
    Just router -> case parseSessionId (smpTo params) of
        Nothing -> pure $ Left $ SystemInfoError ("not a session id: " <> smpTo params)
        Just toSid -> do
            mTarget <- router.mrLookup toSid
            case mTarget of
                Nothing -> pure $ Left $ SystemInfoError ("unknown session: " <> smpTo params)
                Just (targetInfo, targetMailbox) -> do
                    let ownSid = ctxSessionId ctx
                    -- §5 "Permissions": mailScope (default subtree) and
                    -- interruptScope (default children). Not yet a
                    -- per-agent config knob (see module haddock) -- these
                    -- are the spec's own defaults, applied unconditionally.
                    withinSubtree <- isWithinSubtree router ownSid toSid
                    if not withinSubtree
                        then pure $ Left $ sendErrorToQueryError NotPermitted
                        else do
                            ownSlug <- resolveOwnSlug router ownSid
                            let mInReplyTo = parseMessageId =<< smpInReplyTo params
                            hops <- replyHops router ownSid mInReplyTo
                            canInterrupt <-
                                if smpInterrupt params
                                    then isDirectChild router ownSid toSid
                                    else pure False
                            let priority = if canInterrupt then Interrupt else Normal
                                outgoing =
                                    Outgoing
                                        { outId = Nothing
                                        , outFrom = FromSession ownSid ownSlug
                                        , outPriority = priority
                                        , outHops = hops
                                        , outBody = AgentMessage (smpText params) mInReplyTo (smpExpectsReply params)
                                        }
                            sent <- targetMailbox.mbSend outgoing
                            pure $ either (Left . sendErrorToQueryError) (Right . mkResult targetInfo.miStatus) sent
  where
    mkResult :: Text -> Receipt -> SendMessageResult
    mkResult status receipt =
        SendMessageResult
            { smrMessageId = messageIdText receipt.rcptId
            , smrSeq = receipt.rcptSeq
            , smrDuplicate = receipt.rcptDuplicate
            , smrRecipientStatus = status
            }

{- | Start one of this session's helper agents running as a detached child
session (§5): not call\/return -- it outlives this tool call and answers by
mail. Delegates to the front-end's 'ctxSpawnSession' hook (server:
@Host.Runner.serverSpawnSession@, durable, recorded with this session as
parent; TUI \/ @run@: not yet wired, see their own modules), the same way
'sendMessageToSession' delegates to 'ctxMailRouter'.
-}
spawnSession :: ToolExecutionContext -> SpawnSessionParams -> IO (Either QueryError SpawnSessionResult)
spawnSession ctx params = case ctxSpawnSession ctx of
    Nothing -> pure $ Left $ SystemInfoError "spawn-session is not available in this context"
    Just spawn -> do
        result <- spawn (sspAgent params) (sspMessage params)
        pure $ either (Left . SystemInfoError) (Right . mkResult) result
  where
    mkResult :: SessionId -> SpawnSessionResult
    mkResult (SessionId uuid) = SpawnSessionResult{ssrSessionId = UUID.toText uuid}

-- | Best-effort: look up the sender's own registration to report its slug.
resolveOwnSlug :: MailRouter -> SessionId -> IO (Maybe Text)
resolveOwnSlug router sid = (>>= \(info, _) -> info.miAgentSlug) <$> router.mrLookup sid

{- | The hop count for an outgoing reply: 'Nothing' (fresh mail, not a
reply) is 0; a reply is the hops of the mail it answers, plus one (§5).
Looked up in the sender's own mailbox, by 'MessageId', since that is where
the mail being answered was received. A message this session never
actually received (a bogus or foreign @in_reply_to@) is treated as fresh
mail rather than failing the send.
-}
replyHops :: MailRouter -> SessionId -> Maybe MessageId -> IO Int
replyHops _router _ownSid Nothing = pure 0
replyHops router ownSid (Just replyToId) = do
    mOwn <- router.mrLookup ownSid
    case mOwn of
        Nothing -> pure 0
        Just (_, ownMailbox) -> do
            everReceived <- atomically (ownMailbox.mbUnread 0)
            pure $ maybe 0 ((+ 1) . envHops) (find ((== replyToId) . envId) everReceived)

{- | Whether 'targetSid' is 'ancestorSid' itself or one of its descendants,
walking 'MailboxInfo.miParent' up from the target. Bounded depth so a
corrupt or cyclic parent chain cannot hang.
-}
isWithinSubtree :: MailRouter -> SessionId -> SessionId -> IO Bool
isWithinSubtree router ancestorSid targetSid
    | ancestorSid == targetSid = pure True
    | otherwise = go targetSid (32 :: Int)
  where
    go _ 0 = pure False
    go sid depthLeft = do
        mInfo <- router.mrLookup sid
        case mInfo of
            Nothing -> pure False
            Just (info, _) -> case info.miParent of
                Nothing -> pure False
                Just parentSid
                    | parentSid == ancestorSid -> pure True
                    | otherwise -> go parentSid (depthLeft - 1)

-- | Whether 'targetSid's recorded parent is exactly 'parentSid'.
isDirectChild :: MailRouter -> SessionId -> SessionId -> IO Bool
isDirectChild router parentSid targetSid = do
    mTarget <- router.mrLookup targetSid
    pure $ maybe False ((== Just parentSid) . miParent . fst) mTarget

sendErrorToQueryError :: SendError -> QueryError
sendErrorToQueryError err = case err of
    UnknownRecipient -> SystemInfoError "unknown recipient"
    MailboxFull -> SystemInfoError "recipient's mailbox is full"
    NotPermitted -> SystemInfoError "not permitted to send to this session"
    TooManyHops -> SystemInfoError "too many agent-to-agent hops"

parseSessionId :: Text -> Maybe SessionId
parseSessionId = fmap SessionId . UUID.fromText

parseMessageId :: Text -> Maybe MessageId
parseMessageId = fmap MessageId . UUID.fromText
