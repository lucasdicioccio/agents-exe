{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The @send-message@ System Toolbox capability (@todos/session-mailbox.md@,
Phase 4, §5): agent-to-agent mail, addressed by 'SessionId'.

Requires 'ctxMailRouter' to resolve the recipient's mailbox; without one,
the call fails the same way any other capability fails when its supporting
context is missing.

__Scope__: enforces the *sender's own* @mailScope@\/@interruptScope@ (§5
"Permissions"), read from 'MailboxInfo.miMailScope'\/'miInterruptScope' on
the sender's own router registration (populated by each front-end from the
sending agent's JSON config; the server does this in
'System.Agents.Host.Runner.serverMailRouter'). Defaults, when a front-end
or agent leaves them unset, are the spec's own: @subtree@ for mail,
@children@ for 'Interrupt'. A disallowed interrupt request is silently
sent as 'Normal' rather than refused, since the sender was otherwise
entitled to send.
-}
module System.Agents.Tools.SystemToolbox.Mail (
    sendMessageToSession,
    spawnSession,
    watchSession,
    unwatchSession,
    mergeLiveSessions,
) where

import Control.Concurrent.STM (atomically)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (find)
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector

import System.Agents.Session.Mailbox (MailRouter (..), Mailbox (..), MailboxInfo (..), WatchRequest (..))
import System.Agents.Session.Types (
    Envelope (..),
    MailBody (..),
    MailScope (..),
    MessageId (..),
    Outgoing (..),
    Priority (..),
    Receipt (..),
    Sender (..),
    SendError (..),
    SessionId (..),
    messageIdText,
 )
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.Tools.Context (ToolExecutionContext (..))
import System.Agents.Tools.SystemToolbox.Session (conversationIdToText)
import System.Agents.Tools.SystemToolbox.Types (
    QueryError (..),
    SendMessageParams (..),
    SendMessageResult (..),
    SpawnSessionParams (..),
    SpawnSessionResult (..),
    UnwatchSessionParams (..),
    UnwatchSessionResult (..),
    WatchSessionParams (..),
    WatchSessionResult (..),
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
                    -- §5 "Permissions": the sender's own mailScope/
                    -- interruptScope (its own MailboxInfo registration),
                    -- defaulting to the spec's subtree/children when unset.
                    mOwnInfo <- (fmap fst) <$> router.mrLookup ownSid
                    let ownMailScope = maybe MailScopeSubtree miMailScope mOwnInfo
                        ownInterruptScope = maybe MailScopeChildren miInterruptScope mOwnInfo
                    withinScope <- isWithinScope router ownMailScope ownSid toSid
                    if not withinScope
                        then pure $ Left $ sendErrorToQueryError NotPermitted
                        else do
                            ownSlug <- resolveOwnSlug router ownSid
                            let mInReplyTo = parseMessageId =<< smpInReplyTo params
                            hops <- replyHops router ownSid mInReplyTo
                            canInterrupt <-
                                if smpInterrupt params
                                    then isWithinScope router ownInterruptScope ownSid toSid
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

{- | Watch another session's events, forwarded as 'WatchedEvent' mail to
this session's mailbox (§7). Delegates to the front-end's 'ctxWatchSession'
hook, the same way 'sendMessageToSession' delegates to 'ctxMailRouter'.
-}
watchSession :: ToolExecutionContext -> WatchSessionParams -> IO (Either QueryError WatchSessionResult)
watchSession ctx params = case ctxWatchSession ctx of
    Nothing -> pure $ Left $ SystemInfoError "watch-session is not available in this context"
    Just watch -> case parseSessionId (wspSession params) of
        Nothing -> pure $ Left $ SystemInfoError ("not a session id: " <> wspSession params)
        Just target -> do
            let request =
                    WatchRequest
                        { wrTarget = target
                        , wrEvents = wspEvents params
                        , wrTool = wspTool params
                        , wrTtlSeconds = wspTtlSeconds params
                        }
            result <- watch request
            pure $ either (Left . SystemInfoError) (Right . WatchSessionResult) result

-- | Stop a previously registered watch (§7). Delegates to 'ctxUnwatchSession'.
unwatchSession :: ToolExecutionContext -> UnwatchSessionParams -> IO (Either QueryError UnwatchSessionResult)
unwatchSession ctx params = case ctxUnwatchSession ctx of
    Nothing -> pure $ Left $ SystemInfoError "unwatch-session is not available in this context"
    Just unwatch -> Right . UnwatchSessionResult <$> unwatch (uspWatchId params)

{- | Merge a 'MailRouter's live sessions into a @list-sessions@ result (§5:
"@list-sessions@ merges it with the persisted ones"), so a session that
exists (e.g. just 'spawn-session'ed) but hasn't been checkpointed to the
persisted catalog yet is still visible.

Matches by session id (rendered the same way @list-sessions@ already does,
as its underlying 'ConversationId''s text — see
'System.Agents.SessionStore.sessionIdToConversationId'): an id already in
the persisted list has its @status@ overwritten with the live one (more
current); an id the persisted list doesn't have yet is appended, with the
fields @list-sessions@ can't know about a session it never loaded left at
sensible defaults. A 'Nothing' router, or a result shaped unlike
@list-sessions@'s own output, is passed through unchanged.
-}
mergeLiveSessions :: Maybe MailRouter -> Aeson.Value -> IO Aeson.Value
mergeLiveSessions Nothing value = pure value
mergeLiveSessions (Just router) (Aeson.Object obj)
    | Just (Aeson.Array sessions) <- KeyMap.lookup "sessions" obj = do
        live <- router.mrList
        let existingIds = Set.fromList [sid | Aeson.Object s <- Vector.toList sessions, Just (Aeson.String sid) <- [KeyMap.lookup "sessionId" s]]
            overlaid = fmap (overlayStatus live) sessions
            newEntries = [liveSessionEntry sid info | (sid, info) <- live, not (idText sid `Set.member` existingIds)]
            merged = overlaid <> Vector.fromList newEntries
            bumpedTotal = case KeyMap.lookup "totalAccessible" obj of
                Just (Aeson.Number count) -> Aeson.Number (count + fromIntegral (length newEntries))
                _ -> Aeson.toJSON (Vector.length merged)
         in pure $
                Aeson.Object $
                    KeyMap.insert "sessions" (Aeson.Array merged) $
                        KeyMap.insert "totalAccessible" bumpedTotal obj
mergeLiveSessions _ value = pure value

-- | This session's id, rendered the way @list-sessions@ renders one.
idText :: SessionId -> Text
idText = conversationIdToText . SessionStore.sessionIdToConversationId

-- | A brand-new @list-sessions@ entry for a live session the persisted
-- catalog doesn't know about yet.
liveSessionEntry :: SessionId -> MailboxInfo -> Aeson.Value
liveSessionEntry sid info =
    Aeson.object
        [ "sessionId" Aeson..= idText sid
        , "conversationId" Aeson..= idText sid
        , "modificationTime" Aeson..= ("" :: Text)
        , "turnCount" Aeson..= (0 :: Int)
        , "isParent" Aeson..= False
        , "isChild" Aeson..= (info.miParent /= Nothing)
        , "isLocked" Aeson..= False
        , "status" Aeson..= info.miStatus
        ]

-- | Overwrite a persisted entry's @status@ with the live one, if it has one.
overlayStatus :: [(SessionId, MailboxInfo)] -> Aeson.Value -> Aeson.Value
overlayStatus live (Aeson.Object s)
    | Just (Aeson.String sid) <- KeyMap.lookup "sessionId" s
    , Just (_, info) <- List.find ((== sid) . idText . fst) live =
        Aeson.Object (KeyMap.insert "status" (Aeson.String info.miStatus) s)
overlayStatus _ other = other

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

{- | Whether 'targetSid' is reachable from 'ownSid' under the given
'MailScope' (§5 "Permissions"): 'MailScopeOwn' only the sender itself;
'MailScopeChildren' the sender or a direct child; 'MailScopeSubtree' the
sender or any descendant (walking 'MailboxInfo.miParent' up from the
target, bounded depth so a corrupt or cyclic parent chain cannot hang);
'MailScopeAll' always.
-}
isWithinScope :: MailRouter -> MailScope -> SessionId -> SessionId -> IO Bool
isWithinScope _router MailScopeAll _ownSid _targetSid = pure True
isWithinScope _router _scope ownSid targetSid
    | ownSid == targetSid = pure True
isWithinScope _router MailScopeOwn _ownSid _targetSid = pure False
isWithinScope router MailScopeChildren ownSid targetSid = isDirectChild router ownSid targetSid
isWithinScope router MailScopeSubtree ownSid targetSid = go targetSid (32 :: Int)
  where
    go _ 0 = pure False
    go sid depthLeft = do
        mInfo <- router.mrLookup sid
        case mInfo of
            Nothing -> pure False
            Just (info, _) -> case info.miParent of
                Nothing -> pure False
                Just parentSid
                    | parentSid == ownSid -> pure True
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
