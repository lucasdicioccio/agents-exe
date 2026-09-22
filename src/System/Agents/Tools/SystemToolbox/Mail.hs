{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The @send-message@ System Toolbox capability (@todos/session-mailbox.md@,
Phase 4, §5): agent-to-agent mail, addressed by 'SessionId'.

Requires 'ctxMailRouter' to resolve the recipient's mailbox; without one,
the call fails the same way any other capability fails when its supporting
context is missing.
-}
module System.Agents.Tools.SystemToolbox.Mail (
    sendMessageToSession,
) where

import Data.Text (Text)
import qualified Data.UUID as UUID

import System.Agents.Session.Mailbox (MailRouter (..), Mailbox (..), MailboxInfo (..))
import System.Agents.Session.Types (
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
import System.Agents.Tools.SystemToolbox.Types (QueryError (..), SendMessageParams (..), SendMessageResult (..))

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
                    ownSlug <- resolveOwnSlug router (ctxSessionId ctx)
                    let priority = if smpInterrupt params then Interrupt else Normal
                        outgoing =
                            Outgoing
                                { outId = Nothing
                                , outFrom = FromSession (ctxSessionId ctx) ownSlug
                                , outPriority = priority
                                , outHops = 0
                                , outBody = AgentMessage (smpText params) (parseMessageId =<< smpInReplyTo params) (smpExpectsReply params)
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

-- | Best-effort: look up the sender's own registration to report its slug.
resolveOwnSlug :: MailRouter -> SessionId -> IO (Maybe Text)
resolveOwnSlug router sid = (>>= \(info, _) -> info.miAgentSlug) <$> router.mrLookup sid

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
