{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Cross-conversation reference types for building linked knowledge graphs.

This module defines types for:
- Referencing other conversations and messages
- Tracking reference relationships (bidirectional links)
- Supporting citation syntax like [[conv:id#msg-id]]

The reference system enables:
- **Context Preservation**: Reference previous discussions
- **Knowledge Graph**: Build connections between related topics
- **Continuity**: Pick up threads from earlier conversations
- **Search**: Find related conversations via references

== Reference Syntax

References use wiki-style syntax:

* @[[conv:uuid]]@ - Reference an entire conversation
* @[[conv:uuid#msg-id]]@ - Reference a specific message
* @[[conv:uuid|display text]]@ - Reference with custom display text

== Design Notes

1. **Reference Types**: Differentiate between citations, replies, and related links
2. **Bidirectional Links**: Both source and target track the reference
3. **Immutable References**: Once created, references are immutable for audit trails
4. **Soft References**: References don't enforce foreign key constraints to allow
   referencing deleted/archived conversations
-}
module System.Agents.OS.References.Types (
    -- * Reference Types
    ConversationRef (..),
    RefType (..),
    RefDirection (..),

    -- * Reference Components
    ReferenceConfig (..),
    ReferenceState (..),

    -- * Reference Views
    ReferenceView (..),
    ReferencedContent (..),

    -- * Message Reference
    MessageRef (..),

    -- * Parsing
    parseReference,
    renderReference,
    referencePattern,

    -- * Queries
    ReferenceQuery (..),
    defaultReferenceQuery,

    -- * Constants
    referenceConfigComponentId,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import System.Agents.OS.Core.Types (
    Component (..),
    ComponentTypeId (..),
    ConversationId (..),
    EntityId (..),
 )

-------------------------------------------------------------------------------
-- Message Reference
-------------------------------------------------------------------------------

{- | A reference to a specific message within a conversation.

Messages are identified by a unique identifier (could be timestamp-based
or UUID depending on implementation).
-}
newtype MessageRef = MessageRef { unMessageRef :: Text }
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-------------------------------------------------------------------------------
-- Reference Types
-------------------------------------------------------------------------------

{- | Type of reference relationship.

Different reference types carry different semantic meanings:

* 'RefCite' - Academic-style citation, acknowledging source
* 'RefReply' - Direct response to a previous conversation/message
* 'RefRelated' - Related content, not a direct reply
* 'RefFork' - This conversation was forked from the referenced one
-}
data RefType
    = -- | Citation/reference to source material
      RefCite
    | -- | Direct reply/response
      RefReply
    | -- | Related conversation (topic connection)
      RefRelated
    | -- | This conversation was forked from the target
      RefFork
    deriving (Show, Eq, Ord, Generic)

instance FromJSON RefType
instance ToJSON RefType

-- | Direction of reference from the perspective of the source.
data RefDirection
    = -- | Outgoing reference (this conversation references another)
      OutgoingRef
    | -- | Incoming reference (another conversation references this one)
      IncomingRef
    deriving (Show, Eq, Generic)

instance FromJSON RefDirection
instance ToJSON RefDirection

{- | A reference to another conversation and optionally a specific message.

References are stored as ECS components attached to the source conversation.
The target conversation may not exist (soft reference) to allow referencing
archived or deleted conversations.

Reference equality considers:
- Source and target conversation IDs
- Target message ID (if specified)
- Reference type

It does NOT consider:
- Reference timestamp (two refs created at different times to same target are equal)
- Context text
- Created by
-}
data ConversationRef = ConversationRef
    { refSourceConversation :: ConversationId
    -- ^ The conversation making the reference
    , refTargetConversation :: ConversationId
    -- ^ The conversation being referenced
    , refTargetMessage :: Maybe MessageRef
    -- ^ Optional specific message being referenced
    , refType :: RefType
    -- ^ Type of reference relationship
    , refCreatedAt :: UTCTime
    -- ^ When the reference was created
    , refCreatedBy :: Maybe Text
    -- ^ User/agent who created the reference
    , refContext :: Maybe Text
    -- ^ Optional context/surrounding text from source
    }
    deriving (Show, Eq, Generic)

instance FromJSON ConversationRef
instance ToJSON ConversationRef

-------------------------------------------------------------------------------
-- Reference Components
-------------------------------------------------------------------------------

{- | Configuration component for a reference.

References are stored as separate entities in the ECS world with:
- ReferenceConfig: Static reference information
- ReferenceState: Runtime state (resolved, unresolved, broken)
-}
data ReferenceConfig = ReferenceConfig
    { rcSourceId :: ConversationId
    -- ^ Source conversation ID
    , rcTargetId :: ConversationId
    -- ^ Target conversation ID
    , rcTargetMessageRef :: Maybe MessageRef
    -- ^ Optional target message reference
    , rcRefType :: RefType
    -- ^ Type of reference
    , rcContext :: Maybe Text
    -- ^ Context from source message
    }
    deriving (Show, Eq, Generic)

instance FromJSON ReferenceConfig
instance ToJSON ReferenceConfig

instance Component ReferenceConfig where
    componentId _ = ComponentTypeId 40

{- | Runtime state for a reference.

Tracks whether the reference target has been resolved and validated.
-}
data ReferenceState = ReferenceState
    { rsCreatedAt :: UTCTime
    -- ^ When the reference was created
    , rsCreatedBy :: Maybe Text
    -- ^ Who created the reference
    , rsResolvedAt :: Maybe UTCTime
    -- ^ When the target was verified (if resolved)
    , rsIsValid :: Bool
    -- ^ Whether the target exists/is accessible
    , rsResolutionError :: Maybe Text
    -- ^ Error message if resolution failed
    }
    deriving (Show, Eq, Generic)

instance FromJSON ReferenceState
instance ToJSON ReferenceState

instance Component ReferenceState where
    componentId _ = ComponentTypeId 41

-------------------------------------------------------------------------------
-- Reference Views
-------------------------------------------------------------------------------

{- | A rendered view of a reference for display.

This is a read-only structure combining reference data with
resolved content from the target conversation.
-}
data ReferenceView = ReferenceView
    { rvRef :: ConversationRef
    -- ^ The reference data
    , rvDirection :: RefDirection
    -- ^ Incoming or outgoing
    , rvDisplayTitle :: Text
    -- ^ Title to display for the reference
    , rvPreviewText :: Maybe Text
    -- ^ Preview of the referenced content
    , rvTargetTimestamp :: Maybe UTCTime
    -- ^ Timestamp of the target message/conversation
    , rvIsClickable :: Bool
    -- ^ Whether the target is accessible
    }
    deriving (Show, Eq, Generic)

instance FromJSON ReferenceView
instance ToJSON ReferenceView

{- | Content retrieved from a referenced conversation/message.

Used for previewing referenced content inline.
-}
data ReferencedContent = ReferencedContent
    { rcConversationId :: ConversationId
    -- ^ Conversation the content came from
    , rcMessageRef :: Maybe MessageRef
    -- ^ Specific message (if referenced)
    , rcTitle :: Maybe Text
    -- ^ Conversation title
    , rcContent :: Text
    -- ^ Message content or conversation summary
    , rcAuthor :: Maybe Text
    -- ^ Message author (if applicable)
    , rcTimestamp :: UTCTime
    -- ^ When the content was created
    }
    deriving (Show, Eq, Generic)

instance FromJSON ReferencedContent
instance ToJSON ReferencedContent

-------------------------------------------------------------------------------
-- Reference Parsing
-------------------------------------------------------------------------------

{- | Pattern for matching reference syntax in text.

Matches:
- [[conv:uuid]] - conversation reference
- [[conv:uuid#msg-id]] - message reference
- [[conv:uuid|display text]] - with display text
- [[conv:uuid#msg-id|display text]] - full form

Capture groups:
1. Conversation UUID
2. Optional message ID (with #)
3. Optional display text (with |)
-}
referencePattern :: Text
referencePattern = "\\[\\[conv:([a-f0-9-]+)(?:#([a-zA-Z0-9_-]+))?(?:\\|([^\\]]+))?\\]\\]"

{- | Parse a reference from text.

Returns 'Nothing' if the text doesn't match the reference pattern.

This is a simplified implementation that looks for [[conv:...]] patterns
and extracts the components.
-}
parseReference :: Text -> Maybe (ConversationId, Maybe MessageRef, Maybe Text)
parseReference txt = do
    -- Find the start of a reference
    let startIdx = Text.findIndex (== '[') txt
    case startIdx of
        Nothing -> Nothing
        Just idx -> do
            let remainder = Text.drop idx txt
            -- Check for [[conv: prefix
            if not ("[[conv:" `Text.isPrefixOf` remainder)
                then Nothing
                else do
                    -- Extract the content between [[conv: and ]]
                    let content = Text.drop 7 remainder -- drop "[[conv:"
                    endIdx <- Text.findIndex (== ']') content
                    let refBody = Text.take endIdx content
                    parseRefBody refBody
  where
    parseRefBody :: Text -> Maybe (ConversationId, Maybe MessageRef, Maybe Text)
    parseRefBody body = do
        -- Split by | for display text first
        let (refPart, displayPart) = case Text.break (== '|') body of
                (r, "") -> (r, Nothing)
                (r, d) -> (r, Just $ Text.drop 1 d)

        -- Split refPart by # for message ID
        let (convPart, msgPart) = case Text.break (== '#') refPart of
                (c, "") -> (c, Nothing)
                (c, m) -> (c, Just $ MessageRef $ Text.drop 1 m)

        -- Parse conversation ID
        let convId = ConversationId $ readTextEntityId convPart

        -- Only return if we have a valid conversation ID
        if Text.null convPart
            then Nothing
            else Just (convId, msgPart, displayPart)

    -- Helper to convert text to EntityId
    -- In practice, this would properly parse the UUID
    readTextEntityId :: Text -> EntityId
    readTextEntityId _ = EntityId nil
      where
        nil = read "00000000-0000-0000-0000-000000000000"

{- | Render a reference to its text representation.

Inverse of 'parseReference'.
-}
renderReference :: ConversationId -> Maybe MessageRef -> Maybe Text -> Text
renderReference convId mMsgRef mDisplayText =
    "[[conv:" <> convIdText <> msgPart <> displayPart <> "]]"
  where
    convIdText = case convId of
        ConversationId eid -> Text.pack $ show eid

    msgPart = case mMsgRef of
        Just (MessageRef msgId) -> "#" <> msgId
        Nothing -> ""

    displayPart = case mDisplayText of
        Just display -> "|" <> display
        Nothing -> ""

-------------------------------------------------------------------------------
-- Reference Queries
-------------------------------------------------------------------------------

{- | Query specification for finding references.
-}
data ReferenceQuery = ReferenceQuery
    { rqSourceConversation :: Maybe ConversationId
    -- ^ Filter by source conversation
    , rqTargetConversation :: Maybe ConversationId
    -- ^ Filter by target conversation
    , rqRefType :: Maybe RefType
    -- ^ Filter by reference type
    , rqDirection :: Maybe RefDirection
    -- ^ Filter by direction
    , rqLimit :: Maybe Int
    -- ^ Maximum results
    , rqIncludeInvalid :: Bool
    -- ^ Include references that couldn't be resolved
    }
    deriving (Show, Eq, Generic)

-- | Default reference query (no filters, excludes invalid refs).
defaultReferenceQuery :: ReferenceQuery
defaultReferenceQuery =
    ReferenceQuery
        { rqSourceConversation = Nothing
        , rqTargetConversation = Nothing
        , rqRefType = Nothing
        , rqDirection = Nothing
        , rqLimit = Nothing
        , rqIncludeInvalid = False
        }

-------------------------------------------------------------------------------
-- Component ID Constants
-------------------------------------------------------------------------------

-- | ComponentTypeId for ReferenceConfig (40)
referenceConfigComponentId :: ComponentTypeId
referenceConfigComponentId = ComponentTypeId 40

