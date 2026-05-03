{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Core types and operations for TUI draft messages.

This module provides functionality for composing, saving, and managing
message drafts in the TUI without sending them immediately.
-}
module System.Agents.TUI.Drafts (
    -- * Types
    DraftId (..),
    newDraftId,
    DraftAttachment (..),
    DraftMessage (..),
    DraftMode (..),

    -- * Lenses
    draftId,
    draftConversationId,
    draftContent,
    draftAttachments,
    draftCreatedAt,
    draftUpdatedAt,
    draftTitle,

    -- * Operations
    newDraft,
    newDraftForConversation,
    draftAutoTitle,
    updateDraftContent,
    updateDraftTitle,
    addDraftAttachment,
    removeDraftAttachment,

    -- * Auto-save
    AutoSaveState (..),
    initAutoSaveState,
    AutoSaveConfig (..),
    defaultAutoSaveConfig,
    scheduleAutoSave,
    cancelAutoSave,
    getPendingDraft,
    markDraftSaved,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)

import System.Agents.Base (ConversationId (..))

-------------------------------------------------------------------------------
-- DraftId
-------------------------------------------------------------------------------

-- | Unique identifier for a draft message.
newtype DraftId = DraftId UUID
    deriving (Show, Eq, Ord, Generic)

instance FromJSON DraftId
instance ToJSON DraftId

-- | Generate a new unique draft ID.
newDraftId :: IO DraftId
newDraftId = DraftId <$> UUID.nextRandom

-------------------------------------------------------------------------------
-- DraftAttachment
-------------------------------------------------------------------------------

-- | Simplified attachment reference for drafts.
data DraftAttachment = DraftAttachment
    { draftAttPath :: FilePath
    -- ^ Path to the attached file
    , draftAttMimeType :: Text
    -- ^ MIME type of the attachment
    , draftAttFilename :: Text
    -- ^ Original filename
    }
    deriving (Show, Eq, Generic)

instance FromJSON DraftAttachment
instance ToJSON DraftAttachment

-------------------------------------------------------------------------------
-- Draft Types
-------------------------------------------------------------------------------

-- | A draft message that can be persisted and resumed later.
data DraftMessage = DraftMessage
    { _draftId :: DraftId
    -- ^ Unique identifier for this draft
    , _draftConversationId :: Maybe ConversationId
    -- ^ Optional: associated conversation (Nothing = new conversation)
    , _draftContent :: Text
    -- ^ The draft message content
    , _draftAttachments :: [DraftAttachment]
    -- ^ Attachments associated with this draft
    , _draftCreatedAt :: UTCTime
    -- ^ When the draft was created
    , _draftUpdatedAt :: UTCTime
    -- ^ When the draft was last modified
    , _draftTitle :: Maybe Text
    -- ^ Optional user-provided title (auto-generated if Nothing)
    }
    deriving (Show, Eq, Generic)

instance FromJSON DraftMessage
instance ToJSON DraftMessage

makeLenses ''DraftMessage

-- | UI mode for draft management.
data DraftMode
    = -- | No draft mode active
      DraftModeNone
    | -- | Currently composing/editing a specific draft
      DraftModeComposing DraftId
    | -- | Browsing/selecting from existing drafts
      DraftModeSelecting
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Draft Operations
-------------------------------------------------------------------------------

-- | Create a new empty draft.
newDraft :: IO DraftMessage
newDraft = do
    now <- getCurrentTime
    DraftMessage
        <$> newDraftId
        <*> pure Nothing
        <*> pure ""
        <*> pure []
        <*> pure now
        <*> pure now
        <*> pure Nothing

-- | Create a new draft for a specific conversation.
newDraftForConversation :: ConversationId -> IO DraftMessage
newDraftForConversation convId = do
    draft <- newDraft
    pure $ draft{_draftConversationId = Just convId}

-- | Auto-generate a title from draft content.
draftAutoTitle :: DraftMessage -> Text
draftAutoTitle draft =
    case draft._draftTitle of
        Just t -> t
        Nothing ->
            let content = Text.strip draft._draftContent
                firstLine = case nonEmpty (Text.lines content) of
                    Just lines' -> Text.take 40 $ NE.head lines'
                    Nothing -> ""
             in if Text.null content
                    then "Empty draft"
                    else
                        firstLine
                            <> if Text.length content > 40
                                then "..."
                                else ""

-- | Update draft content and timestamp.
updateDraftContent :: Text -> DraftMessage -> IO DraftMessage
updateDraftContent newContent draft = do
    now <- getCurrentTime
    pure $ draft{_draftContent = newContent, _draftUpdatedAt = now}

-- | Update draft title and timestamp.
updateDraftTitle :: Maybe Text -> DraftMessage -> IO DraftMessage
updateDraftTitle newTitle draft = do
    now <- getCurrentTime
    pure $ draft{_draftTitle = newTitle, _draftUpdatedAt = now}

-- | Add an attachment to a draft.
addDraftAttachment :: DraftAttachment -> DraftMessage -> IO DraftMessage
addDraftAttachment att draft = do
    now <- getCurrentTime
    pure $
        draft
            { _draftAttachments = draft._draftAttachments ++ [att]
            , _draftUpdatedAt = now
            }

-- | Remove an attachment from a draft by index.
removeDraftAttachment :: Int -> DraftMessage -> IO DraftMessage
removeDraftAttachment idx draft = do
    now <- getCurrentTime
    let newAttachments = deleteAt idx draft._draftAttachments
    pure $ draft{_draftAttachments = newAttachments, _draftUpdatedAt = now}

-- | Delete an element at a specific index.
deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = take idx xs ++ drop (idx + 1) xs

-------------------------------------------------------------------------------
-- Auto-save State
-------------------------------------------------------------------------------

-- | Auto-save manager state.
data AutoSaveState = AutoSaveState
    { autoSavePending :: Map DraftId (DraftMessage, UTCTime)
    -- ^ Drafts waiting to be saved (draft + last modified time)
    , autoSaveActiveTimers :: Map DraftId (Async ())
    -- ^ Active debounce timers per draft
    }

-- | Initialize empty auto-save state.
initAutoSaveState :: AutoSaveState
initAutoSaveState = AutoSaveState Map.empty Map.empty

-- | Configuration for auto-save behavior.
data AutoSaveConfig = AutoSaveConfig
    { autoSaveDebounceMs :: Int
    -- ^ Milliseconds to wait after last change before saving (default: 2000)
    , autoSaveMaxWaitMs :: Int
    -- ^ Maximum milliseconds to wait before forcing save (default: 10000)
    }

-- | Default auto-save configuration.
defaultAutoSaveConfig :: AutoSaveConfig
defaultAutoSaveConfig =
    AutoSaveConfig
        { autoSaveDebounceMs = 2000 -- 2 seconds
        , autoSaveMaxWaitMs = 10000 -- 10 seconds
        }

{- | Schedule an auto-save for a draft.

This cancels any existing timer for the draft and starts a new one.
-}
scheduleAutoSave ::
    AutoSaveConfig ->
    (DraftMessage -> IO ()) ->
    AutoSaveState ->
    DraftMessage ->
    IO AutoSaveState
scheduleAutoSave config saveFn state draft = do
    now <- getCurrentTime

    -- Cancel any existing timer for this draft
    case Map.lookup draft._draftId (autoSaveActiveTimers state) of
        Just oldTimer -> cancel oldTimer
        Nothing -> pure ()

    -- Start new timer that will trigger the save
    newTimer <- async $ do
        threadDelay (autoSaveDebounceMs config * 1000)
        saveFn draft

    -- Update state
    pure $
        state
            { autoSavePending =
                Map.insert draft._draftId (draft, now) (autoSavePending state)
            , autoSaveActiveTimers =
                Map.insert draft._draftId newTimer (autoSaveActiveTimers state)
            }

-- | Cancel auto-save for a draft (e.g., when draft is sent/deleted).
cancelAutoSave :: AutoSaveState -> DraftId -> IO AutoSaveState
cancelAutoSave state dId = do
    case Map.lookup dId (autoSaveActiveTimers state) of
        Just timer -> cancel timer
        Nothing -> pure ()

    pure $
        state
            { autoSavePending = Map.delete dId (autoSavePending state)
            , autoSaveActiveTimers = Map.delete dId (autoSaveActiveTimers state)
            }

-- | Get pending draft if it exists.
getPendingDraft :: AutoSaveState -> DraftId -> Maybe DraftMessage
getPendingDraft state dId =
    fst <$> Map.lookup dId (autoSavePending state)

-- | Mark a draft as saved (remove from pending).
markDraftSaved :: AutoSaveState -> DraftId -> AutoSaveState
markDraftSaved state dId =
    state{autoSavePending = Map.delete dId (autoSavePending state)}
