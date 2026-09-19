{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : System.Agents.TUI.Buffer
Description : Inline draft buffer system for the TUI

This module provides an inline buffer system for temporary message storage
in the TUI. It allows quick stashing and resuming of message content directly
from the Chats tab, replacing the need for a separate DraftsTab.

Buffers are identified by UUIDs and can be chained for future versioning support.
-}
module System.Agents.TUI.Buffer (
    -- * Buffer Types
    BufferId (..),
    Buffer (..),

    -- * Lenses
    bufferId,
    bufferContent,
    bufferCreatedAt,
    bufferParentBuffer,

    -- * Buffer Creation
    newBuffer,
    newBufferWithContent,

    -- * Buffer Operations
    updateBufferContent,
) where

import Control.Lens (makeLenses)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID

-------------------------------------------------------------------------------
-- Buffer Types
-------------------------------------------------------------------------------

-- | A text buffer for temporary message storage
newtype BufferId = BufferId UUID
    deriving (Show, Eq, Ord)

-- | A buffer containing message content and metadata
data Buffer = Buffer
    { _bufferId :: BufferId
    , _bufferContent :: Text
    , _bufferCreatedAt :: UTCTime
    , _bufferParentBuffer :: Maybe BufferId
    -- ^ For future versioning support
    }
    deriving (Show, Eq)

makeLenses ''Buffer

-------------------------------------------------------------------------------
-- Buffer Creation
-------------------------------------------------------------------------------

-- | Create a new empty buffer
newBuffer :: IO Buffer
newBuffer = do
    uuid <- UUID.nextRandom
    now <- getCurrentTime
    pure $
        Buffer
            { _bufferId = BufferId uuid
            , _bufferContent = ""
            , _bufferCreatedAt = now
            , _bufferParentBuffer = Nothing
            }

-- | Create a buffer with content
newBufferWithContent :: Text -> IO Buffer
newBufferWithContent content = do
    uuid <- UUID.nextRandom
    now <- getCurrentTime
    pure $
        Buffer
            { _bufferId = BufferId uuid
            , _bufferContent = content
            , _bufferCreatedAt = now
            , _bufferParentBuffer = Nothing
            }

-------------------------------------------------------------------------------
-- Buffer Operations
-------------------------------------------------------------------------------

-- | Update buffer content (creates new timestamp)
updateBufferContent :: Text -> Buffer -> IO Buffer
updateBufferContent newContent buffer = do
    now <- getCurrentTime
    pure $
        buffer
            { _bufferContent = newContent
            , _bufferCreatedAt = now
            }
