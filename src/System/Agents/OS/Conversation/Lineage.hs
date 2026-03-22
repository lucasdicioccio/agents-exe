{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
Lineage tracking for nested tool calls and conversation flow.

This module provides types and functions for tracking the full call chain
of agent interactions, enabling:

- Complete audit trails for debugging
- Understanding of tool call hierarchies
- Context propagation through nested calls

== Tradeoffs

1. **Performance**: Building lineage context on every log call may be expensive.
   Consider lazy evaluation or caching for high-frequency logging.

2. **Memory**: Long-running conversations with deep nesting can consume
   significant memory for lineage tracking.

3. **Serialization**: Lineage frames include timestamps which may differ
   across serialized/deserialized values.
-}
module System.Agents.OS.Conversation.Lineage (
    -- * Lineage Types
    Lineage (..),
    LineageFrame (..),
    FrameType (..),

    -- * Construction
    emptyLineage,
    pushLineage,

    -- * Query
    lineageDepth,
    lineageHead,
    lineageRoot,

    -- * Context Building
    buildLineageContext,
    buildLineagePath,

    -- * Frame Queries
    findFramesByType,
    findConversationFrames,
    findTurnFrames,
    findToolCallFrames,

    -- * Utility
    isInConversation,
    isInTurn,
    currentFrameType,

    -- * JSON
    lineageToJson,
) where

import Data.Aeson (FromJSON, ToJSON, Value (..), object, (.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import System.Agents.OS.Core.Types (
    Component (..),
    ComponentTypeId (..),
    ConversationId (..),
    EntityId (..),
    ToolCallId (..),
    TurnId (..),
 )

-------------------------------------------------------------------------------
-- Lineage Types
-------------------------------------------------------------------------------

{- | Lineage tracks the call stack for nested tool calls.

The stack grows from root to current, with the most recent frame at the head.
This enables understanding the full context of any operation.

Example lineage for nested tool calls:

@
Lineage
  [ LineageFrame ToolCallFrame toolCallId3 timestamp3
  , LineageFrame TurnFrame turnId1 timestamp2
  , LineageFrame ConversationFrame convId1 timestamp1
  ]
@

This represents: tool call in a turn in a conversation.
-}
newtype Lineage = Lineage
    { lineageStack :: [LineageFrame]
    -- ^ Stack of frames, most recent first (head = current)
    }
    deriving (Show, Eq, Generic)

instance FromJSON Lineage
instance ToJSON Lineage

instance Component Lineage where
    componentId _ = ComponentTypeId 37

{- | A single frame in the lineage stack.

Each frame represents one level in the call hierarchy with:
- The type of frame (conversation, turn, tool call, etc.)
- The entity ID for that frame
- The timestamp when the frame was created
-}
data LineageFrame = LineageFrame
    { frameType :: FrameType
    -- ^ Type of this frame
    , frameId :: EntityId
    -- ^ Entity ID for this frame (can be cast to specific ID type)
    , frameTimestamp :: UTCTime
    -- ^ When this frame was created
    }
    deriving (Show, Eq, Generic)

instance FromJSON LineageFrame
instance ToJSON LineageFrame

-- | Type of entity in a lineage frame.
data FrameType
    = -- | A conversation frame
      ConversationFrame
    | -- | A turn frame
      TurnFrame
    | -- | A tool call frame
      ToolCallFrame
    | -- | A system-level frame (not tied to conversation)
      SystemFrame
    deriving (Show, Eq, Generic)

instance FromJSON FrameType
instance ToJSON FrameType

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Create an empty lineage (no frames).
emptyLineage :: Lineage
emptyLineage = Lineage []

{- | Push a new frame onto the lineage stack.

The new frame becomes the head of the stack (most recent).
This should be called when entering a new scope (conversation, turn, tool call).

Example:

@
lineage1 = pushLineage ConversationFrame convId now emptyLineage
lineage2 = pushLineage TurnFrame turnId now lineage1
lineage3 = pushLineage ToolCallFrame toolCallId now lineage2
@
-}
pushLineage :: FrameType -> EntityId -> UTCTime -> Lineage -> Lineage
pushLineage fType eid timestamp (Lineage frames) =
    Lineage $ LineageFrame fType eid timestamp : frames

-------------------------------------------------------------------------------
-- Query
-------------------------------------------------------------------------------

-- | Get the depth of the lineage (number of frames).
lineageDepth :: Lineage -> Int
lineageDepth (Lineage frames) = length frames

-- | Get the most recent frame, if any.
lineageHead :: Lineage -> Maybe LineageFrame
lineageHead (Lineage frames) = listToMaybe frames

-- | Get the root frame (first/oldest), if any.
lineageRoot :: Lineage -> Maybe LineageFrame
lineageRoot (Lineage frames) = listToMaybe (reverse frames)

-- | Get the type of the current (most recent) frame, if any.
currentFrameType :: Lineage -> Maybe FrameType
currentFrameType = fmap frameType . lineageHead

-------------------------------------------------------------------------------
-- Context Building
-------------------------------------------------------------------------------

{- | Build a context map from lineage information.

This creates a map suitable for logging, debugging, or passing to tools
that need context about the current call chain.

The map includes:
- @lineageDepth@: Number of frames in the lineage
- @lineagePath@: Human-readable path string
- @currentFrameType@: Type of the current frame
- @rootFrameType@: Type of the root frame
- @conversationId@: ID of the conversation (if in one)
- @turnId@: ID of the current turn (if in one)
- @toolCallId@: ID of the current tool call (if in one)

For performance-critical code, consider caching this result or building
it lazily.
-}
buildLineageContext :: Lineage -> Map Text Value
buildLineageContext lineage =
    Map.fromList $
        [ ("lineageDepth", Number $ fromIntegral $ lineageDepth lineage)
        , ("lineagePath", String $ buildLineagePath lineage)
        ]
            ++ maybe [] (\f -> [("currentFrameType", frameTypeToValue f)]) (fmap frameType $ lineageHead lineage)
            ++ maybe [] (\f -> [("rootFrameType", frameTypeToValue f)]) (fmap frameType $ lineageRoot lineage)
            ++ conversationIdEntry
            ++ turnIdEntry
            ++ toolCallIdEntry
  where
    frameTypeToValue :: FrameType -> Value
    frameTypeToValue ConversationFrame = String "conversation"
    frameTypeToValue TurnFrame = String "turn"
    frameTypeToValue ToolCallFrame = String "tool_call"
    frameTypeToValue SystemFrame = String "system"

    -- Extract specific IDs if present in lineage
    conversationIdEntry = case findConversationId lineage of
        Just cid -> [("conversationId", String $ Text.pack $ show cid)]
        Nothing -> []

    turnIdEntry = case findTurnId lineage of
        Just tid -> [("turnId", String $ Text.pack $ show tid)]
        Nothing -> []

    toolCallIdEntry = case findToolCallId lineage of
        Just tcid -> [("toolCallId", String $ Text.pack $ show tcid)]
        Nothing -> []

{- | Build a human-readable path string from lineage.

Format: @type1:id1/type2:id2/type3:id3@ (root to current)

Example: @conversation:abc123/turn:def456/tool_call:ghi789@
-}
buildLineagePath :: Lineage -> Text
buildLineagePath (Lineage frames) =
    Text.intercalate "/" $ map frameToPath (reverse frames)
  where
    frameToPath :: LineageFrame -> Text
    frameToPath frame =
        frameTypeToText frame.frameType
            <> ":"
            <> (Text.take 8 $ Text.pack $ show frame.frameId)

    frameTypeToText :: FrameType -> Text
    frameTypeToText ConversationFrame = "conversation"
    frameTypeToText TurnFrame = "turn"
    frameTypeToText ToolCallFrame = "tool_call"
    frameTypeToText SystemFrame = "system"

-------------------------------------------------------------------------------
-- Frame Queries
-------------------------------------------------------------------------------

-- | Find all frames of a specific type.
findFramesByType :: FrameType -> Lineage -> [LineageFrame]
findFramesByType fType (Lineage frames) =
    filter ((== fType) . frameType) frames

-- | Find all conversation frames in the lineage.
findConversationFrames :: Lineage -> [LineageFrame]
findConversationFrames = findFramesByType ConversationFrame

-- | Find all turn frames in the lineage.
findTurnFrames :: Lineage -> [LineageFrame]
findTurnFrames = findFramesByType TurnFrame

-- | Find all tool call frames in the lineage.
findToolCallFrames :: Lineage -> [LineageFrame]
findToolCallFrames = findFramesByType ToolCallFrame

-- | Extract conversation ID from lineage if present.
findConversationId :: Lineage -> Maybe ConversationId
findConversationId lineage =
    case findConversationFrames lineage of
        (frame : _) -> Just $ ConversationId frame.frameId
        [] -> Nothing

-- | Extract turn ID from lineage if present.
findTurnId :: Lineage -> Maybe TurnId
findTurnId lineage =
    case findTurnFrames lineage of
        (frame : _) -> Just $ TurnId frame.frameId
        [] -> Nothing

-- | Extract tool call ID from lineage if present (most recent).
findToolCallId :: Lineage -> Maybe ToolCallId
findToolCallId lineage =
    case findToolCallFrames lineage of
        (frame : _) -> Just $ ToolCallId frame.frameId
        [] -> Nothing

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Check if the lineage includes a specific conversation.
isInConversation :: ConversationId -> Lineage -> Bool
isInConversation cid lineage =
    cid `elem` map (ConversationId . frameId) (findConversationFrames lineage)

-- | Check if the lineage includes a specific turn.
isInTurn :: TurnId -> Lineage -> Bool
isInTurn tid lineage =
    tid `elem` map (TurnId . frameId) (findTurnFrames lineage)

-------------------------------------------------------------------------------
-- JSON Helpers
-------------------------------------------------------------------------------

-- | Convert lineage to a structured JSON object for logging.
lineageToJson :: Lineage -> Value
lineageToJson (Lineage frames) =
    object
        [ "depth" .= length frames
        , "frames" .= map frameToJson frames
        , "path" .= buildLineagePath (Lineage frames)
        ]
  where
    frameToJson :: LineageFrame -> Value
    frameToJson frame =
        object
            [ "type" .= frameTypeToText frame.frameType
            , "id" .= show frame.frameId
            , "timestamp" .= frame.frameTimestamp
            ]

    frameTypeToText :: FrameType -> Text
    frameTypeToText ConversationFrame = "conversation"
    frameTypeToText TurnFrame = "turn"
    frameTypeToText ToolCallFrame = "tool_call"
    frameTypeToText SystemFrame = "system"
