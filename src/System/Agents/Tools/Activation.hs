{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tool activation and lifetime management types.

This module defines types for controlling when and how tools are activated
in different contexts (program, conversation, tool-call scopes).

Activation modes allow tools to be:
- Always visible (always activated)
- On-demand (activated via a toolgroup trigger)
- Available for first N steps only (with optional stickiness)

Lifetime controls how long activation state persists:
- Program: for the entire program lifetime
- Root-conversation: for the root conversation
- Conversation: for the current conversation (default)
- Tool-call: just for the current tool call
-}
module System.Agents.Tools.Activation (
    -- * Lifetime
    Lifetime (..),

    -- * Sticky Mode
    StickyMode (..),

    -- * Activation
    Activation (..),

    -- * Session State
    ToolgroupName,
    ActivationState (..),
    ToolboxSessionState (..),
) where

import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

-------------------------------------------------------------------------------
-- Lifetime
-------------------------------------------------------------------------------

{- | Lifetime scope for tool activation state.

Controls how long an activation state persists before being reset.
-}
data Lifetime
    = -- | Activation state lasts for the entire program lifetime
      ProgramLifetime
    | -- | Activation state lasts for the root conversation
      RootConversationLifetime
    | -- | Activation state lasts for the current conversation (default)
      ConversationLifetime
    | -- | Activation state lasts only for the current tool call
      ToolCallLifetime
    deriving (Show, Eq, Ord, Generic)

-- | Custom JSON encoding as lowercase kebab-case strings
instance ToJSON Lifetime where
    toJSON ProgramLifetime = Aeson.String "program"
    toJSON RootConversationLifetime = Aeson.String "root-conversation"
    toJSON ConversationLifetime = Aeson.String "conversation"
    toJSON ToolCallLifetime = Aeson.String "tool-call"

-- | Custom JSON parsing from lowercase kebab-case strings
instance FromJSON Lifetime where
    parseJSON = Aeson.withText "Lifetime" $ \txt ->
        case txt of
            "program" -> return ProgramLifetime
            "root-conversation" -> return RootConversationLifetime
            "conversation" -> return ConversationLifetime
            "tool-call" -> return ToolCallLifetime
            _ -> fail $ "Unknown Lifetime: " ++ Text.unpack txt

-------------------------------------------------------------------------------
-- Sticky Mode
-------------------------------------------------------------------------------

{- | Stickiness behavior for first-N activation mode.

Controls whether tools remain active after their initial activation period.
-}
data StickyMode
    = -- | Tool remains active if it was used during the initial period
      StickyIfUsed
    | -- | Tool becomes inactive after the initial period regardless of use
      NonSticky
    deriving (Show, Eq, Ord, Generic)

-- | Custom JSON encoding as lowercase kebab-case strings
instance ToJSON StickyMode where
    toJSON StickyIfUsed = Aeson.String "sticky-if-used"
    toJSON NonSticky = Aeson.String "non-sticky"

-- | Custom JSON parsing from lowercase kebab-case strings
instance FromJSON StickyMode where
    parseJSON = Aeson.withText "StickyMode" $ \txt ->
        case txt of
            "sticky-if-used" -> return StickyIfUsed
            "non-sticky" -> return NonSticky
            _ -> fail $ "Unknown StickyMode: " ++ Text.unpack txt

-------------------------------------------------------------------------------
-- Activation
-------------------------------------------------------------------------------

{- | Activation mode for tools/toolgroups.

Controls when and how tools become visible/active in the system.
-}
data Activation
    = -- | Tool is always visible and active
      AlwaysActivated
    | -- | Tool is activated on-demand via a toolgroup trigger
      OnDemandActivated !Text
    | -- | Tool is active for the first N steps, with optional stickiness
      FirstNStepsActivated !Int !StickyMode
    deriving (Show, Eq, Ord, Generic)

-- | Custom JSON encoding with tagged objects
instance ToJSON Activation where
    toJSON AlwaysActivated = Aeson.String "always"
    toJSON (OnDemandActivated toolgroup) =
        Aeson.object
            [ "tag" .= ("on-demand" :: Text)
            , "toolgroup" .= toolgroup
            ]
    toJSON (FirstNStepsActivated steps sticky) =
        Aeson.object
            [ "tag" .= ("first-n-steps" :: Text)
            , "steps" .= steps
            , "sticky" .= sticky
            ]

-- | Custom JSON parsing supporting string and tagged object formats
instance FromJSON Activation where
    parseJSON (Aeson.String "always") = return AlwaysActivated
    parseJSON (Aeson.Object obj) = do
        tag <- obj .: "tag"
        case (tag :: Text) of
            "on-demand" -> OnDemandActivated <$> obj .: "toolgroup"
            "first-n-steps" -> FirstNStepsActivated <$> obj .: "steps" <*> obj .: "sticky"
            _ -> fail $ "Unknown Activation tag: " ++ Text.unpack tag
    parseJSON _ = fail "Activation must be a string \"always\" or a tagged object"

-------------------------------------------------------------------------------
-- Session State
-------------------------------------------------------------------------------

-- | Name of a toolgroup.
type ToolgroupName = Text

{- | Current activation state of a tool/toolgroup.

Tools can be either active (visible/available) or inactive.
-}
data ActivationState = Active | Inactive
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

{- | Session state tracking toolgroup activation states.

This is a monoid that can be built by folding over session turns.
Later state overrides earlier state (last activation state wins).
-}
newtype ToolboxSessionState = ToolboxSessionState
    { getToolboxSessionState :: Map ToolgroupName ActivationState
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Later values override earlier ones
instance Semigroup ToolboxSessionState where
    (ToolboxSessionState m1) <> (ToolboxSessionState m2) =
        ToolboxSessionState (Map.unionWith const m2 m1)

-- | Empty session state
instance Monoid ToolboxSessionState where
    mempty = ToolboxSessionState Map.empty
