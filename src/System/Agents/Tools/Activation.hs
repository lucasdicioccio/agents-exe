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
-}
module System.Agents.Tools.Activation (
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
    deriving (Show, Eq, Ord, Generic)

-- | Custom JSON encoding with tagged objects
instance ToJSON Activation where
    toJSON AlwaysActivated = Aeson.String "always"
    toJSON (OnDemandActivated toolgroup) =
        Aeson.object
            [ "tag" .= ("on-demand" :: Text)
            , "toolgroup" .= toolgroup
            ]

-- | Custom JSON parsing supporting string and tagged object formats
instance FromJSON Activation where
    parseJSON (Aeson.String "always") = return AlwaysActivated
    parseJSON (Aeson.Object obj) = do
        tag <- obj .: "tag"
        case (tag :: Text) of
            "on-demand" -> OnDemandActivated <$> obj .: "toolgroup"
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
