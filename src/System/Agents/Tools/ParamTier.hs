{-# LANGUAGE DeriveGeneric #-}

{- | Parameter tier types for progressive disclosure.

This module defines the tier system for tool parameters, enabling
progressive disclosure where parameters can be marked as basic, advanced,
or expert to control their visibility in UIs.
-}
module System.Agents.Tools.ParamTier (
    -- * Parameter Tiers
    ParamTier (..),
    defaultParamTier,
    parseParamTier,
    paramTierToText,

    -- * Filtering
    filterParamsByTier,
    isTierVisible,
) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

-------------------------------------------------------------------------------
-- Parameter Tier Types
-------------------------------------------------------------------------------

{- | Tier levels for parameter progressive disclosure.

Parameters marked with different tiers control their visibility:

* 'Basic' - Always shown, essential parameters
* 'Advanced' - Hidden by default, shown when user opts in
* 'Expert' - Deeply hidden, only for power users

The default tier for parameters without explicit marking is 'Basic'
to maintain backward compatibility.
-}
data ParamTier
    = Basic
    | Advanced
    | Expert
    deriving (Show, Eq, Ord, Generic)

instance ToJSON ParamTier where
    toJSON tier = Aeson.String $ paramTierToText tier

instance FromJSON ParamTier where
    parseJSON = Aeson.withText "ParamTier" $ \t ->
        case parseParamTier t of
            Just tier -> pure tier
            Nothing -> fail $ "Invalid ParamTier: " ++ Text.unpack t

{- | Default tier for parameters without explicit marking.
Using 'Basic' ensures backward compatibility.
-}
defaultParamTier :: ParamTier
defaultParamTier = Basic

-- | Parse a ParamTier from Text.
parseParamTier :: Text -> Maybe ParamTier
parseParamTier t = case Text.toLower $ Text.strip t of
    "basic" -> Just Basic
    "advanced" -> Just Advanced
    "expert" -> Just Expert
    _ -> Nothing

-- | Convert a ParamTier to Text.
paramTierToText :: ParamTier -> Text
paramTierToText Basic = "basic"
paramTierToText Advanced = "advanced"
paramTierToText Expert = "expert"

-------------------------------------------------------------------------------
-- Filtering Functions
-------------------------------------------------------------------------------

{- | Filter parameters based on the current disclosure level.

Parameters up to and including the specified tier are shown.
For example, if disclosure level is 'Advanced', both 'Basic' and
'Advanced' parameters are shown, but 'Expert' parameters are hidden.
-}
filterParamsByTier :: ParamTier -> [(a, ParamTier)] -> [a]
filterParamsByTier disclosureLevel paramTiers =
    map fst $ filter (\(_, tier) -> isTierVisible disclosureLevel tier) paramTiers

{- | Check if a parameter tier should be visible at a given disclosure level.

A parameter is visible when its tier is less than or equal to the
disclosure level (i.e., Basic <= Advanced <= Expert).
-}
isTierVisible :: ParamTier -> ParamTier -> Bool
isTierVisible disclosureLevel paramTier =
    tierToInt paramTier <= tierToInt disclosureLevel

-- | Convert tier to integer for comparison (Basic=0, Advanced=1, Expert=2).
tierToInt :: ParamTier -> Int
tierToInt Basic = 0
tierToInt Advanced = 1
tierToInt Expert = 2
