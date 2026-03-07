{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Feature Branch Isolation metadata for GitHub issues.
--
-- This module defines types and serialization logic for storing feature branch
-- workflow metadata in GitHub issue bodies. The metadata enables:
--
-- * Feature branch isolation - each issue belongs to a specific feature branch
-- * Dependency tracking - issues can declare dependencies on other issues
-- * Final merge tracking - a final issue can be marked for merging to main
-- * Original issue tracking - sub-issues can reference back to parent issues
--
-- See docs/sqq-dag.md for the full specification.
module System.Agents.Tools.GitHub.IssueMetadata (
    -- * Core types
    FeatureBranch (..),
    FinalTag (..),
    IssueReference (..),
    IssueMetadata (..),

    -- * Conversion functions
    parseIssueReference,
    toFeatureBranch,
    featureBranchToText,
    issueReferenceToText,

    -- * JSON serialization helpers
    metadataToJson,
    metadataFromJson,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), fromJSON, withObject, (.:), (.:?), Result(..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Key (Key)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read
import GHC.Generics (Generic)

-- -----------------------------------------------------------------------------
-- JSON Helper Types
-- -----------------------------------------------------------------------------

-- | Aeson Pair type for building JSON objects.
type Pair = (Key, Value)

-- | Optional field helper - only include if value is present.
(.=?) :: (ToJSON v) => Key -> Maybe v -> Maybe Pair
(.=?) k v = fmap ((Aeson..=) k) v

infixr 8 .=?

-- | Required field helper.
(.=) :: (ToJSON v) => Key -> v -> Maybe Pair
(.=) k v = Just ((Aeson..=) k v)

infixr 8 .=

-- | Build JSON object from a list of optional pairs.
object :: [Maybe Pair] -> Value
object = Aeson.object . catMaybes

-- -----------------------------------------------------------------------------
-- Core Types
-- -----------------------------------------------------------------------------

-- | Feature branch name, typically derived from issue number (e.g., "gh-123")
newtype FeatureBranch = FeatureBranch {unFeatureBranch :: Text}
    deriving (Show, Eq, Ord, Generic)

instance ToJSON FeatureBranch where
    toJSON = toJSON . unFeatureBranch

instance FromJSON FeatureBranch where
    parseJSON = fmap FeatureBranch . parseJSON

-- | Tag indicating if this is the final merge issue
--
-- A 'FinalMerge' issue is the "ultimate PR" that merges the feature
-- branch into main. All other issues are 'NotFinal'.
data FinalTag = FinalMerge | NotFinal
    deriving (Show, Eq, Ord, Generic)

instance ToJSON FinalTag where
    toJSON FinalMerge = "final-merge"
    toJSON NotFinal = "not-final"

instance FromJSON FinalTag where
    parseJSON = Aeson.withText "FinalTag" $ \t ->
        case Text.toLower t of
            "final-merge" -> pure FinalMerge
            "final" -> pure FinalMerge
            "not-final" -> pure NotFinal
            "not_final" -> pure NotFinal
            _ -> fail $ "Unknown final tag: " ++ Text.unpack t

-- | Reference to another issue (e.g., #123)
--
-- Stores the numeric issue number. The '#' prefix is added during
-- formatting/serialization.
newtype IssueReference = IssueReference {unIssueReference :: Int}
    deriving (Show, Eq, Ord, Generic)

instance ToJSON IssueReference where
    -- Serialize as "#123" string format for readability
    toJSON = toJSON . issueReferenceToText

instance FromJSON IssueReference where
    -- Parse either "#123" or plain integer 123
    parseJSON (String s) =
        case parseIssueReference s of
            Left err -> fail err
            Right ref -> pure ref
    parseJSON (Number n) =
        -- Convert scientific to integer, handling whole numbers
        -- First check if it's a whole number
        let s = show n
            sNoDecimal = takeWhile (/= '.') s
        in case sNoDecimal of
            "" -> fail "Empty number"
            numStr -> case Text.Read.decimal (Text.pack numStr) of
                Right (intVal, "") -> pure $ IssueReference intVal
                _ -> fail $ "Could not parse as integer: " ++ s
    parseJSON _ = fail "IssueReference must be a string or number"

-- | Complete metadata for a GitHub issue in the feature branch workflow
--
-- This metadata is stored as JSON within the issue body or as an issue
-- field. It tracks:
--
-- * Dependencies - issues that must be completed before this one
-- * Feature branch - the branch this issue belongs to
-- * Final tag - whether this is the ultimate merge issue
-- * Original issue - for sub-issues, tracks back to the parent issue
data IssueMetadata = IssueMetadata
    { imDependencies :: [IssueReference]
    -- ^ Dependency issue numbers (e.g., [#125, #126])
    , imFeatureBranch :: FeatureBranch
    -- ^ The feature branch this issue belongs to
    , imFinalTag :: FinalTag
    -- ^ Whether this is the final merge issue
    , imOriginalIssue :: Maybe IssueReference
    -- ^ Reference to the parent/original issue
    }
    deriving (Show, Eq, Generic)

instance ToJSON IssueMetadata where
    toJSON meta =
        object
            [ "agents_metadata_version" .= Just ("1.0" :: Text)
            , "feature_branch" .= Just (imFeatureBranch meta)
            , "dependencies" .= Just (imDependencies meta)
            , "final_tag" .= Just (imFinalTag meta)
            , "original_issue" .=? imOriginalIssue meta
            ]

instance FromJSON IssueMetadata where
    parseJSON = withObject "IssueMetadata" $ \o -> do
        -- Version check (optional for forward compatibility)
        mVersion <- o .:? "agents_metadata_version"
        case mVersion of
            Just (versionStr :: Text) 
                | versionStr == "1.0" -> pure ()
                | otherwise -> fail $ "Unsupported metadata version: " ++ Text.unpack versionStr
            Nothing -> pure () -- No version field, assume current

        IssueMetadata
            <$> o .: "dependencies"
            <*> o .: "feature_branch"
            <*> o .: "final_tag"
            <*> o .:? "original_issue"

-- -----------------------------------------------------------------------------
-- Parsing and Conversion Functions
-- -----------------------------------------------------------------------------

-- | Parse an issue reference from text like "#123" or "125"
--
-- Accepts:
-- * "#123" - standard GitHub issue reference format
-- * "125" - plain number (parses as issue 125)
--
-- Returns 'Left' error message on invalid input.
parseIssueReference :: Text -> Either String IssueReference
parseIssueReference txt =
    let trimmed = Text.strip txt
     in case Text.uncons trimmed of
            Just ('#', rest)
                | Text.null rest -> Left "Empty issue number after #"
                | otherwise -> parseNumber rest
            Just (c, _) | c >= '0' && c <= '9' -> parseNumber trimmed
            _ -> Left $ "Invalid issue reference format: " ++ Text.unpack txt
  where
    parseNumber numText =
        case Text.Read.decimal numText of
            Right (n, "") -> Right $ IssueReference n
            Right (_, leftover) -> Left $ "Unexpected characters after issue number: " ++ Text.unpack leftover
            Left err -> Left $ "Failed to parse issue number: " ++ err

-- | Normalize an issue number to a feature branch name
--
-- >>> toFeatureBranch (IssueReference 123)
-- FeatureBranch {unFeatureBranch = "gh-123"}
toFeatureBranch :: IssueReference -> FeatureBranch
toFeatureBranch ref = FeatureBranch $ "gh-" <> Text.pack (show $ unIssueReference ref)

-- | Convert FeatureBranch to a valid git branch name
--
-- >>> featureBranchToText (FeatureBranch "gh-123")
-- "gh-123"
featureBranchToText :: FeatureBranch -> Text
featureBranchToText = unFeatureBranch

-- | Format an IssueReference as text (e.g., "#123")
--
-- >>> issueReferenceToText (IssueReference 123)
-- "#123"
issueReferenceToText :: IssueReference -> Text
issueReferenceToText ref = "#" <> Text.pack (show $ unIssueReference ref)

-- -----------------------------------------------------------------------------
-- JSON Serialization Helpers
-- -----------------------------------------------------------------------------

-- | Convert IssueMetadata to JSON Value
--
-- This is a convenience function for cases where you need the raw JSON
-- rather than encoding to a string.
metadataToJson :: IssueMetadata -> Value
metadataToJson = toJSON

-- | Parse IssueMetadata from JSON Value
--
-- Returns 'Left' error message on parse failure.
metadataFromJson :: Value -> Either String IssueMetadata
metadataFromJson v = case fromJSON v of
    Success a -> Right a
    Error err -> Left err

