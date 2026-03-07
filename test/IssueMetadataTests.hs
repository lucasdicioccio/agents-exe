{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for GitHub IssueMetadata types and serialization.
--
-- This module contains tests for:
-- * Type parsing (IssueReference, FeatureBranch, FinalTag)
-- * JSON serialization round-trips
-- * Metadata construction and validation
-- * Edge cases (invalid formats, version mismatches)
module IssueMetadataTests where

import Data.Aeson (Value (..), decode, encode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.Tasty
import Test.Tasty.HUnit

-- Module under test
import System.Agents.Tools.GitHub.IssueMetadata as IssueMetadata

-- -----------------------------------------------------------------------------
-- Test Entry Point
-- -----------------------------------------------------------------------------

-- | All IssueMetadata tests.
tests :: TestTree
tests =
    testGroup
        "IssueMetadata Tests"
        [ parsingTests
        , conversionTests
        , jsonTests
        , roundtripTests
        ]

-- -----------------------------------------------------------------------------
-- 1. Parsing Tests
-- -----------------------------------------------------------------------------

-- | Tests for issue reference parsing.
parsingTests :: TestTree
parsingTests =
    testGroup
        "IssueReference Parsing"
        [ test_parseHashReference
        , test_parsePlainNumber
        , test_parseWithWhitespace
        , test_parseInvalidFormats
        ]

-- | Test parsing "#123" format.
test_parseHashReference :: TestTree
test_parseHashReference = testCase "Parses #123 format" $ do
    parseIssueReference "#123" @?= Right (IssueReference 123)
    parseIssueReference "#0" @?= Right (IssueReference 0)
    parseIssueReference "#99999" @?= Right (IssueReference 99999)

-- | Test parsing plain numbers.
test_parsePlainNumber :: TestTree
test_parsePlainNumber = testCase "Parses plain numbers" $ do
    parseIssueReference "123" @?= Right (IssueReference 123)
    parseIssueReference "42" @?= Right (IssueReference 42)

-- | Test parsing with whitespace.
test_parseWithWhitespace :: TestTree
test_parseWithWhitespace = testCase "Strips whitespace" $ do
    parseIssueReference "  #123  " @?= Right (IssueReference 123)
    parseIssueReference "  123  " @?= Right (IssueReference 123)

-- | Test parsing invalid formats.
test_parseInvalidFormats :: TestTree
test_parseInvalidFormats = testCase "Rejects invalid formats" $ do
    -- Empty after hash
    assertBool "Empty after #" $ isLeft $ parseIssueReference "#"
    -- Non-numeric
    assertBool "Non-numeric" $ isLeft $ parseIssueReference "abc"
    assertBool "Hash then non-numeric" $ isLeft $ parseIssueReference "#abc"
    -- Negative numbers
    assertBool "Negative" $ isLeft $ parseIssueReference "#-123"
    -- Extra characters
    assertBool "Extra chars" $ isLeft $ parseIssueReference "#123abc"
  where
    isLeft (Left _) = True
    isLeft (Right _) = False

-- -----------------------------------------------------------------------------
-- 2. Conversion Tests
-- -----------------------------------------------------------------------------

-- | Tests for type conversions.
conversionTests :: TestTree
conversionTests =
    testGroup
        "Type Conversions"
        [ test_toFeatureBranch
        , test_issueReferenceToText
        , test_featureBranchToText
        ]

-- | Test converting issue reference to feature branch.
test_toFeatureBranch :: TestTree
test_toFeatureBranch = testCase "Converts IssueReference to FeatureBranch" $ do
    toFeatureBranch (IssueReference 123) @?= FeatureBranch "gh-123"
    toFeatureBranch (IssueReference 1) @?= FeatureBranch "gh-1"
    toFeatureBranch (IssueReference 9999) @?= FeatureBranch "gh-9999"

-- | Test formatting issue reference as text.
test_issueReferenceToText :: TestTree
test_issueReferenceToText = testCase "Formats IssueReference as text" $ do
    issueReferenceToText (IssueReference 123) @?= "#123"
    issueReferenceToText (IssueReference 1) @?= "#1"
    issueReferenceToText (IssueReference 0) @?= "#0"

-- | Test extracting text from FeatureBranch.
test_featureBranchToText :: TestTree
test_featureBranchToText = testCase "Extracts text from FeatureBranch" $ do
    featureBranchToText (FeatureBranch "gh-123") @?= "gh-123"
    featureBranchToText (FeatureBranch "feature-branch") @?= "feature-branch"

-- -----------------------------------------------------------------------------
-- 3. JSON Serialization Tests
-- -----------------------------------------------------------------------------

-- | Tests for JSON serialization.
jsonTests :: TestTree
jsonTests =
    testGroup
        "JSON Serialization"
        [ test_issueReferenceJson
        , test_finalTagJson
        , test_featureBranchJson
        , test_metadataJsonContent
        , test_metadataFromJson
        , test_metadataVersionCheck
        , test_issueReferenceFromNumber
        ]

-- | Test IssueReference JSON format.
test_issueReferenceJson :: TestTree
test_issueReferenceJson = testCase "IssueReference serializes as #123 string" $ do
    encode (IssueReference 123) @?= "\"#123\""
    -- Round-trip
    decode "\"#123\"" @?= Just (IssueReference 123)
    decode "\"#0\"" @?= Just (IssueReference 0)

-- | Test FinalTag JSON values.
test_finalTagJson :: TestTree
test_finalTagJson = testCase "FinalTag serializes correctly" $ do
    encode FinalMerge @?= "\"final-merge\""
    encode NotFinal @?= "\"not-final\""
    -- Round-trip
    decode "\"final-merge\"" @?= Just FinalMerge
    decode "\"not-final\"" @?= Just NotFinal
    decode "\"FINAL-MERGE\"" @?= Just FinalMerge  -- Case insensitive
    decode "\"NOT_FINAL\"" @?= Just NotFinal

-- | Test FeatureBranch JSON.
test_featureBranchJson :: TestTree
test_featureBranchJson = testCase "FeatureBranch serializes as plain text" $ do
    encode (FeatureBranch "gh-123") @?= "\"gh-123\""
    decode "\"gh-123\"" @?= Just (FeatureBranch "gh-123")

-- | Test full metadata JSON has correct content (keys may be in any order).
test_metadataJsonContent :: TestTree
test_metadataJsonContent = testCase "IssueMetadata has correct JSON content" $ do
    let meta = IssueMetadata
            { imDependencies = [IssueReference 125, IssueReference 126]
            , imFeatureBranch = FeatureBranch "gh-123"
            , imFinalTag = NotFinal
            , imOriginalIssue = Just (IssueReference 123)
            }
    let json = encode meta
    let jsonStr = Text.unpack (Text.decodeUtf8 (LBS.toStrict json))
    -- Check that all expected fields are present (order doesn't matter)
    assertBool "Has metadata version" $ "\"agents_metadata_version\"" `isInfixOf` jsonStr
    assertBool "Has version 1.0" $ "\"1.0\"" `isInfixOf` jsonStr
    assertBool "Has feature_branch" $ "\"feature_branch\"" `isInfixOf` jsonStr
    assertBool "Has gh-123" $ "\"gh-123\"" `isInfixOf` jsonStr
    assertBool "Has dependencies" $ "\"dependencies\"" `isInfixOf` jsonStr
    assertBool "Has #125" $ "\"#125\"" `isInfixOf` jsonStr
    assertBool "Has #126" $ "\"#126\"" `isInfixOf` jsonStr
    assertBool "Has final_tag" $ "\"final_tag\"" `isInfixOf` jsonStr
    assertBool "Has not-final" $ "\"not-final\"" `isInfixOf` jsonStr
    assertBool "Has original_issue" $ "\"original_issue\"" `isInfixOf` jsonStr
    assertBool "Has #123" $ "\"#123\"" `isInfixOf` jsonStr
  where
    isInfixOf needle haystack = Text.isInfixOf (Text.pack needle) (Text.pack haystack)

-- | Test parsing metadata from JSON.
test_metadataFromJson :: TestTree
test_metadataFromJson = testCase "Can parse IssueMetadata from JSON" $ do
    let json = mconcat
            [ "{"
            , "\"agents_metadata_version\":\"1.0\","
            , "\"feature_branch\":\"gh-123\","
            , "\"dependencies\":[\"#125\",\"#126\"],"
            , "\"final_tag\":\"final-merge\""
            , "}"
            ]
    case decode json of
        Nothing -> assertFailure "Failed to parse metadata"
        Just (meta :: IssueMetadata) -> do
            imDependencies meta @?= [IssueReference 125, IssueReference 126]
            imFeatureBranch meta @?= FeatureBranch "gh-123"
            imFinalTag meta @?= FinalMerge
            imOriginalIssue meta @?= Nothing

-- | Test version checking in metadata.
test_metadataVersionCheck :: TestTree
test_metadataVersionCheck = testCase "Rejects unsupported metadata version" $ do
    let json = mconcat
            [ "{"
            , "\"agents_metadata_version\":\"2.0\","
            , "\"feature_branch\":\"gh-123\","
            , "\"dependencies\":[],"
            , "\"final_tag\":\"not-final\""
            , "}"
            ]
    case decode json of
        Just (_ :: IssueMetadata) -> assertFailure "Should have rejected v2.0"
        Nothing -> pure ()  -- Expected failure

-- | Test IssueReference parsing from number in JSON.
test_issueReferenceFromNumber :: TestTree
test_issueReferenceFromNumber = testCase "IssueReference parses from JSON number" $ do
    decode "123" @?= Just (IssueReference 123)
    decode "0" @?= Just (IssueReference 0)
    decode "999" @?= Just (IssueReference 999)

-- -----------------------------------------------------------------------------
-- 4. Round-trip Tests
-- -----------------------------------------------------------------------------

-- | Tests for serialization round-trips.
roundtripTests :: TestTree
roundtripTests =
    testGroup
        "Round-trip Tests"
        [ test_roundtripIssueMetadata
        , test_roundtripEmptyDependencies
        , test_roundtripFinalMerge
        ]

-- | Test full metadata round-trip.
test_roundtripIssueMetadata :: TestTree
test_roundtripIssueMetadata = testCase "IssueMetadata round-trips correctly" $ do
    let original = IssueMetadata
            { imDependencies = [IssueReference 125, IssueReference 126]
            , imFeatureBranch = FeatureBranch "gh-123"
            , imFinalTag = NotFinal
            , imOriginalIssue = Just (IssueReference 123)
            }
    case decode (encode original) of
        Nothing -> assertFailure "Round-trip failed"
        Just parsed -> do
            imDependencies parsed @?= imDependencies original
            imFeatureBranch parsed @?= imFeatureBranch original
            imFinalTag parsed @?= imFinalTag original
            imOriginalIssue parsed @?= imOriginalIssue original

-- | Test metadata with empty dependencies.
test_roundtripEmptyDependencies :: TestTree
test_roundtripEmptyDependencies = testCase "Metadata with no dependencies round-trips" $ do
    let original = IssueMetadata
            { imDependencies = []
            , imFeatureBranch = FeatureBranch "gh-100"
            , imFinalTag = FinalMerge
            , imOriginalIssue = Nothing
            }
    case decode (encode original) of
        Nothing -> assertFailure "Round-trip failed"
        Just parsed -> do
            imDependencies parsed @?= []
            imFinalTag parsed @?= FinalMerge
            imOriginalIssue parsed @?= Nothing

-- | Test final merge metadata round-trip.
test_roundtripFinalMerge :: TestTree
test_roundtripFinalMerge = testCase "Final merge metadata round-trips" $ do
    let original = IssueMetadata
            { imDependencies = [IssueReference 100, IssueReference 101]
            , imFeatureBranch = FeatureBranch "gh-99"
            , imFinalTag = FinalMerge
            , imOriginalIssue = Just (IssueReference 99)
            }
    case decode (encode original) of
        Nothing -> assertFailure "Round-trip failed"
        Just parsed -> do
            imFinalTag parsed @?= FinalMerge
            length (imDependencies parsed) @?= 2

