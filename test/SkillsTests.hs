{-# LANGUAGE OverloadedRecordDot #-}

{- | Tests for the Skills.md system.

This module tests:
- Skill name validation per agentskills.io spec
- SKILL.md file parsing
-}
module SkillsTests where

import qualified Data.Aeson as Aeson
import Data.Either (isLeft, isRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Prod.Tracer (Tracer, silent)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Tools.Skills.Parser
import System.Agents.Tools.Skills.Types
import System.Agents.Tools.Skills.Toolbox (ScriptName (..))

-------------------------------------------------------------------------------
-- Test Suite
-------------------------------------------------------------------------------

skillsTestSuite :: TestTree
skillsTestSuite =
    testGroup
        "Skills Tests"
        [ skillNameValidationTests
        , skillNameParsingTests
        , skillFileParsingTests
        ]

-------------------------------------------------------------------------------
-- Skill Name Validation Tests
-------------------------------------------------------------------------------

skillNameValidationTests :: TestTree
skillNameValidationTests =
    testGroup
        "Skill Name Validation"
        [ testCase "accepts valid names" $ do
            validateSkillName "pdf-processing" @?= Right (SkillName "pdf-processing")
            validateSkillName "code-review" @?= Right (SkillName "code-review")
            validateSkillName "test123" @?= Right (SkillName "test123")
        , testCase "rejects empty names" $ do
            isLeft (validateSkillName "") @?= True
        , testCase "rejects names too long" $ do
            isLeft (validateSkillName (Text.replicate 65 "a")) @?= True
        , testCase "rejects uppercase" $ do
            isLeft (validateSkillName "PDF-Processing") @?= True
        , testCase "rejects underscores" $ do
            isLeft (validateSkillName "pdf_processing") @?= True
        , testCase "rejects consecutive hyphens" $ do
            isLeft (validateSkillName "pdf--processing") @?= True
        , testCase "rejects leading hyphen" $ do
            isLeft (validateSkillName "-pdf-processing") @?= True
        , testCase "rejects trailing hyphen" $ do
            isLeft (validateSkillName "pdf-processing-") @?= True
        , testCase "accepts single character" $ do
            validateSkillName "a" @?= Right (SkillName "a")
        , testCase "accepts digits" $ do
            validateSkillName "test123" @?= Right (SkillName "test123")
        ]

-------------------------------------------------------------------------------
-- Skill Name JSON/YAML Parsing Tests
-------------------------------------------------------------------------------

skillNameParsingTests :: TestTree
skillNameParsingTests =
    testGroup
        "Skill Name JSON/YAML Parsing"
        [ testCase "parses SkillName from JSON string" $ do
            let json = "\"code-review\""
            case Aeson.eitherDecodeStrict (Text.encodeUtf8 (Text.pack json)) of
                Left err -> assertFailure $ "Failed to parse SkillName: " ++ err
                Right name -> skillNameToText name @?= "code-review"
        , testCase "parses ScriptName from JSON string" $ do
            let json = "\"analyze-changes\""
            case Aeson.eitherDecodeStrict (Text.encodeUtf8 (Text.pack json)) of
                Left err -> assertFailure $ "Failed to parse ScriptName: " ++ err
                Right (ScriptName name) -> name @?= "analyze-changes"
        , testCase "validates SkillName format when parsing" $ do
            let json = "\"Invalid_Name\""
            case Aeson.eitherDecodeStrict (Text.encodeUtf8 (Text.pack json)) :: Either String SkillName of
                Left _ -> pure () -- Expected to fail
                Right _ -> assertFailure "Should reject invalid skill name format"
        ]

-------------------------------------------------------------------------------
-- SKILL.md File Parsing Tests
-------------------------------------------------------------------------------

skillFileParsingTests :: TestTree
skillFileParsingTests =
    testGroup
        "SKILL.md File Parsing"
        [ testCase "parses test/data/skills-01.md successfully" $ do
            let tracer = silent :: Tracer IO System.Agents.Tools.Skills.Parser.Trace
            result <- parseSkillFile tracer "test/data/skills-01.md"
            case result of
                Left err -> assertFailure $ "Failed to parse skill file: " ++ Text.unpack err
                Right skill -> do
                    -- Check metadata
                    skillNameToText (skill.skillMetadata.smName) @?= "code-review"
                    skill.skillMetadata.smDescription @?=
                        "Perform structured code reviews following best practices. Use when reviewing pull requests, auditing code, or evaluating implementation quality."
                    skill.skillMetadata.smLicense @?= Just "MIT"
                    -- Check metadata map
                    Map.lookup "category" (skill.skillMetadata.smMetadata) @?= Just "development"
                    Map.lookup "version" (skill.skillMetadata.smMetadata) @?= Just "1.0"
        , testCase "skill has instructions content" $ do
            let tracer = silent :: Tracer IO System.Agents.Tools.Skills.Parser.Trace
            result <- parseSkillFile tracer "test/data/skills-01.md"
            case result of
                Left err -> assertFailure $ "Failed to parse skill file: " ++ Text.unpack err
                Right skill -> do
                    -- Check that instructions contain expected content
                    let instructions = skill.skillInstructions
                    Text.isInfixOf "# Code Review" instructions @?= True
                    Text.isInfixOf "## Review Checklist" instructions @?= True
                    Text.isInfixOf "### Security" instructions @?= True
                    Text.isInfixOf "### Maintainability" instructions @?= True
        , testCase "skill path is set correctly" $ do
            let tracer = silent :: Tracer IO System.Agents.Tools.Skills.Parser.Trace
            result <- parseSkillFile tracer "test/data/skills-01.md"
            case result of
                Left err -> assertFailure $ "Failed to parse skill file: " ++ Text.unpack err
                Right skill -> do
                    skill.skillPath @?= "test/data"
        , testCase "empty skills directory returns empty list" $ do
            let tracer = silent :: Tracer IO System.Agents.Tools.Skills.Parser.Trace
            -- This tests the directory parsing but doesn't rely on external files
            -- We just verify the function doesn't crash
            result <- parseSkillDirectory tracer "test/data/nonexistent-dir-for-testing"
            case result of
                Left _ -> pure () -- Expected - directory doesn't exist
                Right skills -> skills @?= []
        ]

