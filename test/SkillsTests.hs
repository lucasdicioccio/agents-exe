{-# LANGUAGE OverloadedRecordDot #-}

{- | Tests for the Skills.md system.

This module tests:
- Skill name validation per agentskills.io spec
- Session state monoid laws
- Progressive disclosure behavior
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
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Session.Types
import System.Agents.Tools.Skills.Parser
import System.Agents.Tools.Skills.State
import System.Agents.Tools.Skills.Types

-------------------------------------------------------------------------------
-- Test Suite
-------------------------------------------------------------------------------

skillsTestSuite :: TestTree
skillsTestSuite =
    testGroup
        "Skills Tests"
        [ skillNameValidationTests
        , skillNameParsingTests
        , sessionStateMonoidTests
        , progressiveDisclosureTests
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
-- Monoid Laws Tests
-------------------------------------------------------------------------------

sessionStateMonoidTests :: TestTree
sessionStateMonoidTests =
    testGroup
        "Session State Monoid"
        [ testCase "satisfies right identity" $ do
            let a = sampleState "skill-a"
            a <> mempty @?= a
        , testCase "satisfies left identity" $ do
            let a = sampleState "skill-a"
            mempty <> a @?= a
        , testCase "satisfies associativity" $ do
            let a = sampleState "skill-a"
                b = sampleState "skill-b"
                c = sampleState "skill-c"
            (a <> b) <> c @?= a <> (b <> c)
        , testCase "later enable overrides earlier disable" $ do
            let skillA = SkillName "skill-a"
                disabled = disableSkill skillA
                enabled = enableSkill skillA
                combined = disabled <> enabled
            getEnabledScripts combined skillA @?= Nothing -- All enabled
        , testCase "earlier state preserved for different skills" $ do
            let skillA = SkillName "skill-a"
                skillB = SkillName "skill-b"
                stateA = enableSkill skillA
                stateB = enableSkill skillB
                combined = stateA <> stateB
            isScriptEnabled combined skillA (ScriptName "test") @?= True
            isScriptEnabled combined skillB (ScriptName "test") @?= True
        ]

-- Helper to create a sample state for a skill
sampleState :: Text -> SkillsSessionState
sampleState name =
    case validateSkillName name of
        Left _ -> mempty
        Right skillName -> enableSkill skillName

-------------------------------------------------------------------------------
-- Progressive Disclosure Tests
-------------------------------------------------------------------------------

progressiveDisclosureTests :: TestTree
progressiveDisclosureTests =
    testGroup
        "Progressive Disclosure"
        [ testCase "scripts disabled by default" $ do
            let skillA = SkillName "skill-a"
                scriptX = ScriptName "script-x"
                state = mempty :: SkillsSessionState
            isScriptEnabled state skillA scriptX @?= False
        , testCase "all scripts enabled after skill enable" $ do
            let skillA = SkillName "skill-a"
                scriptX = ScriptName "script-x"
                scriptY = ScriptName "script-y"
                state = enableSkill skillA
            isScriptEnabled state skillA scriptX @?= True
            isScriptEnabled state skillA scriptY @?= True
        , testCase "getEnabledScripts returns Nothing for enabled skill" $ do
            let skillA = SkillName "skill-a"
                state = enableSkill skillA
            -- Nothing means "all scripts enabled" (no explicit list)
            getEnabledScripts state skillA @?= Nothing
        , testCase "getEnabledScripts returns empty list for disabled skill" $ do
            let skillA = SkillName "skill-a"
                state = mempty :: SkillsSessionState
            getEnabledScripts state skillA @?= Just []
        ]

-------------------------------------------------------------------------------
-- SKILL.md File Parsing Tests
-------------------------------------------------------------------------------

skillFileParsingTests :: TestTree
skillFileParsingTests =
    testGroup
        "SKILL.md File Parsing"
        [ testCase "parses test/data/skills-01.md successfully" $ do
            result <- parseSkillFile "test/data/skills-01.md"
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
            result <- parseSkillFile "test/data/skills-01.md"
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
            result <- parseSkillFile "test/data/skills-01.md"
            case result of
                Left err -> assertFailure $ "Failed to parse skill file: " ++ Text.unpack err
                Right skill -> do
                    skill.skillPath @?= "test/data"
        , testCase "empty skills directory returns empty list" $ do
            -- This tests the directory parsing but doesn't rely on external files
            -- We just verify the function doesn't crash
            result <- parseSkillDirectory "test/data/nonexistent-dir-for-testing"
            case result of
                Left _ -> pure () -- Expected - directory doesn't exist
                Right skills -> skills @?= []
        ]

-------------------------------------------------------------------------------
-- Property Test Placeholders
-------------------------------------------------------------------------------

-- Note: Full property testing would require QuickCheck.
-- These are manual tests for the monoid laws.

-- | Test that the monoid laws hold for SkillsSessionState
prop_monoidRightIdentity :: SkillsSessionState -> Bool
prop_monoidRightIdentity a = a <> mempty == a

prop_monoidLeftIdentity :: SkillsSessionState -> Bool
prop_monoidLeftIdentity a = mempty <> a == a

prop_monoidAssociativity :: SkillsSessionState -> SkillsSessionState -> SkillsSessionState -> Bool
prop_monoidAssociativity a b c = (a <> b) <> c == a <> (b <> c)

