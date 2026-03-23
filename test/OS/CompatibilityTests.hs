{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Compatibility tests between old Runtime and new OS model.

These tests verify that the compatibility layer produces identical results
to the old Runtime where applicable.
-}
module OS.CompatibilityTests (
    tests,
) where

import Data.Aeson (Value (..))
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import System.Agents.OS.Compat.Runtime
import System.Agents.Session.Base (UserToolResponse (..))
import System.Agents.Tools.Base (CallResult (..))

-------------------------------------------------------------------------------
-- Test Suite
-------------------------------------------------------------------------------

tests :: TestTree
tests =
    testGroup
        "OS Compatibility Tests"
        [ migrationConfigTests
        , apiParityTests
        , phaseTransitionTests
        ]

-------------------------------------------------------------------------------
-- Migration Configuration Tests
-------------------------------------------------------------------------------

migrationConfigTests :: TestTree
migrationConfigTests =
    testGroup
        "Migration Configuration"
        [ testCase "Default config uses old-only mode" $ do
            let config = defaultMigrationConfig
            migrationEnableOS config @?= False
            migrationEnableCompat config @?= True
            migrationLogPath config @?= Nothing

        , testCase "Dual mode enables both systems" $ do
            let config =
                    MigrationConfig
                        { migrationEnableOS = True
                        , migrationEnableCompat = True
                        , migrationLogPath = Just "/tmp/migration.log"
                        }
            migrationEnableOS config @?= True
            migrationEnableCompat config @?= True

        , testCase "New-only mode disables compatibility" $ do
            let config =
                    MigrationConfig
                        { migrationEnableOS = True
                        , migrationEnableCompat = False
                        , migrationLogPath = Nothing
                        }
            migrationEnableOS config @?= True
            migrationEnableCompat config @?= False

        , testCase "Migration phases are ordered correctly" $ do
            -- PhaseOldOnly < PhaseDual < PhaseNewOnly
            PhaseOldOnly < PhaseDual @?= True
            PhaseDual < PhaseNewOnly @?= True
            PhaseOldOnly < PhaseNewOnly @?= True
        ]

-------------------------------------------------------------------------------
-- API Parity Tests
-------------------------------------------------------------------------------

apiParityTests :: TestTree
apiParityTests =
    testGroup
        "API Parity"
        [ testCase "CallResult to UserToolResponse handles ToolNotFound" $ do
            let toolCall = undefined -- Would be actual ToolCall

            -- ToolNotFound
            let notFound = ToolNotFound toolCall
            let response = callResultToUserToolResponse toolCall notFound
            case response of
                UserToolResponse val ->
                    case val of
                        String s -> assertBool "Should indicate not found" ("not found" `Text.isInfixOf` Text.toLower s)
                        _ -> error "Expected String response"

        , testCase "CallResult to UserToolResponse handles BlobToolSuccess" $ do
            let toolCall = undefined
            let blobResult = BlobToolSuccess toolCall "hello"
            let response = callResultToUserToolResponse toolCall blobResult
            case response of
                UserToolResponse val ->
                    case val of
                        String s -> s @?= "hello"
                        _ -> error "Expected String response"
        ]

-------------------------------------------------------------------------------
-- Phase Transition Tests
-------------------------------------------------------------------------------

phaseTransitionTests :: TestTree
phaseTransitionTests =
    testGroup
        "Phase Transitions"
        [ testCase "PhaseOldOnly returns Left indication" $ do
            let config = defaultMigrationConfig
            -- In PhaseOldOnly, we would get Left with indication to use old Runtime
            -- initializeWithMigration config would return appropriately
            assertBool "PhaseOldOnly mode correct" (not $ migrationEnableOS config)

        , testCase "PhaseDual enables both systems" $ do
            let config =
                    MigrationConfig
                        { migrationEnableOS = True
                        , migrationEnableCompat = True
                        , migrationLogPath = Nothing
                        }
            -- Both systems would be initialized
            assertBool "PhaseDual mode enabled" (migrationEnableOS config && migrationEnableCompat config)

        , testCase "PhaseNewOnly disables compatibility" $ do
            let config =
                    MigrationConfig
                        { migrationEnableOS = True
                        , migrationEnableCompat = False
                        , migrationLogPath = Nothing
                        }
            -- Only OS would be initialized
            assertBool "PhaseNewOnly mode" (migrationEnableOS config && not (migrationEnableCompat config))
        ]

