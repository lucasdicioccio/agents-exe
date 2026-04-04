{-# LANGUAGE OverloadedRecordDot #-}

{- | Tests for the toolbox activation session state system.

This module tests:
- Session state monoid laws
- Meta-tool call parsing
- Progressive disclosure behavior
- Toolgroup activation/deactivation
-}
module ActivationSessionTests where

import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Session.Types
import System.Agents.Tools.Activation
import System.Agents.Tools.Activation.Session

-------------------------------------------------------------------------------
-- Test Suite
-------------------------------------------------------------------------------

activationSessionTestSuite :: TestTree
activationSessionTestSuite =
    testGroup
        "Activation Session Tests"
        [ sessionStateMonoidTests
        , toolCallParsingTests
        , sessionFoldingTests
        , progressiveDisclosureTests
        , stateQueryTests
        ]

-------------------------------------------------------------------------------
-- Monoid Laws Tests
-------------------------------------------------------------------------------

sessionStateMonoidTests :: TestTree
sessionStateMonoidTests =
    testGroup
        "Session State Monoid"
        [ testCase "satisfies right identity" $ do
            let a = sampleState "group-a" Active
            (a <> mempty) @?= a
        , testCase "satisfies left identity" $ do
            let a = sampleState "group-a" Active
            (mempty <> a) @?= a
        , testCase "satisfies associativity" $ do
            let a = sampleState "group-a" Active
                b = sampleState "group-b" Inactive
                c = sampleState "group-c" Active
            ((a <> b) <> c) @?= (a <> (b <> c))
        , testCase "later state overrides earlier state for same group" $ do
            let groupA = "test-group"
                activated = activateToolgroup groupA
                deactivated = deactivateToolgroup groupA
                combined = activated <> deactivated
            isToolgroupActive combined groupA @?= False
        , testCase "earlier state preserved for different groups" $ do
            let groupA = "group-a"
                groupB = "group-b"
                stateA = activateToolgroup groupA
                stateB = activateToolgroup groupB
                combined = stateA <> stateB
            isToolgroupActive combined groupA @?= True
            isToolgroupActive combined groupB @?= True
        , testCase "union combines different groups" $ do
            let stateA = activateToolgroup "group-a"
                stateB = deactivateToolgroup "group-b"
                combined = stateA <> stateB
            getActiveToolgroups combined @?= ["group-a"]
        ]

-- Helper to create a sample state for a toolgroup
sampleState :: ToolgroupName -> ActivationState -> ToolboxSessionState
sampleState name state = ToolboxSessionState $ Map.singleton name state

-------------------------------------------------------------------------------
-- Tool Call Parsing Tests
-------------------------------------------------------------------------------

toolCallParsingTests :: TestTree
toolCallParsingTests =
    testGroup
        "Tool Call Parsing"
        [ testCase "extracts function name from tool call" $ do
            let toolCall = createToolCall "meta_activate_tool" $ Just $ Aeson.object [("toolgroup", Aeson.String "test-group")]
            extractFromToolCall toolCall @?= activateToolgroup "test-group"
        , testCase "parses meta_activate_tool with object arguments" $ do
            let toolCall = createToolCall "meta_activate_tool" $ Just $ Aeson.object [("toolgroup", Aeson.String "filesystem")]
            isToolgroupActive (extractFromToolCall toolCall) "filesystem" @?= True
        , testCase "parses meta_deactivate_tool with object arguments" $ do
            let toolCall = createToolCall "meta_deactivate_tool" $ Just $ Aeson.object [("toolgroup", Aeson.String "filesystem")]
            isToolgroupActive (extractFromToolCall toolCall) "filesystem" @?= False
        , testCase "ignores meta_discover_tools" $ do
            let toolCall = createToolCall "meta_discover_tools" Nothing
            extractFromToolCall toolCall @?= mempty
        , testCase "ignores unknown tool calls" $ do
            let toolCall = createToolCall "some_other_tool" Nothing
            extractFromToolCall toolCall @?= mempty
        , testCase "ignores skill_enable calls" $ do
            let toolCall = createToolCall "skill_enable_pdf-processing" Nothing
            extractFromToolCall toolCall @?= mempty
        , testCase "ignores skill_disable calls" $ do
            let toolCall = createToolCall "skill_disable_pdf-processing" Nothing
            extractFromToolCall toolCall @?= mempty
        , testCase "returns mempty for missing toolgroup argument" $ do
            let toolCall = createToolCall "meta_activate_tool" $ Just $ Aeson.object [("other_arg", Aeson.String "value")]
            extractFromToolCall toolCall @?= mempty
        , testCase "handles empty tool call" $ do
            let toolCall = LlmToolCall $ Aeson.object []
            extractFromToolCall toolCall @?= mempty
        ]

-- Helper to create a tool call with given function name and arguments
createToolCall :: Text -> Maybe Aeson.Value -> LlmToolCall
createToolCall funcName mbArgs =
    let funcObj = case mbArgs of
            Just args -> Aeson.object ["name" Aeson..= funcName, "arguments" Aeson..= args]
            Nothing -> Aeson.object ["name" Aeson..= funcName]
     in LlmToolCall $ Aeson.object ["function" Aeson..= funcObj]

-------------------------------------------------------------------------------
-- Session Folding Tests
-------------------------------------------------------------------------------

sessionFoldingTests :: TestTree
sessionFoldingTests =
    testGroup
        "Session Folding"
        [ testCase "empty session produces empty state" $ do
            let session = createEmptySession
            foldSession session @?= mempty
        , testCase "single activate call in session" $ do
            let session = createSessionWithToolCalls [("meta_activate_tool", "test-group")]
                state = foldSession session
            isToolgroupActive state "test-group" @?= True
        , testCase "single deactivate call in session" $ do
            let session = createSessionWithToolCalls [("meta_deactivate_tool", "test-group")]
                state = foldSession session
            isToolgroupActive state "test-group" @?= False
        , testCase "multiple activations in session" $ do
            let session = createSessionWithToolCalls
                    [ ("meta_activate_tool", "group-a")
                    , ("meta_activate_tool", "group-b")
                    ]
                state = foldSession session
            isToolgroupActive state "group-a" @?= True
            isToolgroupActive state "group-b" @?= True
        , testCase "activation then deactivation of same group" $ do
            let session = createSessionWithToolCalls
                    [ ("meta_activate_tool", "test-group")
                    , ("meta_deactivate_tool", "test-group")
                    ]
                state = foldSession session
            isToolgroupActive state "test-group" @?= False
        , testCase "deactivation then activation of same group" $ do
            let session = createSessionWithToolCalls
                    [ ("meta_deactivate_tool", "test-group")
                    , ("meta_activate_tool", "test-group")
                    ]
                state = foldSession session
            isToolgroupActive state "test-group" @?= True
        , testCase "user turns don't affect state" $ do
            let sessionId = SessionId (UUID.nil)
                turnId = TurnId (UUID.nil)
                userContent = UserTurnContent (SystemPrompt "test") [] Nothing []
                userTurn = UserTurn userContent Nothing
                session = Session {turns = [userTurn], sessionId = sessionId, forkedFromSessionId = Nothing, turnId = turnId}
            foldSession session @?= mempty
        ]

-- Helper to create an empty session
createEmptySession :: Session
createEmptySession =
    Session
        { turns = []
        , sessionId = SessionId UUID.nil
        , forkedFromSessionId = Nothing
        , turnId = TurnId UUID.nil
        }

-- Helper to create a session with a list of (tool_name, toolgroup) pairs
createSessionWithToolCalls :: [(Text, Text)] -> Session
createSessionWithToolCalls calls =
    let toolCalls = map (\(name, group) -> createToolCall name (Just $ Aeson.object [("toolgroup", Aeson.String group)])) calls
        -- LlmResponse now includes responseTokenUsage as 4th field
        llmContent = LlmTurnContent (LlmResponse Nothing Nothing Aeson.Null Nothing) toolCalls
        llmTurn = LlmTurn llmContent Nothing
     in Session
            { turns = [llmTurn]
            , sessionId = SessionId UUID.nil
            , forkedFromSessionId = Nothing
            , turnId = TurnId UUID.nil
            }

-------------------------------------------------------------------------------
-- Progressive Disclosure Tests
-------------------------------------------------------------------------------

progressiveDisclosureTests :: TestTree
progressiveDisclosureTests =
    testGroup
        "Progressive Disclosure"
        [ testCase "toolgroups active by default" $ do
            let state = mempty :: ToolboxSessionState
            isToolgroupActive state "any-group" @?= True
        , testCase "toolgroup becomes inactive after explicit deactivation" $ do
            let state = deactivateToolgroup "test-group"
            isToolgroupActive state "test-group" @?= False
        , testCase "toolgroup becomes active after explicit activation" $ do
            let state = activateToolgroup "test-group"
            isToolgroupActive state "test-group" @?= True
        , testCase "deactivation only affects specified group" $ do
            let state = deactivateToolgroup "group-a"
            isToolgroupActive state "group-a" @?= False
            isToolgroupActive state "group-b" @?= True
        , testCase "activation only affects specified group" $ do
            let state = activateToolgroup "group-a" <> deactivateToolgroup "group-b"
            isToolgroupActive state "group-a" @?= True
            isToolgroupActive state "group-b" @?= False
        ]

-------------------------------------------------------------------------------
-- State Query Tests
-------------------------------------------------------------------------------

stateQueryTests :: TestTree
stateQueryTests =
    testGroup
        "State Query Functions"
        [ testCase "getActiveToolgroups returns only active groups" $ do
            let state = activateToolgroup "active-a" <> deactivateToolgroup "inactive-b" <> activateToolgroup "active-c"
            let active = getActiveToolgroups state
            length active @?= 2
            all (`elem` ["active-a", "active-c"]) active @?= True
        , testCase "getActiveToolgroups returns empty for all-inactive state" $ do
            let state = deactivateToolgroup "a" <> deactivateToolgroup "b"
            getActiveToolgroups state @?= []
        , testCase "getActiveToolgroups returns empty for empty state" $ do
            getActiveToolgroups (mempty :: ToolboxSessionState) @?= []
        , testCase "isToolgroupActive handles untracked groups" $ do
            let state = activateToolgroup "tracked"
            isToolgroupActive state "untracked" @?= True
        ]

-------------------------------------------------------------------------------
-- Property Test Placeholders
-------------------------------------------------------------------------------

-- Note: Full property testing would require QuickCheck.
-- These are manual tests for the monoid laws.

-- | Test that the monoid laws hold for ToolboxSessionState
prop_monoidRightIdentity :: ToolboxSessionState -> Bool
prop_monoidRightIdentity a = a <> mempty == a

prop_monoidLeftIdentity :: ToolboxSessionState -> Bool
prop_monoidLeftIdentity a = mempty <> a == a

prop_monoidAssociativity :: ToolboxSessionState -> ToolboxSessionState -> ToolboxSessionState -> Bool
prop_monoidAssociativity a b c = (a <> b) <> c == a <> (b <> c)

