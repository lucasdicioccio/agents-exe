{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module SessionEditTests where

import Data.Aeson (Value(..), (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import qualified System.Agents.Session.Edit as SessionEdit
import qualified System.Agents.Session.Types as Session

-------------------------------------------------------------------------------
-- Test Suite
-------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Session Edit"
    [ takeTests
    , takeTailTests
    , dropTests
    , dropTailTests
    , censorToolCallsTests
    , censorThinkingTests
    , compositionTests
    , edgeCaseTests
    ]

-------------------------------------------------------------------------------
-- Test Data Helpers
-------------------------------------------------------------------------------

-- | Create a simple test session with alternating UserTurn and LlmTurn
mkTestSession :: Int -> Session.Session
mkTestSession n = Session.Session
    { Session.turns = take n $ cycle
        [ Session.UserTurn $ Session.UserTurnContent
            { Session.userPrompt = Session.SystemPrompt "System prompt"
            , Session.userTools = []
            , Session.userQuery = Just $ Session.UserQuery "User query"
            , Session.userToolResponses = []
            }
        , Session.LlmTurn $ Session.LlmTurnContent
            { Session.llmResponse = Session.LlmResponse
                { Session.responseText = Just "LLM response"
                , Session.responseThinking = Just "Thinking content..."
                , Session.rawResponse = Aeson.object []
                }
            , Session.llmToolCalls = []
            }
        ]
    , Session.sessionId = Session.SessionId undefined  -- Use undefined for tests
    , Session.forkedFromSessionId = Nothing
    , Session.turnId = Session.TurnId undefined  -- Use undefined for tests
    }

-- | Create a test session with tool calls
mkSessionWithToolCalls :: Session.Session
mkSessionWithToolCalls = Session.Session
    { Session.turns =
        [ Session.UserTurn $ Session.UserTurnContent
            { Session.userPrompt = Session.SystemPrompt "System"
            , Session.userTools = []
            , Session.userQuery = Just $ Session.UserQuery "Hello"
            , Session.userToolResponses = []
            }
        , Session.LlmTurn $ Session.LlmTurnContent
            { Session.llmResponse = Session.LlmResponse
                { Session.responseText = Just "I'll help"
                , Session.responseThinking = Just "Let me think..."
                , Session.rawResponse = Aeson.object []
                }
            , Session.llmToolCalls =
                [ Session.LlmToolCall $ Aeson.object
                    [ "function" .= Aeson.object ["name" .= ("tool1" :: Text)]
                    , "arguments" .= Aeson.object []
                    ]
                ]
            }
        , Session.UserTurn $ Session.UserTurnContent
            { Session.userPrompt = Session.SystemPrompt "System"
            , Session.userTools = []
            , Session.userQuery = Nothing
            , Session.userToolResponses =
                [ ( Session.LlmToolCall $ Aeson.object ["name" .= ("tool1" :: Text)]
                  , Session.UserToolResponse $ Aeson.object ["result" .= ("done" :: Text)]
                  )
                ]
            }
        ]
    , Session.sessionId = Session.SessionId undefined
    , Session.forkedFromSessionId = Nothing
    , Session.turnId = Session.TurnId undefined
    }

-------------------------------------------------------------------------------
-- Take Tests
-------------------------------------------------------------------------------

takeTests :: TestTree
takeTests = testGroup "sessionEditTake"
    [ testCase "take 0 returns empty session" $ do
        let session = mkTestSession 10
        let result = SessionEdit.sessionEditTake 0 session
        length result.turns @?= 0

    , testCase "take N from large session returns N turns" $ do
        let session = mkTestSession 10
        let result = SessionEdit.sessionEditTake 3 session
        length result.turns @?= 3

    , testCase "take N > length returns all turns" $ do
        let session = mkTestSession 5
        let result = SessionEdit.sessionEditTake 10 session
        length result.turns @?= 5

    , testCase "take preserves other session fields" $ do
        let session = mkTestSession 5
        let result = SessionEdit.sessionEditTake 2 session
        result.forkedFromSessionId @?= session.forkedFromSessionId

    , testCase "take negative returns empty session" $ do
        let session = mkTestSession 5
        let result = SessionEdit.sessionEditTake (-1) session
        length result.turns @?= 0
    ]

-------------------------------------------------------------------------------
-- Take Tail Tests
-------------------------------------------------------------------------------

takeTailTests :: TestTree
takeTailTests = testGroup "sessionEditTakeTail"
    [ testCase "takeTail 0 returns empty session" $ do
        let session = mkTestSession 10
        let result = SessionEdit.sessionEditTakeTail 0 session
        length result.turns @?= 0

    , testCase "takeTail N from large session returns N last turns" $ do
        let session = mkTestSession 10
        let result = SessionEdit.sessionEditTakeTail 3 session
        length result.turns @?= 3

    , testCase "takeTail N > length returns all turns" $ do
        let session = mkTestSession 5
        let result = SessionEdit.sessionEditTakeTail 10 session
        length result.turns @?= 5

    , testCase "takeTail returns last N turns" $ do
        let session = mkTestSession 4  -- User, LLM, User, LLM
        let result = SessionEdit.sessionEditTakeTail 2 session
        length result.turns @?= 2
        -- The last two turns should be UserTurn (index 2) and LlmTurn (index 3)
        case result.turns of
            [Session.UserTurn _, Session.LlmTurn _] -> pure ()
            _ -> assertFailure "Expected [UserTurn, LlmTurn]"

    , testCase "takeTail negative returns empty session" $ do
        let session = mkTestSession 5
        let result = SessionEdit.sessionEditTakeTail (-1) session
        length result.turns @?= 0
    ]

-------------------------------------------------------------------------------
-- Drop Tests
-------------------------------------------------------------------------------

dropTests :: TestTree
dropTests = testGroup "sessionEditDrop"
    [ testCase "drop 0 returns same session" $ do
        let session = mkTestSession 10
        let result = SessionEdit.sessionEditDrop 0 session
        length result.turns @?= 10

    , testCase "drop N from large session removes first N" $ do
        let session = mkTestSession 10
        let result = SessionEdit.sessionEditDrop 3 session
        length result.turns @?= 7

    , testCase "drop N >= length returns empty session" $ do
        let session = mkTestSession 5
        let result = SessionEdit.sessionEditDrop 5 session
        length result.turns @?= 0

    , testCase "drop N > length returns empty session" $ do
        let session = mkTestSession 5
        let result = SessionEdit.sessionEditDrop 10 session
        length result.turns @?= 0

    , testCase "drop negative returns same session" $ do
        let session = mkTestSession 5
        let result = SessionEdit.sessionEditDrop (-1) session
        length result.turns @?= 5

    , testCase "drop removes first N turns" $ do
        let session = mkTestSession 4  -- User, LLM, User, LLM
        let result = SessionEdit.sessionEditDrop 2 session
        length result.turns @?= 2
        -- After dropping 2, we have: User (index 2), LLM (index 3)
        case result.turns of
            [Session.UserTurn _, Session.LlmTurn _] -> pure ()
            _ -> assertFailure "Expected [UserTurn, LlmTurn]"
    ]

-------------------------------------------------------------------------------
-- Drop Tail Tests
-------------------------------------------------------------------------------

dropTailTests :: TestTree
dropTailTests = testGroup "sessionEditDropTail"
    [ testCase "dropTail 0 returns same session" $ do
        let session = mkTestSession 10
        let result = SessionEdit.sessionEditDropTail 0 session
        length result.turns @?= 10

    , testCase "dropTail N from large session removes last N" $ do
        let session = mkTestSession 10
        let result = SessionEdit.sessionEditDropTail 3 session
        length result.turns @?= 7

    , testCase "dropTail N >= length returns empty session" $ do
        let session = mkTestSession 5
        let result = SessionEdit.sessionEditDropTail 5 session
        length result.turns @?= 0

    , testCase "dropTail N > length returns empty session" $ do
        let session = mkTestSession 5
        let result = SessionEdit.sessionEditDropTail 10 session
        length result.turns @?= 0

    , testCase "dropTail negative returns same session" $ do
        let session = mkTestSession 5
        let result = SessionEdit.sessionEditDropTail (-1) session
        length result.turns @?= 5

    , testCase "dropTail removes last N turns" $ do
        let session = mkTestSession 4  -- User, LLM, User, LLM
        let result = SessionEdit.sessionEditDropTail 2 session
        length result.turns @?= 2
        -- After dropping last 2, we have: User (index 0), LLM (index 1)
        case result.turns of
            [Session.UserTurn _, Session.LlmTurn _] -> pure ()
            _ -> assertFailure "Expected [UserTurn, LlmTurn]"
    ]

-------------------------------------------------------------------------------
-- Censor Tool Calls Tests
-------------------------------------------------------------------------------

censorToolCallsTests :: TestTree
censorToolCallsTests = testGroup "sessionEditCensorToolCalls"
    [ testCase "removes llmToolCalls from LlmTurn" $ do
        let session = mkSessionWithToolCalls
        let result = SessionEdit.sessionEditCensorToolCalls session
        case result.turns of
            (_ : Session.LlmTurn ltc : _) ->
                length ltc.llmToolCalls @?= 0
            _ -> assertFailure "Expected LlmTurn with empty llmToolCalls"

    , testCase "removes userToolResponses from UserTurn" $ do
        let session = mkSessionWithToolCalls
        let result = SessionEdit.sessionEditCensorToolCalls session
        case result.turns of
            [_ , _, Session.UserTurn utc] ->
                length utc.userToolResponses @?= 0
            _ -> assertFailure "Expected UserTurn with empty userToolResponses"

    , testCase "preserves other turn content" $ do
        let session = mkSessionWithToolCalls
        let result = SessionEdit.sessionEditCensorToolCalls session
        -- Check first UserTurn still has query
        case result.turns of
            (Session.UserTurn utc : _) -> do
                Session.userQuery utc @?= Just (Session.UserQuery "Hello")
            _ -> assertFailure "Expected UserTurn with preserved query"

    , testCase "handles empty session" $ do
        let session = (mkTestSession 0) { Session.turns = [] }
        let result = SessionEdit.sessionEditCensorToolCalls session
        length result.turns @?= 0
    ]

-------------------------------------------------------------------------------
-- Censor Thinking Tests
-------------------------------------------------------------------------------

censorThinkingTests :: TestTree
censorThinkingTests = testGroup "sessionEditCensorThinking"
    [ testCase "removes responseThinking from LlmTurn" $ do
        let session = mkSessionWithToolCalls
        let result = SessionEdit.sessionEditCensorThinking session
        case result.turns of
            (_ : Session.LlmTurn ltc : _) ->
                Session.responseThinking (Session.llmResponse ltc) @?= Nothing
            _ -> assertFailure "Expected LlmTurn with no thinking"

    , testCase "preserves responseText" $ do
        let session = mkSessionWithToolCalls
        let result = SessionEdit.sessionEditCensorThinking session
        case result.turns of
            (_ : Session.LlmTurn ltc : _) ->
                Session.responseText (Session.llmResponse ltc) @?= Just "I'll help"
            _ -> assertFailure "Expected preserved response text"

    , testCase "preserves UserTurn unchanged" $ do
        let session = mkSessionWithToolCalls
        let result = SessionEdit.sessionEditCensorThinking session
        case result.turns of
            (Session.UserTurn utc : _) ->
                Session.userQuery utc @?= Just (Session.UserQuery "Hello")
            _ -> assertFailure "Expected preserved UserTurn"

    , testCase "handles session without thinking" $ do
        let session = mkTestSession 4
        let result = SessionEdit.sessionEditCensorThinking session
        -- All turns should still be present
        length result.turns @?= 4
    ]

-------------------------------------------------------------------------------
-- Composition Tests
-------------------------------------------------------------------------------

compositionTests :: TestTree
compositionTests = testGroup "applySessionEdits and composition"
    [ testCase "empty edit list returns unchanged session" $ do
        let session = mkTestSession 10
        let result = SessionEdit.applySessionEdits [] session
        length result.turns @?= 10

    , testCase "single edit in list works" $ do
        let session = mkTestSession 10
        let result = SessionEdit.applySessionEdits [SessionEdit.sessionEditTake 5] session
        length result.turns @?= 5

    , testCase "multiple edits applied left to right" $ do
        let session = mkTestSession 10
        -- Take 8, then take 5 from result -> 5 turns
        let result = SessionEdit.applySessionEdits
                [ SessionEdit.sessionEditTake 8
                , SessionEdit.sessionEditTake 5
                ] session
        length result.turns @?= 5

    , testCase "composition with drop and take" $ do
        let session = mkTestSession 10
        -- Drop 3, then take 5 -> turns 4-8 (5 turns)
        let result = SessionEdit.applySessionEdits
                [ SessionEdit.sessionEditDrop 3
                , SessionEdit.sessionEditTake 5
                ] session
        length result.turns @?= 5

    , testCase "takeTail and dropTail can extract middle" $ do
        let session = mkTestSession 10
        -- Drop 2 from start, drop 2 from end -> middle 6 turns
        let result = SessionEdit.applySessionEdits
                [ SessionEdit.sessionEditDrop 2
                , SessionEdit.sessionEditDropTail 2
                ] session
        length result.turns @?= 6

    , testCase "censor operations compose with take" $ do
        let session = mkSessionWithToolCalls
        let result = SessionEdit.applySessionEdits
                [ SessionEdit.sessionEditTake 2
                , SessionEdit.sessionEditCensorToolCalls
                ] session
        length result.turns @?= 2
        -- Verify tool calls censored
        case result.turns of
            (_ : Session.LlmTurn ltc : _) ->
                length ltc.llmToolCalls @?= 0
            _ -> pure ()  -- First turn is UserTurn
    ]

-------------------------------------------------------------------------------
-- Edge Case Tests
-------------------------------------------------------------------------------

edgeCaseTests :: TestTree
edgeCaseTests = testGroup "edge cases"
    [ testCase "all operations on empty session" $ do
        let session = (mkTestSession 0) { Session.turns = [] }
        let takeResult = SessionEdit.sessionEditTake 5 session
        let takeTailResult = SessionEdit.sessionEditTakeTail 5 session
        let dropResult = SessionEdit.sessionEditDrop 5 session
        let dropTailResult = SessionEdit.sessionEditDropTail 5 session
        let censorToolResult = SessionEdit.sessionEditCensorToolCalls session
        let censorThinkResult = SessionEdit.sessionEditCensorThinking session
        
        all (\s -> length s.turns == 0)
            [takeResult, takeTailResult, dropResult, dropTailResult, censorToolResult, censorThinkResult]
            @?= True

    , testCase "large N values handled gracefully" $ do
        let session = mkTestSession 3
        let takeResult = SessionEdit.sessionEditTake 1000000 session
        let takeTailResult = SessionEdit.sessionEditTakeTail 1000000 session
        length takeResult.turns @?= 3
        length takeTailResult.turns @?= 3

    , testCase "identity composition" $ do
        let session = mkTestSession 5
        -- Drop 0, Take 100, TakeTail 100 -> should be original
        let result = SessionEdit.applySessionEdits
                [ SessionEdit.sessionEditDrop 0
                , SessionEdit.sessionEditTake 100
                , SessionEdit.sessionEditTakeTail 100
                ] session
        length result.turns @?= 5
    ]

