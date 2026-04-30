{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Signal detection engine for trajectory analysis.

This module implements lightweight, rule-based signal detection based on the paper:
"Signals: Trajectory Sampling and Triage for Agentic Interactions" (Chen et al., 2026)

Signals are descriptive markers of recurring behavioral patterns, organized into
three categories:

1. Interaction Signals (Learning-Oriented): Derived from user-assistant discourse
   - Misalignment: Semantic/intent mismatches
   - Stagnation: Discourse without progress
   - Disengagement: Withdrawal of cooperative intent
   - Satisfaction: Successful convergence

2. Execution Signals (Learning-Oriented): Derived from runtime events
   - Failure: Actions not yielding usable outcomes
   - Loop: Repetitive execution without progress

3. Environment Signals (Diagnosis-Oriented): System-level conditions
   - Exhaustion: Resource boundary hits

Detection is performed via lightweight heuristics without requiring LLM calls.
-}
module System.Agents.Session.Signals (
    -- * Detection functions
    detectInteractionSignals,
    detectExecutionSignals,
    detectEnvironmentSignals,
    calculateTrajectorySignals,
    calculateSessionSignals,

    -- * Per-turn detection
    detectTurnInteractionSignals,
    detectTurnExecutionSignals,
    detectTurnEnvironmentSignals,

    -- * Informativeness scoring
    calculateInformativenessScore,

    -- * Heuristics (exported for testing)
    misalignmentHeuristics,
    stagnationHeuristics,
    disengagementHeuristics,
    satisfactionHeuristics,
    loopDetectionHeuristics,
    exhaustionHeuristics,

    -- * Utility functions
    extractToolCallName,
    extractToolCallArguments,
    extractResponseText,
) where

import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import System.Agents.Media.Types (ContentPart (..), MediaAttachment (..))
import System.Agents.Session.Types (
    EnvironmentSignals (..),
    ExecutionSignals (..),
    InteractionSignals (..),
    LlmResponse (..),
    LlmToolCall (..),
    LlmTurnContent (..),
    Session (..),
    TrajectorySignals (..),
    Turn (..),
    UserQuery (..),
    UserToolResponse (..),
    UserTurnContent (..),
    defaultExecutionSignals,
    defaultInteractionSignals,
 )

-------------------------------------------------------------------------------
-- Top-Level Detection Functions
-------------------------------------------------------------------------------

{- | Calculate trajectory signals for an entire session.

Aggregates signals from all turns and computes the informativeness score.
This is the main entry point for signal analysis.
-}
calculateTrajectorySignals :: Session -> TrajectorySignals
calculateTrajectorySignals session =
    let turnList = session.turns
        interaction = detectInteractionSignals turnList
        execution = detectExecutionSignals turnList
        environment = detectEnvironmentSignals turnList
        score = calculateInformativenessScore interaction execution environment
     in TrajectorySignals
            { trajInteraction = interaction
            , trajExecution = execution
            , trajEnvironment = environment
            , trajInformativenessScore = score
            }

-- | Alias for calculateTrajectorySignals for backward compatibility naming.
calculateSessionSignals :: Session -> TrajectorySignals
calculateSessionSignals = calculateTrajectorySignals

-------------------------------------------------------------------------------
-- Interaction Signal Detection
-------------------------------------------------------------------------------

{- | Detect interaction signals from user-assistant discourse.

Analyzes all turns to detect patterns of:
- Misalignment (rephrasing, corrections, clarifications)
- Stagnation (near-duplicate responses, circular explanations)
- Disengagement (exit requests, negative stance, abandonment)
- Satisfaction (gratitude, success confirmations)
-}
detectInteractionSignals :: [Turn] -> InteractionSignals
detectInteractionSignals turnList =
    let userTexts = extractUserTexts turnList
        llmTexts = extractLlmTexts turnList

        -- Misalignment: Check for correction phrases in user queries
        misalignmentCount = sum $ map countMisalignmentPatterns userTexts

        -- Stagnation: Detect near-duplicate responses within recent window
        stagnationCount = detectStagnationPatterns llmTexts

        -- Disengagement: Check for exit/negative markers
        disengagement = any hasDisengagementMarkers userTexts

        -- Satisfaction: Check for success/gratitude markers
        satisfaction = any hasSatisfactionMarkers userTexts
     in InteractionSignals
            { sigMisalignmentCount = misalignmentCount
            , sigStagnationCount = stagnationCount
            , sigDisengagementDetected = disengagement
            , sigSatisfactionDetected = satisfaction
            }

{- | Detect interaction signals for a single turn.

Used for per-turn signal display in the TUI.
-}
detectTurnInteractionSignals :: Turn -> InteractionSignals
detectTurnInteractionSignals turn =
    case turn of
        UserTurn utc _ ->
            let userText = fromMaybe "" (userQuery utc >>= Just . queryText)
                misalignment = countMisalignmentPatterns userText
                disengagement = hasDisengagementMarkers userText
                satisfaction = hasSatisfactionMarkers userText
             in defaultInteractionSignals
                    { sigMisalignmentCount = if misalignment > 0 then 1 else 0
                    , sigDisengagementDetected = disengagement
                    , sigSatisfactionDetected = satisfaction
                    }
        PartialUserTurn _ _ ->
            -- Partial user turns are treated like user turns for interaction signals
            -- They represent incomplete user input that can still show intent patterns
            defaultInteractionSignals
        LlmTurn _ _ ->
            -- Stagnation requires cross-turn comparison; individual turns
            -- don't show stagnation on their own
            defaultInteractionSignals{sigStagnationCount = 0}

-- | Phrases indicating misalignment (user corrections/clarifications)
misalignmentHeuristics :: [Text]
misalignmentHeuristics =
    [ "rephrase"
    , "correction"
    , "clarify"
    , "i meant"
    , "not what i asked"
    , "that's not right"
    , "you misunderstood"
    , "wrong"
    , "incorrect"
    , "fix"
    , "change"
    , "update"
    , "revise"
    , "modify"
    , "adjust"
    , "rewrite"
    , "redo"
    , "start over"
    , "try again"
    , "different approach"
    , "another way"
    , "not quite"
    , "close but"
    , "almost"
    , "better"
    , "instead"
    , "rather"
    , "prefer"
    , "want"
    , "need"
    , "should be"
    , "must be"
    , "has to be"
    , "supposed to"
    ]

-- | Count misalignment patterns in text.
countMisalignmentPatterns :: Text -> Int
countMisalignmentPatterns text =
    let lowerText = Text.toLower text
        matches = filter (\p -> p `Text.isInfixOf` lowerText) misalignmentHeuristics
     in length matches

-- | Phrases indicating disengagement (user withdrawal)
disengagementHeuristics :: [Text]
disengagementHeuristics =
    [ "talk to human"
    , "talk to a human"
    , "speak to human"
    , "real person"
    , "human agent"
    , "customer service"
    , "support team"
    , "stop"
    , "cancel"
    , "quit"
    , "exit"
    , "done"
    , "enough"
    , "this isn't working"
    , "not working"
    , "doesn't work"
    , "won't work"
    , "can't help"
    , "useless"
    , "waste of time"
    , "giving up"
    , "forget it"
    , "never mind"
    , "goodbye"
    , "bye"
    , "end"
    , "terminate"
    , "abort"
    , "this is frustrating"
    , "frustrated"
    , "annoying"
    , "terrible"
    , "awful"
    , "worst"
    , "hate"
    , "disappointed"
    , "unsatisfied"
    ]

-- | Check if text contains disengagement markers.
hasDisengagementMarkers :: Text -> Bool
hasDisengagementMarkers text =
    let lowerText = Text.toLower text
     in any (\p -> p `Text.isInfixOf` lowerText) disengagementHeuristics

-- | Phrases indicating satisfaction (successful convergence)
satisfactionHeuristics :: [Text]
satisfactionHeuristics =
    [ "thank you"
    , "thanks"
    , "appreciate"
    , "grateful"
    , "that worked"
    , "it worked"
    , "works now"
    , "working"
    , "perfect"
    , "excellent"
    , "great"
    , "awesome"
    , "amazing"
    , "fantastic"
    , "wonderful"
    , "brilliant"
    , "outstanding"
    , "superb"
    , "exactly"
    , "precisely"
    , "that's it"
    , "that's right"
    , "correct"
    , "solved"
    , "fixed"
    , "resolved"
    , "done"
    , "complete"
    , "finished"
    , "mission accomplished"
    , "goal achieved"
    , "success"
    , "successful"
    , "beautiful"
    , "love it"
    , "like it"
    , "good job"
    , "well done"
    , "nice work"
    , "impressive"
    ]

-- | Check if text contains satisfaction markers.
hasSatisfactionMarkers :: Text -> Bool
hasSatisfactionMarkers text =
    let lowerText = Text.toLower text
     in any (\p -> p `Text.isInfixOf` lowerText) satisfactionHeuristics

-- | Phrases indicating stagnation (repetitive content markers)
stagnationHeuristics :: [Text]
stagnationHeuristics =
    [ "as i said"
    , "as mentioned"
    , "as explained"
    , "again"
    , "repeating"
    , "same"
    , "similar"
    , "identical"
    , "just like"
    , "same as"
    , "no change"
    , "unchanged"
    , "still"
    , "yet again"
    , "once more"
    , "previously"
    , "before"
    , "earlier"
    , "last time"
    , "in the previous"
    ]

{- | Detect stagnation patterns in LLM responses.

Looks for:
1. Near-duplicate responses within a sliding window (3 turns)
2. Explicit stagnation markers in text
3. Repetitive phrase usage
-}
detectStagnationPatterns :: [Text] -> Int
detectStagnationPatterns texts =
    let
        -- Check for near-duplicate detection within window of 3
        duplicates = countNearDuplicates 3 texts
        -- Count explicit stagnation markers
        markers = sum $ map countStagnationMarkers texts
     in
        duplicates + markers

-- | Count stagnation markers in a single text.
countStagnationMarkers :: Text -> Int
countStagnationMarkers text =
    let lowerText = Text.toLower text
        matches = filter (\p -> p `Text.isInfixOf` lowerText) stagnationHeuristics
     in length matches

{- | Count near-duplicate responses within a sliding window.

Considers two texts near-duplicates if they share >80% of words
or one is a substring of the other.
-}
countNearDuplicates :: Int -> [Text] -> Int
countNearDuplicates windowSize texts =
    let windows = takeWindowed windowSize texts
        dupPairs = filter hasNearDuplicate windows
     in length dupPairs
  where
    takeWindowed n xs = [take n (drop i xs) | i <- [0 .. length xs - n]]

    hasNearDuplicate :: [Text] -> Bool
    hasNearDuplicate [] = False
    hasNearDuplicate (_ : []) = False
    hasNearDuplicate (x : xs) = any (isNearDuplicate x) xs

    isNearDuplicate :: Text -> Text -> Bool
    isNearDuplicate t1 t2 =
        let w1 = Text.words (Text.toLower t1)
            w2 = Text.words (Text.toLower t2)
            -- Check substring relationship
            isSubstring = t1 `Text.isInfixOf` t2 || t2 `Text.isInfixOf` t1
            -- Check word overlap > 80%
            common = length $ filter (\w -> w `elem` w2) w1
            total = max (length w1) (length w2)
            highOverlap = total > 0 && (common * 100) `div` total >= 80
         in isSubstring || (highOverlap && total > 5) -- Only count if meaningful length

-------------------------------------------------------------------------------
-- Execution Signal Detection
-------------------------------------------------------------------------------

{- | Detect execution signals from tool call patterns.

Analyzes tool call sequences to detect:
- Failure: Empty results, errors, no-op responses
- Loop: Repeated tool calls with identical/similar arguments
-}
detectExecutionSignals :: [Turn] -> ExecutionSignals
detectExecutionSignals turnList =
    let
        -- Collect all tool calls in order
        allToolCalls = concatMap extractTurnToolCalls turnList

        -- Detect failures from tool responses
        failureCount = countFailuresInTurnList turnList

        -- Detect loops in tool call sequences
        loopInfo = detectExecutionLoop allToolCalls
        (loopDetected, loopTools) = case loopInfo of
            Just (tools, _) -> (True, tools)
            Nothing -> (False, [])
     in
        ExecutionSignals
            { sigFailureCount = failureCount
            , sigLoopDetected = loopDetected
            , sigLoopToolSequence = loopTools
            }

{- | Detect execution signals for a single turn.

Used for per-turn signal display in the TUI.
-}
detectTurnExecutionSignals :: Turn -> ExecutionSignals
detectTurnExecutionSignals turn =
    case turn of
        UserTurn utc _ ->
            let
                -- Count failures in tool responses
                failures = sum $ map (countFailureInResponse . snd) utc.userToolResponses
             in
                defaultExecutionSignals{sigFailureCount = failures}
        PartialUserTurn _ _ ->
            -- Partial user turns don't have tool responses yet
            defaultExecutionSignals
        LlmTurn _ _ ->
            -- Loop detection requires cross-turn analysis
            defaultExecutionSignals

-- | Extract tool calls from a turn.
extractTurnToolCalls :: Turn -> [LlmToolCall]
extractTurnToolCalls (UserTurn _ _) = []
extractTurnToolCalls (PartialUserTurn _ _) = []
extractTurnToolCalls (LlmTurn ltc _) = ltc.llmToolCalls

-- | Count failures across all turns.
countFailuresInTurnList :: [Turn] -> Int
countFailuresInTurnList turnList = sum $ map countFailuresInTurn turnList
  where
    countFailuresInTurn :: Turn -> Int
    countFailuresInTurn (LlmTurn _ _) = 0
    countFailuresInTurn (PartialUserTurn _ _) = 0
    countFailuresInTurn (UserTurn utc _) =
        sum $ map (countFailureInResponse . snd) utc.userToolResponses

-- | Heuristics for detecting failure in a tool response.
countFailureInResponse :: UserToolResponse -> Int
countFailureInResponse (TextResponse txt) =
    if isFailureText txt then 1 else 0
countFailureInResponse (JsonResponse val) =
    if isFailureJson val then 1 else 0
countFailureInResponse (MediaResponse _) = 0 -- Media responses are typically not failures
countFailureInResponse (MixedResponse parts) =
    -- Check text parts for failure indicators
    let textParts = [t | TextPart t <- parts]
     in length $ filter isFailureText textParts

-- | Check if text response indicates failure.
isFailureText :: Text -> Bool
isFailureText txt =
    let lower = Text.toLower txt
        failureIndicators =
            [ "error"
            , "failed"
            , "failure"
            , "exception"
            , "timeout"
            , "unavailable"
            , "not found"
            , "does not exist"
            , "invalid"
            , "unauthorized"
            , "forbidden"
            , "permission denied"
            , "no such"
            , "unable to"
            , "could not"
            , "can't"
            , "cannot"
            , "empty"
            , "null"
            , "undefined"
            , "no results"
            , "nothing found"
            , "404"
            , "500"
            , "503"
            ]
     in any (\i -> i `Text.isInfixOf` lower) failureIndicators

-- | Check if JSON response indicates failure.
isFailureJson :: Aeson.Value -> Bool
isFailureJson val = case val of
    Aeson.Object obj ->
        -- Check for common error fields
        case KeyMap.lookup "error" obj of
            Just _ -> True
            Nothing ->
                case KeyMap.lookup "success" obj of
                    Just (Aeson.Bool False) -> True
                    _ ->
                        case KeyMap.lookup "status" obj of
                            Just (Aeson.String s) ->
                                Text.toLower s `elem` ["error", "failed", "failure"]
                            _ -> False
    Aeson.Array arr -> null arr -- Empty array might indicate no results
    Aeson.Null -> True
    _ -> False

{- | Detect loops in tool call sequences.

Pattern rules:
- Repeated calls with identical inputs (exact loop)
- Repeated calls with systematically varying inputs (parameter drift)
- Repeated multi-tool cycles (oscillation)

Returns: Just (tool names, loop count) if loop detected, Nothing otherwise
-}
detectExecutionLoop :: [LlmToolCall] -> Maybe ([Text], Int)
detectExecutionLoop toolCalls
    | length toolCalls < 3 = Nothing
    | otherwise =
        let
            -- Extract tool names
            toolNames = map extractToolCallName toolCalls

            -- Check for exact repetition of same tool
            exactLoop = findExactLoop toolNames

            -- Check for oscillation patterns (A->B->A)
            oscillation = findOscillation toolNames

            -- Check for repeated sequences of 2+ tools
            sequenceLoop = findSequenceLoop toolNames
         in
            exactLoop <|> oscillation <|> sequenceLoop

-- | Find exact loops (same tool called repeatedly).
findExactLoop :: [Text] -> Maybe ([Text], Int)
findExactLoop [] = Nothing
findExactLoop names =
    let grouped = groupConsecutive names
        -- Look for groups of 3+ of same tool
        loops = filter (\(_, c) -> c >= 3) grouped
     in case loops of
            ((name, count) : _) -> Just ([name], count)
            [] -> Nothing
  where
    groupConsecutive :: [Text] -> [(Text, Int)]
    groupConsecutive [] = []
    groupConsecutive (x : xs) = go x 1 xs
      where
        go name count [] = [(name, count)]
        go name count (y : ys)
            | y == name = go name (count + 1) ys
            | otherwise = (name, count) : go y 1 ys

-- | Find oscillation patterns (A->B->A->B).
findOscillation :: [Text] -> Maybe ([Text], Int)
findOscillation names
    | length names < 4 = Nothing
    | otherwise =
        let
            -- Look for A-B-A-B pattern
            isOscillation [a, b, c, d] = a == c && b == d && a /= b
            isOscillation _ = False

            -- Sliding window of 4
            windows = takeWindowed 4 names
            oscillations = filter isOscillation windows
         in
            case oscillations of
                (win : _) -> Just (nub win, length oscillations)
                [] -> Nothing
  where
    takeWindowed n xs = filter (\w -> length w == n) [take n (drop i xs) | i <- [0 .. length xs - n]]

-- | Find repeated sequences of 2+ tools.
findSequenceLoop :: [Text] -> Maybe ([Text], Int)
findSequenceLoop names
    | length names < 6 = Nothing
    | otherwise =
        let
            -- Try sequence lengths from 2 to half the list
            maxLen = length names `div` 2
            candidates = concatMap (\n -> findRepeats n names) [2 .. maxLen]
         in
            case candidates of
                ((seqTools, count) : _) -> Just (seqTools, count)
                [] -> Nothing
  where
    findRepeats :: Int -> [Text] -> [([Text], Int)]
    findRepeats n xs =
        let seqs = [take n (drop i xs) | i <- [0, n .. length xs - n]]
            -- Check if all sequences are equal
            firstSeq = take n xs
            allSame = all (== firstSeq) seqs
         in if length seqs >= 2 && allSame && n > 0
                then [(firstSeq, length seqs)]
                else []

-- | Loop detection heuristics exported for documentation/testing.
loopDetectionHeuristics :: Text
loopDetectionHeuristics = Text.intercalate "\n"
    [ "Loop detection identifies:"
    , "- Exact loops: Same tool called 3+ times in sequence"
    , "- Oscillation: A->B->A->B pattern between two tools"
    , "- Sequence loops: Repeated cycles of 2+ tools"
    ]

-------------------------------------------------------------------------------
-- Environment Signal Detection
-------------------------------------------------------------------------------

{- | Detect environment signals from system-level events.

Analyzes turns for:
- Exhaustion: Rate limits, context overflows, API failures
-}
detectEnvironmentSignals :: [Turn] -> EnvironmentSignals
detectEnvironmentSignals turnList =
    let
        -- Count exhaustion events across all responses
        exhaustionTypes = concatMap detectExhaustionInTurn turnList
        uniqueTypes = nub exhaustionTypes
     in
        EnvironmentSignals
            { sigExhaustionCount = length exhaustionTypes
            , sigExhaustionTypes = uniqueTypes
            }

{- | Detect environment signals for a single turn.

Used for per-turn signal display in the TUI.
-}
detectTurnEnvironmentSignals :: Turn -> EnvironmentSignals
detectTurnEnvironmentSignals turn =
    let types = detectExhaustionInTurn turn
     in EnvironmentSignals
            { sigExhaustionCount = length types
            , sigExhaustionTypes = types
            }

-- | Detect exhaustion markers in a turn.
detectExhaustionInTurn :: Turn -> [Text]
detectExhaustionInTurn (LlmTurn ltc _) =
    -- Check LLM response for exhaustion markers
    let respText = fromMaybe "" (responseText ltc.llmResponse)
     in detectExhaustionInText respText
detectExhaustionInTurn (PartialUserTurn _ _) =
    -- Partial user turns don't have responses to check
    []
detectExhaustionInTurn (UserTurn utc _) =
    -- Check tool responses for exhaustion markers
    concatMap (detectExhaustionInResponse . snd) utc.userToolResponses

-- | Detect exhaustion markers in a response.
detectExhaustionInResponse :: UserToolResponse -> [Text]
detectExhaustionInResponse (TextResponse txt) = detectExhaustionInText txt
detectExhaustionInResponse (JsonResponse val) = detectExhaustionInJson val
detectExhaustionInResponse _ = []

-- | Patterns indicating resource exhaustion.
exhaustionHeuristics :: [(Text, Text)]
exhaustionHeuristics =
    [ ("rate limit", "rate_limit")
    , ("rate_limit", "rate_limit")
    , ("too many requests", "rate_limit")
    , ("context limit", "context_overflow")
    , ("context_overflow", "context_overflow")
    , ("context length", "context_overflow")
    , ("token limit", "context_overflow")
    , ("maximum context", "context_overflow")
    , ("quota exceeded", "quota_exceeded")
    , ("insufficient quota", "quota_exceeded")
    , ("billing limit", "quota_exceeded")
    , ("timeout", "timeout")
    , ("timed out", "timeout")
    , ("deadline exceeded", "timeout")
    , ("connection refused", "connection_error")
    , ("connection reset", "connection_error")
    , ("network error", "connection_error")
    , ("unavailable", "service_unavailable")
    , ("service unavailable", "service_unavailable")
    , ("503", "service_unavailable")
    , ("internal error", "internal_error")
    , ("500", "internal_error")
    ]

-- | Detect exhaustion markers in text.
detectExhaustionInText :: Text -> [Text]
detectExhaustionInText txt =
    let lower = Text.toLower txt
        matches = filter (\(pattern, _) -> pattern `Text.isInfixOf` lower) exhaustionHeuristics
     in map snd matches

-- | Detect exhaustion markers in JSON.
detectExhaustionInJson :: Aeson.Value -> [Text]
detectExhaustionInJson val =
    -- Convert to text and search
    let txt = Text.pack $ show val
     in detectExhaustionInText txt

-------------------------------------------------------------------------------
-- Informativeness Score Calculation
-------------------------------------------------------------------------------

{- | Calculate the composite informativeness score for a trajectory.

Score ranges from 0-100, where higher scores indicate trajectories more
likely to contain actionable insights for learning and improvement.

Scoring formula (based on the paper's approach):
- Base score: 30
- Failure points: +10 per failure (max 30)
- Misalignment points: +5 per misalignment (max 20)
- Loop bonus: +20 if loop detected
- Disengagement bonus: +15 if disengagement detected
- Satisfaction penalty: -10 if satisfaction detected
-}
calculateInformativenessScore :: InteractionSignals -> ExecutionSignals -> EnvironmentSignals -> Int
calculateInformativenessScore is es env =
    let
        -- Base score
        baseScore = 30

        -- Positive signals (indicate actionable insights)
        failurePoints = min 30 (sigFailureCount es * 10)
        misalignPoints = min 20 (sigMisalignmentCount is * 5)
        loopPoints = if sigLoopDetected es then 20 else 0
        disengagePoints = if sigDisengagementDetected is then 15 else 0
        stagnationPoints = min 10 (sigStagnationCount is * 3)
        exhaustionPoints = min 10 (sigExhaustionCount env * 5)

        -- Satisfaction reduces informativeness (success case)
        satisfactionPenalty = if sigSatisfactionDetected is then -10 else 0

        -- Calculate total
        rawScore =
            baseScore
                + failurePoints
                + misalignPoints
                + loopPoints
                + disengagePoints
                + stagnationPoints
                + exhaustionPoints
                + satisfactionPenalty
     in
        max 0 $ min 100 rawScore

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Extract user query texts from turns.
extractUserTexts :: [Turn] -> [Text]
extractUserTexts turnList = catMaybes $ map extractUserText turnList
  where
    extractUserText (UserTurn utc _) = userQuery utc >>= Just . queryText
    extractUserText (PartialUserTurn _ _) = Nothing
    extractUserText (LlmTurn _ _) = Nothing

-- | Extract LLM response texts from turns.
extractLlmTexts :: [Turn] -> [Text]
extractLlmTexts turnList = mapMaybe extractLlmText turnList
  where
    extractLlmText (LlmTurn ltc _) = responseText ltc.llmResponse
    extractLlmText (UserTurn _ _) = Nothing
    extractLlmText (PartialUserTurn _ _) = Nothing

-- | Extract tool call name from LlmToolCall.
extractToolCallName :: LlmToolCall -> Text
extractToolCallName (LlmToolCall val) =
    case val of
        Aeson.Object obj ->
            case KeyMap.lookup "function" obj of
                Just (Aeson.Object funcObj) ->
                    case KeyMap.lookup "name" funcObj of
                        Just (Aeson.String toolName) -> toolName
                        _ -> "(unnamed)"
                _ -> case KeyMap.lookup "name" obj of
                    Just (Aeson.String toolName) -> toolName
                    _ -> "(unnamed)"
        _ -> "(unnamed)"

-- | Extract tool call arguments from LlmToolCall.
extractToolCallArguments :: LlmToolCall -> Maybe Aeson.Value
extractToolCallArguments (LlmToolCall val) =
    case val of
        Aeson.Object obj ->
            case KeyMap.lookup "function" obj of
                Just (Aeson.Object funcObj) ->
                    case KeyMap.lookup "arguments" funcObj of
                        Just args -> Just args
                        Nothing -> Nothing
                _ -> Nothing
        _ -> Nothing

-- | Extract response text from various response formats.
extractResponseText :: UserToolResponse -> Text
extractResponseText (TextResponse txt) = txt
extractResponseText (JsonResponse val) = Text.pack $ show val
extractResponseText (MediaResponse media) = "[Media: " <> media.mediaMimeType <> "]"
extractResponseText (MixedResponse parts) =
    Text.unlines $ map extractPart parts
  where
    extractPart (TextPart t) = t
    extractPart (MediaPart m) = "[Media: " <> m.mediaMimeType <> "]"

