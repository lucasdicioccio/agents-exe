{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Module for editing session files via composable transformations.
--
-- This module provides functions to transform 'Session' values, similar to
-- Unix head/tail utilities. These functions can be composed together to
-- create complex session transformations.
--
-- All functions have type @Session -> Session@ for easy composition:
--
-- > sessionEditTake 10 . sessionEditDrop 5 $ session  -- Take turns 6-15
--
-- The command line interface maps arguments to these transformation functions,
-- reading JSON from STDIN and outputting JSON to STDOUT.
--
-- Example usage:
--
-- > $ cat session.json | agents-exe session-edit take 10
-- > $ cat session.json | agents-exe session-edit take-tail 5
-- > $ cat session.json | agents-exe session-edit censor thinking
--
module System.Agents.Session.Edit
    ( -- * Session transformation functions
      sessionEditTake
    , sessionEditTakeTail
    , sessionEditDrop
    , sessionEditDropTail
    , sessionEditCensorToolCalls
    , sessionEditCensorThinking
      -- * Composition helpers
    , applySessionEdits
      -- * Re-export for convenience
    , Session.Session
    ) where

import qualified System.Agents.Session.Types as Session

-- | Take the first N turns from a session.
--
-- If N is negative, returns an empty session.
-- If N is larger than the number of turns, returns the original session.
--
-- >>> sessionEditTake 3 session
sessionEditTake :: Int -> Session.Session -> Session.Session
sessionEditTake n session
    | n <= 0 = session { Session.turns = [] }
    | otherwise = session { Session.turns = take n session.turns }

-- | Take the last N turns from a session (like Unix @tail -n N@).
--
-- If N is negative, returns an empty session.
-- If N is larger than the number of turns, returns the original session.
--
-- >>> sessionEditTakeTail 3 session
sessionEditTakeTail :: Int -> Session.Session -> Session.Session
sessionEditTakeTail n session
    | n <= 0 = session { Session.turns = [] }
    | otherwise = session { Session.turns = takeLast n session.turns }
  where
    takeLast :: Int -> [a] -> [a]
    takeLast k xs = drop (max 0 (length xs - k)) xs

-- | Drop the first N turns from a session.
--
-- If N is negative, returns the original session.
-- If N is larger than the number of turns, returns an empty session.
--
-- >>> sessionEditDrop 2 session
sessionEditDrop :: Int -> Session.Session -> Session.Session
sessionEditDrop n session
    | n <= 0 = session
    | otherwise = session { Session.turns = drop n session.turns }

-- | Drop the last N turns from a session.
--
-- If N is negative, returns the original session.
-- If N is larger than the number of turns, returns an empty session.
--
-- >>> sessionEditDropTail 2 session
sessionEditDropTail :: Int -> Session.Session -> Session.Session
sessionEditDropTail n session
    | n <= 0 = session
    | otherwise = session { Session.turns = dropLast n session.turns }
  where
    dropLast :: Int -> [a] -> [a]
    dropLast k xs = take (max 0 (length xs - k)) xs

-- | Censor (remove) all tool calls from a session.
--
-- This removes:
-- * Tool calls from LLM turns ('llmToolCalls')
-- * Tool responses from user turns ('userToolResponses')
--
-- The tool call structures are replaced with an empty list, preserving
-- the overall session structure.
--
-- >>> sessionEditCensorToolCalls session
sessionEditCensorToolCalls :: Session.Session -> Session.Session
sessionEditCensorToolCalls session =
    session { Session.turns = map censorTurn session.turns }
  where
    censorTurn :: Session.Turn -> Session.Turn
    censorTurn (Session.UserTurn utc) =
        Session.UserTurn $ utc { Session.userToolResponses = [] }
    censorTurn (Session.LlmTurn ltc) =
        Session.LlmTurn $ ltc { Session.llmToolCalls = [] }

-- | Censor (remove) all thinking content from a session.
--
-- This removes the 'responseThinking' field from all LLM responses,
-- replacing it with 'Nothing'.
--
-- This is useful for:
-- * Reducing session size when thinking content is not needed
-- * Privacy/security when sharing sessions
-- * Cleaner output for display purposes
--
-- >>> sessionEditCensorThinking session
sessionEditCensorThinking :: Session.Session -> Session.Session
sessionEditCensorThinking session =
    session { Session.turns = map censorTurn session.turns }
  where
    censorTurn :: Session.Turn -> Session.Turn
    censorTurn (Session.UserTurn utc) = Session.UserTurn utc
    censorTurn (Session.LlmTurn ltc) =
        Session.LlmTurn $ ltc { Session.llmResponse = censorResponse ltc.llmResponse }

    censorResponse :: Session.LlmResponse -> Session.LlmResponse
    censorResponse resp = resp { Session.responseThinking = Nothing }

-- | Apply a list of session edits in sequence (left to right).
--
-- The edits are applied in the order they appear in the list:
--
-- > applySessionEdits [sessionEditDrop 5, sessionEditTake 10] session
-- > -- Equivalent to: sessionEditTake 10 . sessionEditDrop 5 $ session
-- > -- Results in turns 6-15 from the original session
--
applySessionEdits :: [Session.Session -> Session.Session] -> Session.Session -> Session.Session
applySessionEdits edits session = foldl (\s f -> f s) session edits

