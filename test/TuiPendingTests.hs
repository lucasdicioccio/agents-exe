{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pure tests for the TUI's Pending panel helpers (Phase 3c,
@todos/os-as-standalone-server.md@ Design §4): 'pendingSummaryLine'. None
of this touches a runner or the wire -- it is a plain view over
'DeferredCallView's already delivered by @calls.deferred@.
-}
module TuiPendingTests (tests) where

import qualified Data.Aeson as Aeson
import Data.UUID (nil)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Session.Base (
    DeferredCallView (..),
    LlmToolCall (..),
    Reason (..),
    ToolCallDisposition (Defer),
    ToolCallId (..),
 )
import System.Agents.TUI.Types (pendingSummaryLine)

mkCall :: Aeson.Value -> DeferredCallView
mkCall name =
    DeferredCallView
        { dcvToolCallId = ToolCallId nil
        , dcvToken = Nothing
        , dcvToolName = case name of
            Aeson.String t -> t
            _ -> "tool"
        , dcvDisposition = Defer (Reason "approval")
        , dcvCall = LlmToolCall (Aeson.object [])
        }

tests :: TestTree
tests =
    testGroup
        "TUI pending panel (Phase 3c)"
        [ testCase "pendingSummaryLine of no calls is empty" $
            pendingSummaryLine [] @?= ""
        , testCase "pendingSummaryLine of one call" $
            pendingSummaryLine [mkCall "bash_command"] @?= "1 pending call: bash_command"
        , testCase "pendingSummaryLine of several calls lists every tool name" $
            pendingSummaryLine [mkCall "bash_command", mkCall "search"]
                @?= "2 pending calls: bash_command, search"
        ]
