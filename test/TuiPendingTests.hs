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
import qualified Data.UUID as UUID
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Session.Base (
    DeferredCallView (..),
    LlmToolCall (..),
    Reason (..),
    ToolCallDisposition (Defer),
    ContinuationToken (..),
    ToolCallId (..),
 )
import System.Agents.TUI.Types (failedCallText, hookFailedStatusText, nextPendingToken, pendingSummaryLine, selectedPendingCall)

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

-- | A deferred call carrying a continuation token (or none).
withToken :: Maybe Int -> Aeson.Value -> DeferredCallView
withToken n name = (mkCall name){dcvToken = tok <$> n}
  where
    tok i = ContinuationToken (UUID.fromWords 0 0 0 (fromIntegral i))

token :: Int -> Maybe ContinuationToken
token i = dcvToken (withToken (Just i) "x")

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
        , testCase "with no selection, the first completable call is the selected one" $
            fmap dcvToken (selectedPendingCall Nothing [withToken Nothing "a", withToken (Just 1) "b", withToken (Just 2) "c"])
                @?= Just (token 1)
        , testCase "a selected token picks its call" $
            fmap dcvToken (selectedPendingCall (token 2) [withToken (Just 1) "b", withToken (Just 2) "c"])
                @?= Just (token 2)
        , testCase "a vanished selection falls back to the first completable call" $
            fmap dcvToken (selectedPendingCall (token 9) [withToken (Just 1) "b", withToken (Just 2) "c"])
                @?= Just (token 1)
        , testCase "no completable call selects nothing" $
            selectedPendingCall Nothing [withToken Nothing "a"] @?= Nothing
        , testCase "nextPendingToken cycles through the completable calls and wraps" $ do
            let calls = [withToken (Just 1) "a", withToken Nothing "skip", withToken (Just 2) "b", withToken (Just 3) "c"]
            nextPendingToken Nothing calls @?= token 2
            nextPendingToken (token 1) calls @?= token 2
            nextPendingToken (token 2) calls @?= token 3
            nextPendingToken (token 3) calls @?= token 1
        , testCase "nextPendingToken with one call stays on it, with none is empty" $ do
            nextPendingToken Nothing [withToken (Just 1) "a"] @?= token 1
            nextPendingToken Nothing [] @?= Nothing
        , testCase "failedCallText carries the reason, or a default" $ do
            failedCallText "  no way  " @?= "Error: no way"
            failedCallText "" @?= "Error: the user declined this call"
            failedCallText "  \n " @?= "Error: the user declined this call"
        , testCase "hookFailedStatusText names the failure and trims" $
            hookFailedStatusText " before hook failed, denying: boom\n" @?= "Hook failed: before hook failed, denying: boom"
        ]
