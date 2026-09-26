{-# LANGUAGE OverloadedStrings #-}

{- | MCP progress notifications reach the tool's progress callback.

Runs @test/data/mcp-progress-server.py@ (needs @python3@; the server tests
pass trivially without it) through the real MCP toolbox.
-}
module McpProgressTests (tests) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Prod.Tracer (Tracer (..))
import System.Directory (findExecutable)
import System.Process (proc)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

import qualified System.Agents.MCP.Base as Mcp
import qualified System.Agents.MCP.Client as McpClient
import qualified System.Agents.Tools.McpToolbox as Mcp

tests :: TestTree
tests =
    testGroup
        "MCP progress notifications"
        [ testCase "progressPayload keeps progress, and total and message when present" $ do
            let n = Mcp.ProgressNotification (Mcp.TextProgressToken "t") 1.5 (Just 3) (Just "half")
            McpClient.progressPayload n
                @?= Aeson.object ["progress" Aeson..= (1.5 :: Double), "total" Aeson..= (3 :: Double), "message" Aeson..= ("half" :: String)]
            McpClient.progressPayload (Mcp.ProgressNotification (Mcp.TextProgressToken "t") 1 Nothing Nothing)
                @?= Aeson.object ["progress" Aeson..= (1 :: Double)]
        , testCase "ProgressNotification parses string and numeric tokens" $ do
            fmap Mcp.progressToken (Aeson.decode "{\"progressToken\":\"abc\",\"progress\":0.5}")
                @?= Just (Mcp.TextProgressToken "abc")
            fmap Mcp.progressToken (Aeson.decode "{\"progressToken\":7,\"progress\":1,\"total\":2}")
                @?= Just (Mcp.NumberProgresstoken 7)
        , testCase "tools/call carries the progress token in _meta" $ do
            let msg = McpClient.CallToolRequestMsg (Mcp.CallToolRequest "slow" Nothing) (Just (Mcp.TextProgressToken "tok"))
            case Aeson.toJSON msg of
                Aeson.Object o ->
                    KeyMap.lookup "_meta" o @?= Just (Aeson.object ["progressToken" Aeson..= ("tok" :: String)])
                other -> assertFailure ("expected an object: " <> show other)
        , serverTest "polling server" []
        , serverTest "server announcing listChanged" ["listChanged"]
        ]

serverTest :: String -> [String] -> TestTree
serverTest label extraArgs = testCase ("a call's progress reaches its callback: " <> label) $ do
    mpy <- findExecutable "python3"
    case mpy of
        Nothing -> pure ()
        Just py -> do
            toolbox <- Mcp.initializeMcpToolbox (Tracer (\_ -> pure ())) "progress" (proc py ("test/data/mcp-progress-server.py" : extraArgs)) Nothing
            found <- Mcp.waitForInitialDiscoveryTimeout 15000000 toolbox
            assertBool "tool discovery finished" found
            tools <- readTVarIO (Mcp.toolsList toolbox)
            desc <- case tools of
                (d : _) -> pure d
                [] -> assertFailure "no tool discovered"
            seen <- newTVarIO []
            let onProgress v = atomically (modifyTVar' seen (v :))
            _ <- Mcp.callTool toolbox desc Nothing (Just onProgress)
            -- notifications are read concurrently with the response: wait for both
            r <- timeout 5000000 $ atomically $ do
                xs <- readTVar seen
                if length xs >= 2 then pure xs else retry
            case r of
                Nothing -> assertFailure "progress notifications did not arrive"
                Just xs ->
                    reverse xs
                        @?= [ Aeson.object ["progress" Aeson..= (1 :: Double), "total" Aeson..= (2 :: Double), "message" Aeson..= ("half" :: String)]
                            , Aeson.object ["progress" Aeson..= (2 :: Double), "total" Aeson..= (2 :: Double)]
                            ]
            -- without a callback no token is advertised
            res <- Mcp.callTool toolbox desc Nothing Nothing
            case res of
                Just (Right _) -> pure ()
                _ -> assertFailure "call without progress failed"
            Mcp.stopMcpToolbox toolbox
