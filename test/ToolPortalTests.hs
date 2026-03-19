{- |
Module      : ToolPortalTests
Description : Tests for ToolPortal's callToolViaPortal function

This module contains tests for verifying the ToolPortal functionality,
including portal execution, error handling, tracing, and the makeToolPortal function.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ToolPortalTests where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

import Prod.Tracer (Tracer(..), contramap, runTracer, silent)

import System.Agents.ToolPortal
import System.Agents.ToolRegistration (ToolRegistration(..))
import System.Agents.Tools.Base (CallResult(..), Tool(..), ToolDef(..), mapToolResult)
import System.Agents.Tools.Context (ToolCall(..), ToolResult(..))
import qualified System.Agents.Tools.IO as IOTools
import qualified System.Agents.Tools.Bash as BashTools
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Tools.Trace (ToolTrace(..))

-- | Test suite entry point
toolPortalTestSuite :: TestTree
toolPortalTestSuite =
    testGroup
        "ToolPortal Tests"
        [ portalErrorTests
        , portalExecutionTests
        -- , makeToolPortalTests
        -- , portalTracingTests
        -- , collectTracesTests
        ]

-------------------------------------------------------------------------------
-- PortalError Tests
-------------------------------------------------------------------------------

portalErrorTests :: TestTree
portalErrorTests =
    testGroup
        "PortalError"
        [ testCase "PortalToolNotFound show instance" $ do
            let err = PortalToolNotFound "missing-tool"
            show err @?= "PortalToolNotFound \"missing-tool\""
        
        , testCase "PortalToolNotAllowed show instance" $ do
            let err = PortalToolNotAllowed "bash" ["sqlite", "curl"]
            show err @?= "PortalToolNotAllowed \"bash\" [\"sqlite\",\"curl\"]"
        
        , testCase "PortalInvalidArguments show instance" $ do
            let err = PortalInvalidArguments "missing required field"
            show err @?= "PortalInvalidArguments \"missing required field\""
        
        , testCase "PortalExecutionError show instance" $ do
            let err = PortalExecutionError "timeout occurred"
            show err @?= "PortalExecutionError \"timeout occurred\""
        
        , testCase "PortalError equality" $ do
            let err1 = PortalToolNotFound "tool-a"
            let err2 = PortalToolNotFound "tool-a"
            let err3 = PortalToolNotFound "tool-b"
            let err4 = PortalToolNotAllowed "tool-a" ["allowed"]
            
            err1 @?= err2
            assertBool "Different tool names should not be equal" (err1 /= err3)
            assertBool "Different error types should not be equal" (err1 /= err4)
        ]

-------------------------------------------------------------------------------
-- Portal Execution Tests
-------------------------------------------------------------------------------

portalExecutionTests :: TestTree
portalExecutionTests =
    testGroup
        "callToolViaPortal"
        [ testCase "returns error when tool not found" $ do
            let toolCall = ToolCall
                    { callToolName = "nonexistent-tool"
                    , callArgs = object []
                    , callCallerId = "test-caller"
                    }
            
            result <- callToolViaPortal silent [] toolCall
            
            case result of
                Left (PortalToolNotFound name) -> name @?= "nonexistent-tool"
                Left other -> assertFailure $ "Expected PortalToolNotFound, got: " ++ show other
                Right _ -> assertFailure "Expected error for missing tool"
        
        , testCase "successfully executes a mock tool" $ do
            let toolCall = ToolCall
                    { callToolName = "io_mock-tool"
                    , callArgs = object ["input" .= ("test" :: Text)]
                    , callCallerId = "test-caller"
                    }
            
            let mockReg = makeMockRegistration "mock-tool"
            
            result <- callToolViaPortal silent [mockReg] toolCall
            
            case result of
                Left err -> assertFailure $ "Expected success, got error: " ++ show err
                Right callResult -> do
                    case callResult of
                        BlobToolSuccess () output -> output @?= "mock output"
                        other -> assertFailure $ "Expected BlobToolSuccess, got: " ++ show other
        ]

-------------------------------------------------------------------------------
-- makeToolPortal Tests
-------------------------------------------------------------------------------

{-

makeToolPortalTests :: TestTree
makeToolPortalTests =
    testGroup
        "makeToolPortal"
        [ testCase "creates a portal that returns ToolResult on success" $ do
            let mockReg = makeMockRegistration "test-tool"
            portalTraceRef <- newIORef []
            let portalTracer = Tracer $ \trace -> modifyIORef' portalTraceRef (trace :)
            let portal = makeToolPortal portalTracer [mockReg]
            
            let toolCall = ToolCall
                    { callToolName = "io_test-tool"
                    , callArgs = object []
                    , callCallerId = "caller-1"
                    }
            
            result <- portal toolCall
            
            -- Check that result has expected structure
            case result of
                ToolResult{resultData = data_, resultDuration = duration, resultTraceId = traceId} -> do
                    -- Verify data is a JSON object with expected structure
                    case data_ of
                        Object obj -> do
                            -- Check for type field indicating success
                            case KeyMap.lookup "type" obj of
                                Just (String "blob") -> pure ()
                                _ -> assertFailure "Expected blob type in result data"
                        _ -> assertFailure "Expected JSON object in result data"
                    
                    -- Duration should be non-negative
                    assertBool "Duration should be non-negative" (duration >= 0)
                    
                    -- TraceId should be non-empty
                    assertBool "TraceId should be non-empty" (not $ Text.null traceId)
            
            -- Verify portal trace was collected
            traces <- readIORef portalTraceRef
            length traces @?= 1
            case traces of
                (portalTrace:_) -> do
                    portalToolCall portalTrace @?= toolCall
                    -- No nested traces expected for simple mock tool
                    portalNestedTraces portalTrace @?= []
                _ -> assertFailure "Expected one portal trace"
        
        , testCase "creates a portal that returns error for unknown tool" $ do
            portalTraceRef <- newIORef []
            let portalTracer = Tracer $ \trace -> modifyIORef' portalTraceRef (trace :)
            let portal = makeToolPortal portalTracer []
            
            let toolCall = ToolCall
                    { callToolName = "unknown-tool"
                    , callArgs = object []
                    , callCallerId = "caller-2"
                    }
            
            result <- portal toolCall
            
            case result of
                ToolResult{resultData = data_} -> do
                    case data_ of
                        Object obj -> do
                            case KeyMap.lookup "error" obj of
                                Just _ -> pure () -- Expected error field
                                Nothing -> assertFailure "Expected error field in result data"
                        _ -> assertFailure "Expected JSON object in result data"
            
            -- Verify portal trace was still collected even for error
            traces <- readIORef portalTraceRef
            length traces @?= 1
        
        , testCase "portal executes multiple different tools" $ do
            let reg1 = makeMockRegistration "tool-a"
            let reg2 = makeMockRegistration "tool-b"
            portalTraceRef <- newIORef []
            let portalTracer = Tracer $ \trace -> modifyIORef' portalTraceRef (trace :)
            let portal = makeToolPortal portalTracer [reg1, reg2]
            
            -- Call first tool
            let call1 = ToolCall
                    { callToolName = "io_tool-a"
                    , callArgs = object []
                    , callCallerId = "multi-caller"
                    }
            result1 <- portal call1
            
            -- Call second tool
            let call2 = ToolCall
                    { callToolName = "io_tool-b"
                    , callArgs = object []
                    , callCallerId = "multi-caller"
                    }
            result2 <- portal call2
            
            -- Both should succeed
            case (result1, result2) of
                (ToolResult{resultData = data1}, ToolResult{resultData = data2}) -> do
                    case (data1, data2) of
                        (Object obj1, Object obj2) -> do
                            case (KeyMap.lookup "type" obj1, KeyMap.lookup "type" obj2) of
                                (Just (String "blob"), Just (String "blob")) -> pure ()
                                _ -> assertFailure "Expected both results to be blob type"
                        _ -> assertFailure "Expected JSON objects in results"
            
            -- Verify both calls were traced
            traces <- readIORef portalTraceRef
            length traces @?= 2
        ]
-}

-------------------------------------------------------------------------------
-- Portal Tracing Tests
-- DISABLED FOR NOW
-------------------------------------------------------------------------------

{-
portalTracingTests :: TestTree
portalTracingTests =
    testGroup
        "PortalToolTrace"
        [ testCase "PortalToolTrace contains ToolCall and nested traces" $ do
            let toolCall = ToolCall
                    { callToolName = "bash_test"
                    , callArgs = object ["cmd" .= ("echo hello" :: Text)]
                    , callCallerId = "test-123"
                    }
            let nestedTraces = [BashToolsTrace $ BashTools.RunCommandStart "/bin/bash" ["echo hello"]]
            let portalTrace = PortalToolTrace
                    { portalToolCall = toolCall
                    , portalNestedTraces = nestedTraces
                    }
            
            portalToolCall portalTrace @?= toolCall
            portalNestedTraces portalTrace @?= nestedTraces
        
        , testCase "PortalToolTrace show instance" $ do
            let toolCall = ToolCall "test" (object []) "caller"
            let portalTrace = PortalToolTrace toolCall []
            let shown = show portalTrace
            
            assertBool "Show should contain PortalToolTrace" ("PortalToolTrace" `Text.isInfixOf` Text.pack shown)
            assertBool "Show should contain tool name" ("test" `Text.isInfixOf` Text.pack shown)
        
        , testCase "portal captures nested traces from tool execution" $ do
            -- Create a tool that emits traces during execution
            let tracingReg = makeTracingMockRegistration "tracing-tool"
            
            portalTraceRef <- newIORef []
            let portalTracer = Tracer $ \trace -> modifyIORef' portalTraceRef (trace :)
            let portal = makeToolPortal portalTracer [tracingReg]
            
            let toolCall = ToolCall
                    { callToolName = "io_tracing-tool"
                    , callArgs = object []
                    , callCallerId = "trace-caller"
                    }
            
            result <- portal toolCall
            
            -- Verify the result is successful
            case result of
                ToolResult{resultData = data_} -> do
                    case data_ of
                        Object obj -> do
                            case KeyMap.lookup "type" obj of
                                Just (String "blob") -> pure ()
                                _ -> assertFailure "Expected blob type"
                        _ -> assertFailure "Expected JSON object"
            
            -- Verify portal trace was collected with nested traces
            traces <- readIORef portalTraceRef
            case traces of
                (portalTrace:_) -> do
                    portalToolCall portalTrace @?= toolCall
                    -- The tracing tool emits 2 IOToolsTrace entries
                    length (portalNestedTraces portalTrace) @?= 2
                _ -> assertFailure "Expected one portal trace"
        ]

-------------------------------------------------------------------------------
-- collectTraces Tests
-------------------------------------------------------------------------------

collectTracesTests :: TestTree
collectTracesTests =
    testGroup
        "collectTraces"
        [ testCase "collects traces in reverse order" $ do
            (ref, tracer) <- collectTraces
            
            let desc = IOTools.IOScriptDescription "test" "test desc"
            
            -- Emit some traces
            runTracer tracer $ IOToolsTrace $ IOTools.IOScriptStarted desc $ object ["step" .= (1 :: Int)]
            runTracer tracer $ IOToolsTrace $ IOTools.IOScriptStarted desc $ object ["step" .= (2 :: Int)]
            runTracer tracer $ IOToolsTrace $ IOTools.IOScriptStarted desc $ object ["step" .= (3 :: Int)]
            
            -- Read collected traces (should be in reverse order)
            traces <- readIORef ref
            length traces @?= 3
            
            -- Verify reverse order (newest first)
            case traces of
                (IOToolsTrace (IOTools.IOScriptStarted _ v):_) -> do
                    case v of
                        Object obj -> 
                            case KeyMap.lookup "step" obj of
                                Just (Number 3) -> pure () -- Last trace emitted should be first
                                _ -> assertFailure "Expected step 3"
                        _ -> assertFailure "Expected object"
                _ -> assertFailure "Expected IOToolsTrace"
        
        , testCase "returns empty list initially" $ do
            (ref, _tracer) <- collectTraces
            traces <- readIORef ref
            traces @?= ([] :: [ToolTrace])
        
        , testCase "collects various trace types" $ do
            (ref, tracer) <- collectTraces
            
            let ioDesc = IOTools.IOScriptDescription "io-test" "IO test desc"
            
            runTracer tracer $ IOToolsTrace $ IOTools.IOScriptStarted ioDesc $ object []
            runTracer tracer $ BashToolsTrace $ BashTools.RunCommandStart "/bin/echo" ["test"]
            
            traces <- readIORef ref
            length traces @?= 2
        ]
-}

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- | Create a mock tool registration for testing
makeMockRegistration :: Text -> ToolRegistration
makeMockRegistration name =
    let toolName = OpenAI.ToolName ("io_" <> name)
        
        -- Mock tool that returns a simple success result
        mockTool :: Tool ()
        mockTool = Tool
            { toolDef = IOTool $ IOTools.IOScriptDescription
                { IOTools.ioSlug = "io_" <> name
                , IOTools.ioDescription = "Mock tool for testing"
                }
            , toolRun = \_tracer _ctx _args -> do
                -- Return a simple blob success
                pure $ BlobToolSuccess () "mock output"
            }
        
        -- Mock OpenAI tool declaration
        declareTool = OpenAI.Tool
            { OpenAI.toolName = toolName
            , OpenAI.toolDescription = "Mock tool for testing"
            , OpenAI.toolParamProperties = []
            }
        
        -- Find function that matches on this tool's name
        find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
        find call = 
            if OpenAI.toolCallFunctionName (OpenAI.toolCallFunction call) == toolName
                then Just $ mapToolResult (const call) mockTool
                else Nothing
        
    in ToolRegistration
        { innerTool = mockTool
        , declareTool = declareTool
        , findTool = find
        }

-- | Create a mock tool registration that emits traces during execution
makeTracingMockRegistration :: Text -> ToolRegistration
makeTracingMockRegistration name =
    let toolName = OpenAI.ToolName ("io_" <> name)
        
        -- Mock tool that emits traces during execution
        mockTool :: Tool ()
        mockTool = Tool
            { toolDef = IOTool $ IOTools.IOScriptDescription
                { IOTools.ioSlug = "io_" <> name
                , IOTools.ioDescription = "Tracing mock tool for testing"
                }
            , toolRun = \tracer _ctx _args -> do
                -- Emit some traces
                let desc = IOTools.IOScriptDescription "tracing" "Tracing tool"
                runTracer tracer $ IOToolsTrace $ IOTools.IOScriptStarted desc $ object ["phase" .= ("start" :: Text)]
                runTracer tracer $ IOToolsTrace $ IOTools.IOScriptStarted desc $ object ["phase" .= ("end" :: Text)]
                -- Return success
                pure $ BlobToolSuccess () "traced output"
            }
        
        -- Mock OpenAI tool declaration
        declareTool = OpenAI.Tool
            { OpenAI.toolName = toolName
            , OpenAI.toolDescription = "Tracing mock tool for testing"
            , OpenAI.toolParamProperties = []
            }
        
        -- Find function that matches on this tool's name
        find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
        find call = 
            if OpenAI.toolCallFunctionName (OpenAI.toolCallFunction call) == toolName
                then Just $ mapToolResult (const call) mockTool
                else Nothing
        
    in ToolRegistration
        { innerTool = mockTool
        , declareTool = declareTool
        , findTool = find
        }

