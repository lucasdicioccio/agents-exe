{- | Provides a runtime value capable to load and reload a mcp list of tools.
Note that reloads are asynchronous.
-}
module System.Agents.Tools.McpToolbox (
    -- * Types
    ToolDescription (..),
    Trace (..),
    Toolbox (..),

    -- * Initialization
    initializeMcpToolbox,

    -- * Synchronization
    waitForInitialDiscovery,
    waitForInitialDiscoveryTimeout,
) where

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMChan (newTBMChanIO, readTBMChan, writeTBMChan)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, writeTVar)
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Network.JSONRPC as Rpc
import Prod.Tracer (Tracer (..), contramap, traceBoth)
import System.Process (CreateProcess)
import System.Timeout (timeout)

import qualified System.Agents.MCP.Base as Mcp
import System.Agents.MCP.Client (LoopProps (..), LoopTrace (..))
import qualified System.Agents.MCP.Client as McpClient
import qualified System.Agents.MCP.Client.Runtime as McpClient

-------------------------------------------------------------------------------
newtype ToolDescription = ToolDescription {getToolDescription :: Mcp.Tool}
    deriving (Show)

data Trace
    = McpClientRunTrace !McpClient.RunTrace
    | McpClientClientTrace !McpClient.ClientTrace
    | McpClientLoopTrace !McpClient.LoopTrace
    deriving (Show)

data Toolbox = Toolbox
    { name :: Text
    , job :: Async ()
    , toolsList :: TVar [ToolDescription]
    , callTool :: ToolDescription -> Maybe Aeson.Object -> IO (McpClient.ToolCallResponse)
    , initialDiscoveryDone :: MVar ()
    {- ^ MVar that is filled when the initial tool discovery completes.
    Use 'waitForInitialDiscovery' to wait for this event.
    -}
    }

{- | Wait for the initial tool discovery to complete.
This blocks until the first 'ToolsRefreshed' event is received from the MCP server.
If the discovery has already completed, this returns immediately.
-}
waitForInitialDiscovery :: Toolbox -> IO ()
waitForInitialDiscovery = readMVar . initialDiscoveryDone

{- | Wait for the initial tool discovery to complete with a timeout.
Returns 'True' if discovery completed, 'False' if the timeout was reached.
-}
waitForInitialDiscoveryTimeout :: Int -> Toolbox -> IO Bool
waitForInitialDiscoveryTimeout microsecs toolbox = do
    result <- timeout microsecs (readMVar $ initialDiscoveryDone toolbox)
    pure $ case result of
        Just () -> True
        Nothing -> False

initializeMcpToolbox ::
    Tracer IO Trace ->
    Text ->
    CreateProcess ->
    IO Toolbox
initializeMcpToolbox ttracer tname proc = do
    -- tool calls
    chan <- newTBMChanIO 30
    let nextToolCall = atomically $ readTBMChan chan
    let doCallTool :: ToolDescription -> Maybe Aeson.Object -> IO McpClient.ToolCallResponse
        doCallTool td param = do
            let tc = McpClient.ToolCall td.getToolDescription.name param
            mbox <- newEmptyMVar
            let done res = do
                    putMVar mbox res
            let fullcall = McpClient.FullToolCall tc done
            atomically $ writeTBMChan chan fullcall
            takeMVar mbox

    -- tool discovery synchronization
    discoveredTools <- newTVarIO []
    initialDiscoveryMVar <- newEmptyMVar
    let toolsListTracer = storeToolsInDiscoveredValues discoveredTools initialDiscoveryMVar

    -- tracers
    let rtTracer = contramap McpClientRunTrace ttracer
    let clientTracer = contramap McpClientClientTrace ttracer
    let loopTracer = traceBoth toolsListTracer (contramap McpClientLoopTrace ttracer)

    -- wire things together
    mcpRt <- McpClient.initRuntime rtTracer proc
    let props = LoopProps loopTracer nextToolCall
    ajob <- async (McpClient.runClient clientTracer mcpRt (McpClient.defaultLoop props))
    pure $ Toolbox tname ajob discoveredTools doCallTool initialDiscoveryMVar

{- | Tracer that stores discovered tools in the TVar and signals completion
via the MVar on the first 'ToolsRefreshed' event.
-}
storeToolsInDiscoveredValues ::
    TVar [ToolDescription] ->
    MVar () ->
    Tracer IO LoopTrace
storeToolsInDiscoveredValues list discoveryMVar = Tracer f
  where
    f :: LoopTrace -> IO ()
    f (StartToolCall _ _) = pure ()
    f (EndToolCall _ _ _) = pure ()
    f (ExitingToolCallLoop) =
        atomically (writeTVar list [])
    f (ToolsRefreshed mitems) = do
        let items = extractToolDescriptions mitems
        atomically (writeTVar list items)
        -- Signal that initial discovery is done (idempotent - only first call succeeds)
        void $ tryTakeMVar discoveryMVar >> putMVar discoveryMVar ()

    extractToolDescriptions ::
        [Maybe (Either Rpc.ErrorObj McpClient.ListToolsResultRsp)] ->
        [ToolDescription]
    extractToolDescriptions mitems =
        let g (Just (Right rsp)) =
                fmap ToolDescription $
                    Mcp.tools (McpClient.getListToolsResult rsp)
            g _ = []
         in mconcat $ map g mitems
