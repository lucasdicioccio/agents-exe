-- need to capture
-- \* identifying info about the agent
-- \* identifying info about the conversation
-- \* some history of the conversation
-- \* a thread that is either waiting on input or waiting on an llm
-- \* some resumable information
-- \* some notion of party

-- | organizes multi-agents discussions into parties
module System.Agents.Party where

import Control.Concurrent.Async
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, takeTMVar, tryPutTMVar)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Text (Text)
import qualified Prod.Tracer as Prod

import System.Agents.Agent (AgentFunctions (..), handleConversation)
import qualified System.Agents.Agent as Agent
import System.Agents.Base (ConversationId, newConversationId)

data ConversationState
    = ConversationState
    { conversationId :: ConversationId
    , task :: Async () -- add some final status
    , prompt :: Maybe Text -> STM Bool
    , traces :: IO [Agent.Trace]
    }

converse :: Agent.Runtime -> Text -> IO ConversationState
converse baseRuntime txt = do
    cId <- newConversationId
    inbox <- newEmptyTMVarIO
    tracesIORef <- newIORef []
    let logTracesInIORef = Prod.Tracer $ \t -> modifyIORef tracesIORef (t :)
    let adaptedRuntime = Agent.addTracer baseRuntime logTracesInIORef
    a <- async $ handleConversation adaptedRuntime (agentFunctions inbox) cId txt
    pure $
        ConversationState
            { task = a
            , conversationId = cId
            , prompt = tryPutTMVar inbox
            , traces = readIORef tracesIORef
            }
  where
    nextQuery :: TMVar (Maybe Text) -> IO (Maybe Text)
    nextQuery inbox =
        atomically $ takeTMVar inbox

    agentFunctions :: TMVar (Maybe Text) -> AgentFunctions ()
    agentFunctions inbox =
        AgentFunctions
            (nextQuery inbox)
            (print)
            (print)
