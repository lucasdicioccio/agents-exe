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
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, takeTMVar, tryPutTMVar)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Text (Text)
import qualified Prod.Tracer as Prod

import System.Agents.Agent (AgentFunctions (..), handleConversation)
import qualified System.Agents.Agent as Agent
import System.Agents.Base (ConversationId, newConversationId)
import System.Agents.LLMs.OpenAI as LLMs

data ConversationStatus
    = WaitingForPrompt
    | Executing
    | Final
    deriving (Show, Eq, Ord)

data ConversationState
    = ConversationState
    { conversationId :: ConversationId
    , task :: Async (Either String LLMs.History)
    , prompt :: Maybe Text -> STM Bool
    , status :: IO ConversationStatus
    , traces :: IO [Agent.Trace]
    }

converse :: Agent.Runtime -> Text -> IO ConversationState
converse baseRuntime txt = do
    cId <- newConversationId
    inbox <- newEmptyTMVarIO
    tracesIORef <- newIORef []
    statusTVar <- newTVarIO Executing
    let logTracesInIORef = Prod.Tracer $ \t -> modifyIORef tracesIORef (t :)
    let adaptedRuntime = Agent.addTracer baseRuntime logTracesInIORef
    a <- async $ handleConversation adaptedRuntime (agentFunctions inbox statusTVar) cId txt
    pure $
        ConversationState
            { task = a
            , conversationId = cId
            , prompt = tryPutTMVar inbox
            , status = readTVarIO statusTVar
            , traces = readIORef tracesIORef
            }
  where
    agentFunctions ::
        TMVar (Maybe Text) ->
        TVar ConversationStatus ->
        AgentFunctions (Either String LLMs.History)
    agentFunctions inbox statusTVar =
        AgentFunctions
            (nextQuery inbox statusTVar)
            (claimProgress statusTVar)
            (endWithError statusTVar)
            (endWithSuccess statusTVar)

    nextQuery ::
        TMVar (Maybe Text) ->
        TVar ConversationStatus ->
        IO (Maybe Text)
    nextQuery inbox statusTVar = do
        -- claim we need a prompt
        atomically $ do
            writeTVar statusTVar WaitingForPrompt
        -- then wait on the prompt and claim we are executing
        atomically $ do
            writeTVar statusTVar Executing
            takeTMVar inbox

    claimProgress ::
        TVar ConversationStatus ->
        LLMs.History ->
        IO ()
    claimProgress statusTVar _ = do
        atomically $ do
            writeTVar statusTVar WaitingForPrompt

    endWithSuccess ::
        TVar ConversationStatus ->
        LLMs.History ->
        IO (Either String LLMs.History)
    endWithSuccess statusTVar hist = do
        atomically $ writeTVar statusTVar Final
        pure $ Right hist

    endWithError ::
        TVar ConversationStatus ->
        String ->
        IO (Either String LLMs.History)
    endWithError statusTVar err = do
        atomically $ writeTVar statusTVar Final
        pure $ Left err
