{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.OneShot where

import Data.Foldable (traverse_)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text.IO as Text

import System.Agents.AgentTree
import System.Agents.Base (newConversationId)
import qualified System.Agents.LLMs.OpenAI as LLM
import qualified System.Agents.Runtime as Runtime

mainPrintAgent :: Props -> IO ()
mainPrintAgent props = do
    withAgentTreeRuntime props $ \x -> do
        case x of
            Errors errs -> traverse_ print errs
            Initialized _ -> pure ()

mainOneShotText :: Props -> Text -> IO ()
mainOneShotText props query = do
    withAgentTreeRuntime props $ \x -> do
        case x of
            Errors errs -> traverse_ print errs
            Initialized ai -> runMainAgent ai.agentRuntime
  where
    conversationFunctions =
        Runtime.ConversationFunctions
            (pure Nothing)
            (\_hist -> pure ())
            (\err -> putStrLn $ unlines ["execution error", err])
            (\hist -> printLastAnswer hist)
    runMainAgent rt = do
        cId <- newConversationId
        Runtime.handleConversation rt conversationFunctions cId query

printLastAnswer :: LLM.History -> IO ()
printLastAnswer hist = do
    let msg = LLM.rspContent =<< LLM.lastAnswerMaybe hist
    Text.putStrLn $ Maybe.fromMaybe "finished but with no response" msg
