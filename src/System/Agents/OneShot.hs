{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.OneShot where

import Data.Foldable (traverse_)
import Data.Text (Text)

import System.Agents.Agent
import System.Agents.Base (newConversationId)
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.Runtime as Runtime

mainPrintAgent :: Props -> IO ()
mainPrintAgent props = do
    withAgentRuntime props $ \x -> do
        case x of
            LoadingErrors errs -> traverse_ print errs
            OtherErrors errs -> traverse_ print errs
            Initialized _ -> pure ()

mainOneShotText :: Props -> Text -> IO ()
mainOneShotText props query = do
    withAgentRuntime props $ \x -> do
        case x of
            LoadingErrors errs -> traverse_ print errs
            OtherErrors errs -> traverse_ print errs
            Initialized ai -> runMainAgent ai.agentRuntime
  where
    agentFunctions =
        Runtime.AgentFunctions
            (pure Nothing)
            (\_hist -> pure ())
            (\err -> putStrLn $ unlines ["execution error", err])
            (\hist -> OpenAI.printLastAnswer hist)
    runMainAgent rt = do
        cId <- newConversationId
        Runtime.handleConversation rt agentFunctions cId query
