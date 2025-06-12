{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.CLI where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

import System.Agents.Agent
import System.Agents.Base (newConversationId)
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.Runtime as Runtime
import qualified System.Agents.Tools as Tools
import qualified System.Agents.Tools.Bash as Tools
import qualified System.Agents.Tools.IO as Tools

mainInteractiveAgent :: Props -> IO ()
mainInteractiveAgent props = do
    withAgentRuntime props $ \x -> do
        case x of
            LoadingErrors errs -> traverse_ print errs
            OtherErrors errs -> traverse_ print errs
            Initialized ai ->
                runMainAgent ai
  where
    agentFunctions ask =
        Runtime.AgentFunctions
            (fmap queryOrNothing ask)
            (\_hist -> pure ())
            (\err -> putStrLn $ unlines ["parse error", err])
            (\_ -> putStrLn "done")

    runMainAgent :: AgentInfo -> IO ()
    runMainAgent ai = do
        let nextQuery = askQuery ai
        query <- nextQuery
        cId <- newConversationId
        Runtime.handleConversation ai.agentRuntime (agentFunctions nextQuery) cId query

    askQuery :: AgentInfo -> IO Text
    askQuery ai = do
        go
      where
        helpStr = do
            unlines
                [ "? or ?help -- show this help"
                , "?desc-main -- dump loaded main agent"
                , "?desc-agents -- dump loaded helper agents' descriptions"
                , "?desc-tools -- dump loaded tools"
                , "?reload-tools -- reload tools"
                ]
        go = do
            putStrLn "### Enter query:"
            query <- Text.pack <$> getLine
            case query of
                "?help" -> do
                    putStrLn helpStr
                    go
                "?desc-main" -> do
                    print ai.agentDescription
                    go
                "?desc-agents" -> do
                    traverse_ print ai.agentSibling.agents
                    go
                "?desc-tools" -> do
                    traverse_ printAgentTools (ai.agentRuntime : ai.agentSiblingRuntimes)
                    go
                "?reload-tools" -> do
                    traverse_ Runtime.triggerRefreshTools (ai.agentRuntime : ai.agentSiblingRuntimes)

                    go
                txt
                    | "?" `Text.isPrefixOf` txt -> do
                        putStrLn helpStr
                        go
                    | otherwise -> pure txt

    printAgentTools :: Runtime.Runtime -> IO ()
    printAgentTools rt = do
        registry <- rt.agentTools
        Text.putStrLn (renderToolRegistry registry)

    renderToolRegistry :: (Aeson.ToJSON b) => [Tools.Registration a b c] -> Text
    renderToolRegistry registry =
        Text.unlines $
            fmap renderRegisteredTool registry

    renderRegisteredTool :: (Aeson.ToJSON b) => Tools.Registration a b c -> Text
    renderRegisteredTool reg =
        case reg.innerTool.toolDef of
            Tools.BashTool bashScript ->
                Text.unwords ["command", Text.pack bashScript.scriptPath, Text.decodeUtf8 $ LByteString.toStrict $ Aeson.encode reg.declareTool]
            Tools.IOTool ioScript ->
                Text.unwords ["io", ioScript.ioSlug, ioScript.ioDescription]

    queryOrNothing :: Text -> Maybe Text
    queryOrNothing "" = Nothing
    queryOrNothing t = Just t
