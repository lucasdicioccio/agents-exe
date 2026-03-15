{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'init' command handler.

The init command creates initial agent configuration files and
directory structure for a new agent project.
-}
module System.Agents.CLI.Initialize (
    handleInitialize,
) where

import Control.Exception (catch)
import Control.Monad (forM_)

import System.Agents.Base (Agent (..))
import qualified System.Agents.CLI.InitProject as InitProject

-- | Handle the initialize command: create agent configuration files
handleInitialize ::
    -- | Path to API keys file
    FilePath ->
    -- | List of agent files (only first is used)
    [FilePath] ->
    IO ()
handleInitialize apiKeysFile agentFiles = do
    let defaultAgent =
            Agent
                { slug = "main-agent"
                , apiKeyId = "main-key"
                , flavor = "OpenAIv1"
                , modelUrl = "https://api.openai.com/v1"
                , modelName = "gpt-4-turbo"
                , announce = "a helpful puppet-master capable of orchestrating other agents ensuring"
                , toolDirectory = Just "tools"
                , bashToolboxes = Nothing
                , systemPrompt =
                    [ "You are a helpful software agent trying to solve user requests"
                    , "Your preferred action mode is to act as a puppet master capable of driving other agents."
                    , "You can prompt other agents via tools by passing them a prompt using a JSON payload."
                    , "You efficiently single-shot prompt other agents to efficiently use your token budget."
                    , "You only provide prompt examples when you think other agents may benefit."
                    , "You notify users as you progress"
                    , "If an agent fails, do not retry and abdicate"
                    ]
                , mcpServers = Just []
                , openApiToolboxes = Nothing
                , postgrestToolboxes = Nothing
                , builtinToolboxes = Just []
                , extraAgents = Nothing
                , skillSources = Nothing
                , autoEnableSkills = Nothing
                }

    forM_ (take 1 agentFiles) $ \agentFile -> do
        catch (InitProject.initAgentFile defaultAgent agentFile >> pure ()) handleInitError
        catch (InitProject.initAgentTooldir defaultAgent agentFile >> pure ()) handleInitError
        catch (InitProject.initKeyFile apiKeysFile >> pure ()) handleInitError
  where
    handleInitError :: InitProject.InitializeError -> IO ()
    handleInitError e = putStrLn $ "Note: " ++ show e

