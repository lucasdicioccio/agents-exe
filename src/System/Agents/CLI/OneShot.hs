{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'run' command handler (one-shot mode).

The run command executes a single prompt against an agent and outputs
the response. This is the non-interactive mode for agents-exe.
-}
module System.Agents.CLI.OneShot (
    -- * Types
    OneShotOptions (..),

    -- * Handler
    handleOneShot,
) where

import Control.Monad (forM_)
import Data.Map (Map)
import Data.Text (Text)

import qualified Prod.Tracer as Prod
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import qualified System.Agents.OneShot as OneShot
import qualified System.Agents.SessionStore as SessionStore

import System.Agents.CLI.Aliases (AliasDefinition)
import System.Agents.CLI.PromptScript (PromptScript, interpretPromptScript)

-- | Options for the one-shot command
data OneShotOptions = OneShotOptions
    { sessionFile :: Maybe FilePath
    , thinkingOutput :: OneShot.ThinkingOutput
    , promptScript :: PromptScript
    }
    deriving (Show)

-- | Handle the one-shot run command
handleOneShot ::
    -- | Base tracer for logging
    Prod.Tracer IO AgentTree.Trace ->
    -- | Session store for persistence
    SessionStore.SessionStore ->
    -- | Path to API keys file
    FilePath ->
    -- | List of agent files (only first is used)
    [FilePath] ->
    -- | Available prompt aliases
    Map Text AliasDefinition ->
    -- | One-shot options
    OneShotOptions ->
    IO ()
handleOneShot baseTracer sessionStore apiKeysFile agentFiles aliases opts = do
    apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile
    forM_ (take 1 agentFiles) $ \agentFile -> do
        promptContents <- interpretPromptScript aliases opts.promptScript opts.sessionFile
        mSession <- maybe (pure Nothing) SessionStore.readSessionFromFile opts.sessionFile
        registry <- AgentTree.newRuntimeRegistry
        let oneShot text props = OneShot.mainOneShotTextWithThinking sessionStore opts.sessionFile mSession opts.thinkingOutput props text
        oneShot promptContents $
            AgentTree.Props
                { AgentTree.apiKeys = apiKeys
                , AgentTree.rootAgentFile = agentFile
                , AgentTree.interactiveTracer = baseTracer
                , -- Wrap the function to match the new signature (ignoring the tracer argument)
                  -- The turnAgentRuntimeIntoIOTool function handles the tracer internally
                  AgentTree.agentToTool = \_tracer rt slug aid -> OneShotTool.turnAgentRuntimeIntoIOTool sessionStore rt slug aid
                , AgentTree.runtimeRegistry = registry
                }
