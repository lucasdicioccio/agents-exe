{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'run' command handler (one-shot mode).

The run command executes a single prompt against an agent and outputs
the response. This is the non-interactive mode for agents-exe.

This module uses OS-native structures for agent management.
-}
module System.Agents.CLI.OneShot (
    Trace (..),

    -- * Types
    OneShotOptions (..),
    OneShotAgent (..),

    -- * Handler
    handleOneShot,

    -- * Agent creation
    createOneShotAgent,

    -- * Tool access
    listOneShotAgentTools,
) where

import Control.Monad (forM_, unless)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.IO as Text.IO

import qualified Prod.Tracer as Prod
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import qualified System.Agents.OneShot as OneShot
import qualified System.Agents.SessionStore as SessionStore

import System.Agents.AgentTree (OSAgentTree (..))
import System.Agents.CLI.Aliases (AliasDefinition)
import System.Agents.CLI.PromptScript (MediaReference (..), PromptScript, interpretPromptScript, resolveMediaType)
import System.Agents.Media.Types (MediaAttachment (..))
import System.Agents.OS.AgentHandle (AgentHandle, createAgentHandle, getAgentId, getAgentTools)
import System.Agents.ToolRegistration (ToolRegistration)
import System.Exit (exitFailure)
import System.IO (stderr)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import System.FilePath (takeFileName)

data Trace
    = AgentTreeTrace !AgentTree.TreeTrace
    | OneShotTrace !OneShot.Trace
    | OneShotToolTrace !OneShotTool.Trace
    deriving (Show)

-- | Options for the one-shot command
data OneShotOptions = OneShotOptions
    { sessionFile :: Maybe FilePath
    , thinkingOutput :: OneShot.ThinkingOutput
    , promptScript :: PromptScript
    , mediaFiles :: [MediaReference]
    }
    deriving (Show)

{- | OneShot agent using OS-native structures.

This structure wraps an 'AgentHandle' for one-shot execution.
-}
data OneShotAgent = OneShotAgent
    { osaHandle :: AgentHandle
    }

-- | Manual Show instance for OneShotAgent.
instance Show OneShotAgent where
    show agent =
        "OneShotAgent {oneShotAgentId = " ++ show (getAgentId agent.osaHandle) ++ "}"

{- | Create a OneShotAgent from an OSAgentTree.

This function extracts the root agent from the tree for one-shot execution.
-}
createOneShotAgent :: OSAgentTree -> OneShotAgent
createOneShotAgent tree =
    OneShotAgent
        { osaHandle = createAgentHandle tree
        }

{- | List tools for a OneShotAgent.

Reads tools directly from the OS-native TVar via the shared 'AgentHandle'.
-}
listOneShotAgentTools :: OneShotAgent -> IO [ToolRegistration]
listOneShotAgentTools = getAgentTools . osaHandle

{- | Load media files from references and create MediaAttachments.
Returns Left with error message if any file cannot be loaded.
-}
loadMediaAttachments :: [MediaReference] -> IO (Either Text [MediaAttachment])
loadMediaAttachments refs = do
    results <- mapM loadMedia refs
    pure $ sequence results
  where
    loadMedia :: MediaReference -> IO (Either Text MediaAttachment)
    loadMedia ref = do
        case resolveMediaType ref of
            Left err -> pure $ Left $ Text.pack err
            Right mimeType -> do
                -- Try to read the file
                fileContent <- ByteString.readFile (mediaFilePath ref)
                let base64Data = Text.Encoding.decodeUtf8 $ Base64.encode fileContent
                let filename = Just $ Text.pack $ takeFileName (mediaFilePath ref)
                pure $ Right $ MediaAttachment mimeType base64Data filename

-- | Handle the one-shot run command
handleOneShot ::
    -- | Base tracer for logging
    Prod.Tracer IO Trace ->
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
handleOneShot tracer sessionStore apiKeysFile agentFiles aliases opts = do
    -- Load and validate media files first
    mediaResult <- loadMediaAttachments opts.mediaFiles
    case mediaResult of
        Left err -> do
            Text.IO.hPutStrLn stderr $ "Error: " <> err
            exitFailure
        Right mediaAttachments -> do
            -- Log media attachments
            unless (null mediaAttachments) $ do
                Text.IO.hPutStrLn stderr $ "Attaching " <> Text.pack (show $ length mediaAttachments) <> " media file(s):"
                mapM_ (\m -> Text.IO.hPutStrLn stderr $ "  - " <> m.mediaMimeType <> ": " <> maybe "unnamed" id m.mediaFilename) mediaAttachments

            apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile
            forM_ (take 1 agentFiles) $ \agentFilePath -> do
                promptContents <- interpretPromptScript aliases opts.promptScript opts.sessionFile
                mSession <- maybe (pure Nothing) SessionStore.readSessionFromFile opts.sessionFile
                -- Use OS-native agent loading (no registry needed)
                let oneShot text props =
                        OneShot.mainOneShotTextWithThinking
                            (Prod.contramap OneShotTrace tracer)
                            sessionStore
                            opts.sessionFile
                            mSession
                            opts.thinkingOutput
                            mediaAttachments -- Pass media to the one-shot handler
                            props
                            text
                oneShot promptContents $
                    AgentTree.Props
                        { AgentTree.apiKeys = apiKeys
                        , AgentTree.apiKeysFile = apiKeysFile
                        , AgentTree.rootAgentFile = agentFilePath
                        , AgentTree.interactiveTracer = Prod.contramap AgentTreeTrace tracer
                        , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool (Prod.contramap OneShotToolTrace tracer) sessionStore apiKeys
                        , AgentTree.sessionStore = sessionStore
                        }

