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
    PromptScriptOptions (..),

    -- * Parsers
    parsePromptScriptOptions,
    parsePromptScriptInput,

    -- * Prompt loading
    loadPromptScriptOptions,

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
import Options.Applicative

import qualified Prod.Tracer as Prod
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import qualified System.Agents.OneShot as OneShot
import qualified System.Agents.SessionPrint.Inject as SessionInject
import qualified System.Agents.SessionStore as SessionStore

import System.Agents.AgentTree (OSAgentTree (..))
import System.Agents.CLI.Aliases (AliasDefinition)
import System.Agents.CLI.PromptScript (
    MediaReference (..),
    PromptScript,
    PromptScriptDirective (..),
    interpretPromptScript,
    parseMediaReference,
    resolveMediaType,
 )
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

-- | Options specific to composing the prompt script, media attachments,
-- and thinking output for a one-shot command.
data PromptScriptOptions = PromptScriptOptions
    { psoPromptScript :: PromptScript
    , psoMediaFiles :: [MediaReference]
    , psoThinkingOutput :: OneShot.ThinkingOutput
    }
    deriving (Show)

-- | Options for the one-shot command
data OneShotOptions = OneShotOptions
    { sessionFile :: Maybe FilePath
    , promptScriptOptions :: PromptScriptOptions
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

-- | Parse the common prompt-script-related options.
parsePromptScriptOptions :: Parser PromptScriptOptions
parsePromptScriptOptions =
    PromptScriptOptions
        <$> parsePromptScriptInput
        <*> many parseMediaOption
        <*> parseThinkingOption

-- | Parse the --thinking option with choices: none, stdout, stderr
parseThinkingOption :: Parser OneShot.ThinkingOutput
parseThinkingOption =
    option
        (maybeReader parseThinking)
        ( long "thinking"
            <> metavar "TARGET"
            <> help "Where to output thinking content: none, stdout, or stderr (default: none)"
            <> value OneShot.ThinkingNone
            <> showDefaultWith showThinking
        )
  where
    parseThinking :: String -> Maybe OneShot.ThinkingOutput
    parseThinking "none" = Just OneShot.ThinkingNone
    parseThinking "stdout" = Just OneShot.ThinkingStdout
    parseThinking "stderr" = Just OneShot.ThinkingStderr
    parseThinking _ = Nothing

    showThinking :: OneShot.ThinkingOutput -> String
    showThinking OneShot.ThinkingNone = "none"
    showThinking OneShot.ThinkingStdout = "stdout"
    showThinking OneShot.ThinkingStderr = "stderr"

-- | Parse a media reference option.
--
-- Supports formats:
-- - image/png;/path/to/image.png (explicit MIME type)
-- - /path/to/image.png (inferred from extension)
parseMediaOption :: Parser MediaReference
parseMediaOption =
    option
        (eitherReader parseMediaReference)
        ( long "media"
            <> short 'm'
            <> metavar "MEDIA"
            <> help "Attach a media file. Format: [mime/type;]/path/to/file (can be specified multiple times)"
        )

-- | Parse prompt script input.
parsePromptScriptInput :: Parser PromptScript
parsePromptScriptInput =
    let pair = (,) <$> optional parseAliasPrompt <*> many parseRegularDirectives
     in fmap (\(md0, ds) -> maybe ds (: ds) md0) pair
  where
    parseRegularDirectives :: Parser PromptScriptDirective
    parseRegularDirectives =
        asum
            [ promptOption
            , fileOption
            , shellOption
            , smallSeparatorFlag
            , largeSeparatorFlag
            , sessionXSOption
            , sessionSOption
            , sessionMOption
            , sessionLOption
            , sessionXLOption
            ]

parseAliasPrompt :: Parser PromptScriptDirective
parseAliasPrompt =
    AliasPrompt
        <$> strOption
            ( long "alias"
                <> metavar "NAME"
                <> help "Use a predefined prompt alias (e.g., translate, summarize, code-review, explain)"
            )

promptOption :: Parser PromptScriptDirective
promptOption =
    Str
        <$> strOption
            ( long "prompt"
                <> short 'p'
                <> metavar "PROMPT"
                <> help "prompt text paragraph"
            )

fileOption :: Parser PromptScriptDirective
fileOption =
    FileContents
        <$> strOption
            ( long "file"
                <> short 'f'
                <> metavar "FILE"
                <> help "prompt text file"
            )

shellOption :: Parser PromptScriptDirective
shellOption =
    ShellOutput
        <$> strOption
            ( long "shell"
                <> metavar "SHELL"
                <> help "prompt the stdout of a shell command"
            )

smallSeparatorFlag :: Parser PromptScriptDirective
smallSeparatorFlag =
    Separator 4
        <$> strOption
            ( long "sep4"
                <> short 's'
                <> metavar "SEPARATOR"
                <> help "a short separator"
            )

largeSeparatorFlag :: Parser PromptScriptDirective
largeSeparatorFlag =
    Separator 40
        <$> strOption
            ( long "sep40"
                <> short 'S'
                <> metavar "SEPARATOR"
                <> help "a long separator"
            )

sessionXSOption :: Parser PromptScriptDirective
sessionXSOption =
    SessionContents
        <$> strOption
            ( long "session-xs"
                <> metavar "SESSIONFILE"
                <> help "inject session file content at minimal verbosity (queries/responses only, skips tool-only turns)"
            )
        <*> pure SessionInject.SessionXS

sessionSOption :: Parser PromptScriptDirective
sessionSOption =
    SessionContents
        <$> strOption
            ( long "session-s"
                <> metavar "SESSIONFILE"
                <> help "inject session file content at low verbosity (+thinking, +tool names)"
            )
        <*> pure SessionInject.SessionS

sessionMOption :: Parser PromptScriptDirective
sessionMOption =
    SessionContents
        <$> strOption
            ( long "session-m"
                <> metavar "SESSIONFILE"
                <> help "inject session file content at medium verbosity (+statistics)"
            )
        <*> pure SessionInject.SessionM

sessionLOption :: Parser PromptScriptDirective
sessionLOption =
    SessionContents
        <$> strOption
            ( long "session-l"
                <> metavar "SESSIONFILE"
                <> help "inject session file content at high verbosity (+tool call results)"
            )
        <*> pure SessionInject.SessionL

sessionXLOption :: Parser PromptScriptDirective
sessionXLOption =
    SessionContents
        <$> strOption
            ( long "session-xl"
                <> metavar "SESSIONFILE"
                <> help "inject session file content at maximum verbosity (complete)"
            )
        <*> pure SessionInject.SessionXL

{- | Load media attachments and interpret the prompt script from 'PromptScriptOptions'.

Returns the interpreted prompt text and the loaded media attachments.
Exits on failure to load any media file.
-}
loadPromptScriptOptions ::
    -- | Available prompt aliases
    Map Text AliasDefinition ->
    -- | Optional session file for alias input mode
    Maybe FilePath ->
    -- | Prompt script options
    PromptScriptOptions ->
    IO (Text, [MediaAttachment])
loadPromptScriptOptions aliases mSessionFile opts = do
    mediaResult <- loadMediaAttachments (psoMediaFiles opts)
    case mediaResult of
        Left err -> do
            Text.IO.hPutStrLn stderr $ "Error: " <> err
            exitFailure
        Right mediaAttachments -> do
            unless (null mediaAttachments) $ do
                Text.IO.hPutStrLn stderr $ "Attaching " <> Text.pack (show $ length mediaAttachments) <> " media file(s):"
                mapM_ (\m -> Text.IO.hPutStrLn stderr $ "  - " <> m.mediaMimeType <> ": " <> maybe "unnamed" id m.mediaFilename) mediaAttachments
            promptContents <- interpretPromptScript aliases (psoPromptScript opts) mSessionFile
            pure (promptContents, mediaAttachments)

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
    (promptContents, mediaAttachments) <-
        loadPromptScriptOptions aliases opts.sessionFile opts.promptScriptOptions
    apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile
    forM_ (take 1 agentFiles) $ \agentFilePath -> do
        mSession <- maybe (pure Nothing) SessionStore.readSessionFromFile opts.sessionFile
        -- Use OS-native agent loading (no registry needed)
        let oneShot text props =
                OneShot.mainOneShotTextWithThinking
                    (Prod.contramap OneShotTrace tracer)
                    sessionStore
                    opts.sessionFile
                    mSession
                    (psoThinkingOutput opts.promptScriptOptions)
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

