{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Command-line options and the server's lifecycle.
module AgentsServer.Server (
    ServerOptions (..),
    serverOptions,
    runServer,

    -- * Reusing the server flags from another executable (@agents-exe serve@)
    agentFileOption,
    apiKeysOption,
    ServerFlags (..),
    serverFlags,
    serverOptionsFromFlags,
) where

import Control.Concurrent.Async (concurrently_)
import Control.Exception (IOException, catch, finally, throwIO, try)
import Control.Monad (forM_, void, when)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Time (NominalDiffTime)
import Network.Wai.Handler.Warp
import qualified Network.Socket as Socket
import Options.Applicative
import System.Directory (doesFileExist, removeFile)
import System.Posix.Files (ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)
import System.Posix.Signals (Handler (CatchOnce), installHandler, sigINT, sigTERM)

import AgentsServer.Api
import AgentsServer.Auth (loadAuthTokens)
import AgentsServer.Log
import AgentsServer.OpenApi (apiDocument)
import AgentsServer.UI (uiEnabled)
import qualified Data.ByteString.Char8 as Char8
import System.Agents.Host
import System.Agents.Host.Runner (recoverOnStartup, withSessionRunner)
import System.Agents.Postgres (isPostgresUrl, withPostgresStores)
import System.Agents.Tools.Params.Types (ProcessParams, ProcessValue (..))

data ServerOptions = ServerOptions
    { soAgentFiles :: [FilePath]
    , soApiKeysFile :: FilePath
    , soDatabase :: FilePath
    , soBind :: String
    , soPort :: Int
    , soLiveSessionTtl :: NominalDiffTime
    , soShutdownGrace :: Int
    -- ^ Seconds to let open requests finish on shutdown.
    , soAuthTokens :: Maybe FilePath
    -- ^ Bearer tokens and their owners; without, no authentication.
    , soStreamTokens :: Bool
    -- ^ Stream LLM answers as @text.delta@ events.
    , soAdminOwners :: [Text]
    -- ^ Owners allowed to store and delete agents.
    , soNoUI :: Bool
    -- ^ Do not serve the chat page, even on a loopback bind.
    , soCorsOrigins :: [Text]
    {- ^ @--cors-origin@, repeatable: origins allowed to call this server
    cross-origin. @"*"@ is refused at startup when @--auth-tokens@ is on
    (see 'runServer').
    -}
    , soSocket :: Maybe FilePath
    {- ^ @--socket@: also listen on this Unix domain socket, in addition to
    @--bind@\/@--port@. A stale file at this path is removed at start; the
    socket is created with mode 0600. Requests over it carry no @Origin@
    and need no bearer token beyond what @--auth-tokens@ imposes elsewhere:
    the socket itself, and who can reach it, is the trust boundary.
    -}
    , soLegacySessionDirs :: [FilePath]
    {- ^ Read-only fallback locations for old @conv.<uuid>.json@ session
    history, honoured through 'Host.hcLegacySessionDirs'
    (@todos/os-as-standalone-server.md@ §6). Plain @agents-server@ (flags
    only) leaves this empty; @agents-exe serve@ fills it in from the
    resolved config's session-store read prefixes.
    -}
    , soProcessParams :: ProcessParams
    {- ^ @--set@/@--set-json@/@--pin@/@--pin-json@: process-scope parameter
    values shared by every loaded agent (@todos/tool-partial-application.md@,
    §4). A pinned one cannot be overridden by a session or message value.
    -}
    }

serverOptions :: Parser ServerOptions
serverOptions =
    serverOptionsFromFlags
        <$> agentFileOption
        <*> apiKeysOption
        <*> serverFlags "agents-server.db"
        <*> parseProcessParamsOptions

-- | @--agent-file@, repeatable, required at least once (as for @agents-server@).
agentFileOption :: Parser [FilePath]
agentFileOption = some (strOption (long "agent-file" <> metavar "FILE" <> help "Root agent file; repeat for several agents"))

-- | @--api-keys@, required (as for @agents-server@).
apiKeysOption :: Parser FilePath
apiKeysOption = strOption (long "api-keys" <> metavar "FILE" <> help "API keys file")

{- | The server's own flags: everything in 'ServerOptions' except the agent
files, the API keys file and the process parameters, which @agents-exe
serve@ gets from agents-exe's own global @--agent-file@\/@--agent@,
@--api-keys@ and @--set@\/@--pin@ instead of duplicating them (see
'AgentsServer.Server.ServerOptions' vs. 'ServerFlags').
-}
data ServerFlags = ServerFlags
    { sfDatabase :: FilePath
    , sfBind :: String
    , sfPort :: Int
    , sfLiveSessionTtl :: NominalDiffTime
    , sfShutdownGrace :: Int
    , sfAuthTokens :: Maybe FilePath
    , sfStreamTokens :: Bool
    , sfAdminOwners :: [Text]
    , sfNoUI :: Bool
    , sfCorsOrigins :: [Text]
    , sfSocket :: Maybe FilePath
    }

-- | @db@'s default value is the caller's to choose (@agents-exe serve@
-- defaults it next to the resolved sessions directory; @agents-server@
-- defaults it to @agents-server.db@ in the current directory).
serverFlags :: FilePath -> Parser ServerFlags
serverFlags defaultDb =
    ServerFlags
        <$> strOption (long "db" <> metavar "FILE|URL" <> value defaultDb <> showDefault <> help "SQLite file, or postgresql:// URL, for sessions")
        <*> strOption (long "bind" <> metavar "HOST" <> value "127.0.0.1" <> showDefault <> help "Address to listen on")
        <*> option auto (long "port" <> metavar "PORT" <> value 8080 <> showDefault <> help "Port to listen on")
        <*> (fromInteger <$> option auto (long "live-session-ttl" <> metavar "SECONDS" <> value 900 <> showDefault <> help "Idle time before a session's in-memory state is dropped"))
        <*> option auto (long "shutdown-grace" <> metavar "SECONDS" <> value 10 <> showDefault <> help "Time open requests get to finish on shutdown")
        <*> optional (strOption (long "auth-tokens" <> metavar "FILE" <> help "Bearer tokens and their owners; callers then only see their own sessions"))
        <*> switch (long "stream-tokens" <> help "Stream LLM answers, sending text.delta events as the text arrives")
        <*> ( maybe [] (filter (not . Text.null) . map Text.strip . Text.splitOn ",")
                <$> optional (strOption (long "admin-owners" <> metavar "OWNER,…" <> help "Owners allowed to store and delete agents over the API (needs --auth-tokens)"))
            )
        <*> switch (long "no-ui" <> help "Do not serve the chat page at /")
        <*> many (Text.pack <$> strOption (long "cors-origin" <> metavar "ORIGIN" <> help "Allow this origin to call the server cross-origin (e.g. http://localhost:5173); repeat for several, or pass \"*\" for any (needs no --auth-tokens)"))
        <*> optional (strOption (long "socket" <> metavar "PATH" <> help "Also listen on this Unix domain socket (in addition to --bind/--port); a stale file there is removed at start, the socket is created 0600. The socket is the local trust boundary: requests over it carry no Origin and need no bearer token beyond --auth-tokens"))

-- | Assemble a 'ServerOptions' from agent files, an API keys path, 'ServerFlags' and process parameters.
serverOptionsFromFlags :: [FilePath] -> FilePath -> ServerFlags -> ProcessParams -> ServerOptions
serverOptionsFromFlags agentFiles apiKeysFile flags params =
    ServerOptions
        { soAgentFiles = agentFiles
        , soApiKeysFile = apiKeysFile
        , soDatabase = flags.sfDatabase
        , soBind = flags.sfBind
        , soPort = flags.sfPort
        , soLiveSessionTtl = flags.sfLiveSessionTtl
        , soShutdownGrace = flags.sfShutdownGrace
        , soAuthTokens = flags.sfAuthTokens
        , soStreamTokens = flags.sfStreamTokens
        , soAdminOwners = flags.sfAdminOwners
        , soNoUI = flags.sfNoUI
        , soCorsOrigins = flags.sfCorsOrigins
        , soSocket = flags.sfSocket
        , soLegacySessionDirs = []
        , soProcessParams = params
        }

{- | Parse @--set@/@--set-json@/@--pin@/@--pin-json@ (all repeatable) into
'ProcessParams'. The @-json@ variant accepts any JSON value; the plain
variant always produces a string. @--pin@ marks the value so a session or
message value cannot override it (see @todos/tool-partial-application.md@, §4).
-}
parseProcessParamsOptions :: Parser ProcessParams
parseProcessParamsOptions =
    Map.fromList . concat
        <$> sequenceA
            [ many (parseOneParam False "set" "NAME=VALUE" "Set a process-scope parameter value; repeatable" parseStringValue)
            , many (parseOneParam True "pin" "NAME=VALUE" "Set a parameter value that sessions cannot override; repeatable" parseStringValue)
            , many (parseOneParam False "set-json" "NAME=JSON" "Set a parameter to any JSON value; repeatable" parseJsonValue)
            , many (parseOneParam True "pin-json" "NAME=JSON" "Set a JSON parameter value that sessions cannot override; repeatable" parseJsonValue)
            ]
  where
    parseOneParam :: Bool -> String -> String -> String -> (String -> Either String Aeson.Value) -> Parser (Text, ProcessValue)
    parseOneParam pinned longName meta helpText parseVal =
        option
            (eitherReader (parseNameValue pinned parseVal))
            (long longName <> metavar meta <> help helpText)

    parseNameValue :: Bool -> (String -> Either String Aeson.Value) -> String -> Either String (Text, ProcessValue)
    parseNameValue pinned parseVal raw = case break (== '=') raw of
        (name, '=' : val) | not (null name) -> do
            v <- parseVal val
            Right (Text.pack name, ProcessValue v pinned)
        _ -> Left ("expected NAME=VALUE, got: " <> raw)

    parseStringValue :: String -> Either String Aeson.Value
    parseStringValue = Right . Aeson.String . Text.pack

    parseJsonValue :: String -> Either String Aeson.Value
    parseJsonValue raw = case Aeson.eitherDecodeStrict (TextEnc.encodeUtf8 (Text.pack raw)) of
        Left err -> Left ("invalid JSON: " <> err)
        Right v -> Right v

{- | Load the agents, open the database, and serve until SIGTERM or SIGINT.

On a signal the server stops accepting connections, ends event streams,
answers waiting requests with the current state, gives open requests the
grace period, then cancels active runs (storing their sessions) and closes
the database.
-}
runServer :: ServerOptions -> Logger -> IO ()
runServer opts logger = do
    auth <- traverse loadAuthTokens opts.soAuthTokens
    when (not (null opts.soAdminOwners) && null auth) $
        throwIO $
            userError "--admin-owners needs --auth-tokens: without authentication, owners cannot be told apart"
    when ("*" `elem` opts.soCorsOrigins && isJust auth) $
        throwIO $
            userError "--cors-origin '*' needs no authentication: with --auth-tokens, list the exact origins allowed to send bearer tokens"
    let cfg =
            (defaultHostConfig opts.soAgentFiles opts.soApiKeysFile opts.soDatabase)
                { hcLiveSessionTtl = opts.soLiveSessionTtl
                , hcStreamTokens = opts.soStreamTokens
                , hcProcessParams = opts.soProcessParams
                , hcLegacySessionDirs = opts.soLegacySessionDirs
                }
        tracer = hostTraceLogger logger
        withStores k
            | isPostgresUrl opts.soDatabase =
                withPostgresStores (Char8.pack opts.soDatabase) $ \stores -> withHostStores cfg stores tracer k
            | otherwise = withHost cfg tracer k
    withStores $ \host ->
        withSessionRunner host $ \runner -> do
            _ <- recoverOnStartup runner
            env0 <- newServerEnv host runner auth
            let ui = not opts.soNoUI && uiEnabled (Text.pack opts.soBind) (isJust auth)
                env =
                    env0
                        { envAdmins = opts.soAdminOwners
                        , envUI = ui
                        , envDocument = Aeson.toJSON (apiDocument (Just (baseUrl opts)))
                        , envCorsOrigins = opts.soCorsOrigins
                        }
            mUnixSock <- traverse openUnixSocket opts.soSocket
            let started =
                    logLine logger "server.started" $
                        [ "bind" .= opts.soBind
                        , "port" .= opts.soPort
                        , "socket" .= opts.soSocket
                        , "agents" .= Map.keys host.hostAgents
                        , "admin_owners" .= opts.soAdminOwners
                        , "database" .= redactDatabase opts.soDatabase
                        , "authentication" .= (maybe "none" (const "bearer") auth :: String)
                        , "ui" .= ui
                        , "cors_origins" .= opts.soCorsOrigins
                        ]
                            <> [ "warning" .= ("no authentication: anyone who can reach this address can run the agents" :: String)
                               | Nothing <- [auth]
                               ]
                -- Only the TCP listener installs the OS signal handlers (once);
                -- 'onSignals' also closes the Unix socket, so both listeners
                -- stop accepting new connections on the same SIGTERM/SIGINT.
                settings =
                    setHost (fromString opts.soBind)
                        . setPort opts.soPort
                        . setBeforeMainLoop started
                        . setGracefulShutdownTimeout (Just opts.soShutdownGrace)
                        . setInstallShutdownHandler (onSignals env mUnixSock)
                        $ defaultSettings
                runTcp = runSettings settings (requestLogger logger (application env))
                runUnix sock =
                    let unixSettings = setGracefulShutdownTimeout (Just opts.soShutdownGrace) defaultSettings
                     in runSettingsSocket unixSettings sock (requestLogger logger (application env))
                        `finally` closeUnixSocket sock opts.soSocket
            case mUnixSock of
                Nothing -> runTcp
                Just sock -> concurrently_ runTcp (runUnix sock)
            logLine logger "server.stopping" []
    logLine logger "server.stopped" []
  where
    onSignals :: ServerEnv -> Maybe Socket.Socket -> IO () -> IO ()
    onSignals env mUnixSock closeSocket = do
        let stop = do
                logLine logger "server.signal" []
                requestShutdown env
                closeSocket
                -- Stop accepting on the Unix socket too; connections it already
                -- accepted keep running until they finish (bounded in practice
                -- by the process exiting, same as the TCP listener's --shutdown-grace).
                forM_ mUnixSock $ \sock -> try (Socket.close sock) >>= \case
                    Right () -> pure ()
                    Left (_ :: IOException) -> pure ()
        void $ installHandler sigTERM (CatchOnce stop) Nothing
        void $ installHandler sigINT (CatchOnce stop) Nothing

{- | Bind, listen, and secure the Unix domain socket for @--socket@: remove a
stale file at the path (from a previous, uncleanly stopped run), then
create the socket 0600.
-}
openUnixSocket :: FilePath -> IO Socket.Socket
openUnixSocket path = do
    stale <- doesFileExist path
    when stale $ removeFile path `catch` \(_ :: IOException) -> pure ()
    sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
    Socket.bind sock (Socket.SockAddrUnix path)
    Socket.listen sock Socket.maxListenQueue
    setFileMode path (ownerReadMode `unionFileModes` ownerWriteMode)
    pure sock

-- | Close and unlink the socket on shutdown; tolerant of it having already
-- been closed by 'onSignals', or the file already having been removed.
closeUnixSocket :: Socket.Socket -> Maybe FilePath -> IO ()
closeUnixSocket sock mpath = do
    _ <- try (Socket.close sock) :: IO (Either IOException ())
    forM_ mpath $ \path -> removeFile path `catch` \(_ :: IOException) -> pure ()

{- | The URL the OpenAPI document names as this server's own. A wildcard
bind is reported as localhost, which is where a reader of the document on
this machine would reach it.
-}
baseUrl :: ServerOptions -> Text
baseUrl opts = "http://" <> host <> ":" <> Text.pack (show opts.soPort)
  where
    host
        | opts.soBind `elem` ["0.0.0.0", "::", "*"] = "127.0.0.1"
        | otherwise = Text.pack opts.soBind

{- | A database for the logs: a Postgres URL loses its user and password,
@postgresql://user:secret\@db.example/agents@ becoming
@postgresql://db.example/agents@.
-}
redactDatabase :: String -> String
redactDatabase db = case break (== ':') db of
    (scheme, ':' : '/' : '/' : rest)
        | isPostgresUrl db ->
            let (authority, path) = break (== '/') rest
                host = reverse (takeWhile (/= '@') (reverse authority))
             in scheme <> "://" <> host <> path
    _ -> db
