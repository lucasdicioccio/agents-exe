module System.Agents.CLI.InitProject where

import Control.Applicative ((<|>))
import Control.Exception (Exception, throwIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Maybe as Maybe
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.Process as Process

import System.Agents.FileLoader.Base (AgentDescription (..), OpenAIAgent (..))

data InitializeError
    = AgentFilePreExists !FilePath
    | UnparseableAgentFile !FilePath
    | KeyFilePreExists !FilePath
    deriving (Show)

instance Exception (InitializeError)

initOpenAIAgent :: OpenAIAgent -> FilePath -> IO ()
initOpenAIAgent o path = do
    exist <- doesFileExist path
    if exist
        then do
            throwIO $ AgentFilePreExists path
        else do
            LByteString.writeFile path $ Aeson.encodePretty (OpenAIAgentDescription o)
            _ <- openFileWithEditor path
            putStrLn $ unwords ["agent definition saved at:", path]
            readBack <- Aeson.eitherDecodeFileStrict' path
            case readBack of
                (Left err) -> throwIO $ UnparseableAgentFile err
                (Right (Unspecified _)) -> throwIO $ UnparseableAgentFile "unspecified agent description type"
                (Right (OpenAIAgentDescription o2)) -> do
                    createDirectoryIfMissing True o2.toolDirectory
                    putStrLn $ unwords ["tool dir:", o2.toolDirectory, "ok"]

initAgentsDir :: FilePath -> IO ()
initAgentsDir path = do
    createDirectoryIfMissing True path
    putStrLn $ unwords ["agents dir:", path, "ok"]

initOpenAIKey :: FilePath -> IO ()
initOpenAIKey path = do
    exist <- doesFileExist path
    if exist
        then do
            throwIO $ KeyFilePreExists path
        else do
            putStrLn $ "You need to initialize an OpenAI Key."
            putStrLn $ ""
            putStrLn $ "Visit https://platform.openai.com/api-keys to generate a key, for this project agents."
            putStrLn $ ""
            putStrLn $ "Note: you should restrict the key to Model capabilities."
            putStrLn $ ""
            putStrLn $ "press ENTER to continue and edit the key with editor:"
            withEditor print
            _ <- getLine
            openFileWithEditor path

openFileWithEditor :: FilePath -> IO ()
openFileWithEditor path =
    withEditor $ \(editor, baseArgs) ->
        Process.callProcess editor (baseArgs <> [path])

withEditor :: ((String, [String]) -> IO a) -> IO a
withEditor run = do
    argz <- Maybe.maybe ("vi", []) extractEditor <$> (lookupEnv "EDITOR" <|> lookupEnv "GIT_EDITOR")
    run argz
  where
    extractEditor :: String -> (String, [String])
    extractEditor cmd =
        let
            chunks = words cmd
         in
            (unwords $ take 1 chunks, drop 1 chunks)
