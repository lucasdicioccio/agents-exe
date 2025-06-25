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

import System.Agents.ApiKeys (ApiKey (..), ApiKeys (..))
import System.Agents.Base (Agent (..), AgentDescription (..))

data InitializeError
    = AgentFilePreExists !FilePath
    | UnparseableAgentFile !FilePath
    | KeyFilePreExists !FilePath
    deriving (Show)

instance Exception (InitializeError)

initOpenAIAgent :: Agent -> FilePath -> IO ()
initOpenAIAgent o path = do
    exist <- doesFileExist path
    if exist
        then do
            throwIO $ AgentFilePreExists path
        else do
            LByteString.writeFile path $ Aeson.encodePretty (AgentDescription o)
            _ <- openFileWithEditor path
            putStrLn $ unwords ["agent definition saved at:", path]
            readBack <- Aeson.eitherDecodeFileStrict' path
            case readBack of
                (Left err) -> throwIO $ UnparseableAgentFile err
                (Right (AgentDescription o2)) -> do
                    createDirectoryIfMissing True o2.toolDirectory
                    putStrLn $ unwords ["tool dir:", o2.toolDirectory, "ok"]

initOpenAIKeys :: FilePath -> IO ()
initOpenAIKeys path = do
    exist <- doesFileExist path
    if exist
        then do
            throwIO $ KeyFilePreExists path
        else do
            putStrLn $ "You need to initialize an OpenAI Key."
            putStrLn $ ""
            putStrLn $ "For OpenAI:"
            putStrLn $ "- visit https://platform.openai.com/api-keys to generate a key, for this project agents."
            putStrLn $ "For MistralAI:"
            putStrLn $ "- visit https://console.mistral.ai/api-keys to generate a key, for this project agents."
            putStrLn $ ""
            putStrLn $ "Note: whenever applicable you should restrict the key to Model capabilities and bind the key to a budget-limited API while you experiment."
            putStrLn $ ""
            putStrLn $ "press ENTER to continue and edit the key with editor:"
            withEditor print
            LByteString.writeFile path $ Aeson.encodePretty (ApiKeys [ApiKey "main-key" "<insert-api-key>"])
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
