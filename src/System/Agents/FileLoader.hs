-- | load agents from the filesystem
module System.Agents.FileLoader (
    module System.Agents.Base,
    module System.Agents.FileLoader.JSON,
    Trace (..),
    Agents (..),
    listJsonDirectory,
    loadJsonFile,
    InvalidAgentError,
) where

import qualified Data.List as List
import Prod.Tracer (Tracer, runTracer)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

import System.Agents.Base (Agent (..), AgentDescription (..))
import System.Agents.FileLoader.JSON

-------------------------------------------------------------------------------
data Trace
    = LoadJsonFile !FilePath
    | LoadJsonFileFailure !FilePath String
    deriving (Show)

loadJsonFile :: Tracer IO Trace -> FilePath -> IO (Either InvalidAgentError AgentDescription)
loadJsonFile tracer path = do
    runTracer tracer (LoadJsonFile path)
    ret <- readJsonDescriptionFile path
    case ret of
        Right d -> pure $ Right d
        Left e -> do
            runTracer tracer (LoadJsonFileFailure path e)
            pure $ Left $ LoadFailure path e

-------------------------------------------------------------------------------
data Agents = Agents
    { dir :: FilePath
    , agents :: [AgentDescription]
    }

data InvalidAgentError
    = LoadFailure FilePath String
    deriving (Show)

listJsonDirectory :: FilePath -> IO [FilePath]
listJsonDirectory path = do
    sources <$> listDirectory path
  where
    sources :: [FilePath] -> [FilePath]
    sources xs =
        fmap fullPath $
            List.filter isJson xs

    fullPath :: FilePath -> FilePath
    fullPath p = path </> p

    isJson :: FilePath -> Bool
    isJson p = takeExtension p == ".json"
