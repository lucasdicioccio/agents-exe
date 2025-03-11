-- | load agents from the filesystem
module System.Agents.FileLoader (
    module System.Agents.FileLoader.Base,
    module System.Agents.FileLoader.JSON,
    Trace (..),
    Agents (..),
    loadDirectory,
    loadJsonFile,
    InvalidAgentError,
) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Either (partitionEithers)
import qualified Data.List as List
import Prod.Tracer (Tracer, runTracer)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

import System.Agents.FileLoader.Base
import System.Agents.FileLoader.JSON

-------------------------------------------------------------------------------
data Trace
    = LoadJsonFile !FilePath
    | LoadJsonFileFailure !FilePath String
    deriving (Show)

data Agents = Agents
    { dir :: FilePath
    , agents :: [AgentDescription]
    }

loadDirectory :: Tracer IO Trace -> FilePath -> IO (Agents, [InvalidAgentError])
loadDirectory tracer path = do
    contents <- listDirectory path
    loaded <- mapConcurrently (loadJsonFile tracer) (sources contents)
    let (erroredScripts, loadedAgents) = partitionEithers loaded
    let ok = Agents path loadedAgents
    let ko = erroredScripts
    pure (ok, ko)
  where
    sources :: [FilePath] -> [FilePath]
    sources xs =
        fmap fullPath $
            List.filter isJson xs

    fullPath :: FilePath -> FilePath
    fullPath p = path </> p

    isJson :: FilePath -> Bool
    isJson p = takeExtension p == ".json"

data InvalidAgentError
    = LoadFailure FilePath String
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
