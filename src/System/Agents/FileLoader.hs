-- | load agents from the filesystem
module System.Agents.FileLoader (
    module System.Agents.Base,
    module System.Agents.FileLoader.JSON,
    Trace (..),
    Agents (..),
    listJsonDirectory,
    listAgentDirectory,
    loadJsonFile,
    loadAgentFile,
    readAgentDescriptionFile,
    readAgentValueFile,
    InvalidAgentError (..),

    -- * Templates
    Template.TemplateEnv (..),
    Template.TemplateLibraries,
    Template.defaultTemplateEnv,
    Template.standardLibraries,
    Template.isTemplateFile,
) where

import qualified Data.Aeson as Aeson

import qualified Data.List as List
import Prod.Tracer (Tracer, runTracer)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import System.Agents.Base (Agent (..), AgentDescription (..))
import System.Agents.FileLoader.JSON
import qualified System.Agents.FileLoader.Template as Template

-------------------------------------------------------------------------------
data Trace
    = LoadJsonFile !FilePath
    | LoadJsonFileFailure !FilePath String
    deriving (Show)

loadJsonFile :: Tracer IO Trace -> FilePath -> IO (Either InvalidAgentError AgentDescription)
loadJsonFile tracer = loadAgentFile tracer Template.defaultTemplateEnv

{- | Load an agent file: a @.tramaj@ file is evaluated as a template (see
"System.Agents.FileLoader.Template"), anything else is read as JSON.
-}
loadAgentFile :: Tracer IO Trace -> Template.TemplateEnv -> FilePath -> IO (Either InvalidAgentError AgentDescription)
loadAgentFile tracer env path = do
    runTracer tracer (LoadJsonFile path)
    ret <- readAgentDescriptionFile env path
    case ret of
        Right d -> pure $ Right d
        Left e -> do
            runTracer tracer (LoadJsonFileFailure path e)
            pure $ Left $ LoadFailure path e

-- | 'loadAgentFile' without the traces.
readAgentDescriptionFile :: Template.TemplateEnv -> FilePath -> IO (Either String AgentDescription)
readAgentDescriptionFile env path
    | Template.isTemplateFile path = fmap snd <$> Template.readTemplateDescriptionFile env path
    | otherwise = readJsonDescriptionFile path

{- | An agent file's description together with the JSON value it was parsed
from: for a template, the value the program evaluated to.
-}
readAgentValueFile :: Template.TemplateEnv -> FilePath -> IO (Either String (Aeson.Value, AgentDescription))
readAgentValueFile env path
    | Template.isTemplateFile path = Template.readTemplateDescriptionFile env path
    | otherwise = do
        desc <- readJsonDescriptionFile path
        value <- Aeson.eitherDecodeFileStrict' path
        pure ((,) <$> value <*> desc)

-------------------------------------------------------------------------------
data Agents = Agents
    { dir :: FilePath
    , agents :: [AgentDescription]
    }

data InvalidAgentError
    = LoadFailure FilePath String
    deriving (Show)

{- | List all JSON files in a directory.
Returns an empty list if the directory does not exist.
-}
listJsonDirectory :: FilePath -> IO [FilePath]
listJsonDirectory path = do
    exists <- doesDirectoryExist path
    if exists
        then do
            entries <- listDirectory path
            pure $ sources entries
        else pure []
  where
    sources :: [FilePath] -> [FilePath]
    sources xs =
        fmap fullPath $
            List.filter isJson xs

    fullPath :: FilePath -> FilePath
    fullPath p = path </> p

    isJson :: FilePath -> Bool
    isJson p = takeExtension p == ".json"

{- | List the agent files of a directory: @.json@ files and @.tramaj@
templates, in that order. Returns an empty list if the directory does not
exist.
-}
listAgentDirectory :: FilePath -> IO [FilePath]
listAgentDirectory path = do
    exists <- doesDirectoryExist path
    if exists
        then do
            entries <- listDirectory path
            pure $
                fmap (path </>) $
                    List.filter (\p -> takeExtension p == ".json") entries
                        <> List.filter Template.isTemplateFile entries
        else pure []
