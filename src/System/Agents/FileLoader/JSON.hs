-- | load agents from the filesystem
module System.Agents.FileLoader.JSON where

import qualified Data.Aeson as Aeson
import System.Agents.FileLoader.Base

readJsonDescriptionFile :: FilePath -> IO (Either String AgentDescription)
readJsonDescriptionFile = Aeson.eitherDecodeFileStrict'
