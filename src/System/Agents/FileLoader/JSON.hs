-- | load agents from the filesystem
module System.Agents.FileLoader.JSON where

import qualified Data.Aeson as Aeson
import System.Agents.Base (AgentDescription)

readJsonDescriptionFile :: FilePath -> IO (Either String AgentDescription)
readJsonDescriptionFile = Aeson.eitherDecodeFileStrict'
