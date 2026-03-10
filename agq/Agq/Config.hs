{-# LANGUAGE DeriveGeneric #-}
module Agq.Config
  ( AgqConfig(..)
  , defaultConfig
  , loadConfig
  ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Generics (Generic)

data AgqConfig = AgqConfig
  { queueDb          :: FilePath
  , taskDir          :: FilePath
  , sessionsDir      :: FilePath
  , baseBranch       :: Text
  , githubUsername   :: Text
  , pollSeconds      :: Int
  , lockStaleSeconds :: Int
  , projects         :: Map Text Text
  , agents           :: Map Text Text
  } deriving (Generic, Show)

instance FromJSON AgqConfig
instance ToJSON AgqConfig

defaultConfig :: AgqConfig
defaultConfig = AgqConfig
  { queueDb          = "tasks/agq.db"
  , taskDir          = "tasks"
  , sessionsDir      = "tasks-sessions"
  , baseBranch       = "main"
  , githubUsername   = "lucasdicioccio"
  , pollSeconds      = 30
  , lockStaleSeconds = 7200
  , projects         = Map.fromList [("root", "."), ("architect", ".")]
  , agents           = Map.fromList
      [ ("default",   "tasks-agents/kimi-agent-oneshot.json")
      , ("architect", "tasks-agents/kimi-architect.json")
      ]
  }

loadConfig :: FilePath -> IO AgqConfig
loadConfig path = do
  bs <- LBS.readFile path
  case Aeson.eitherDecode bs of
    Left err -> do
      putStrLn $ "Warning: failed to parse " <> path <> ": " <> err
      putStrLn "Using default config."
      return defaultConfig
    Right cfg -> return cfg
