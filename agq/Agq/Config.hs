{-# LANGUAGE DeriveGeneric #-}
module Agq.Config
  ( AgqConfig(..)
  , AgqLabels(..)
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

data AgqLabels = AgqLabels
  { labelToBeTaken :: Text  -- issue is ready to be picked up
  , labelTaken     :: Text  -- issue has been claimed
  , labelWait      :: Text  -- issue is waiting on dependencies
  , labelAgentPr   :: Text  -- PR was created by an agent
  } deriving (Generic, Show)

instance FromJSON AgqLabels
instance ToJSON AgqLabels

defaultLabels :: AgqLabels
defaultLabels = AgqLabels
  { labelToBeTaken = "agents/to-be-taken"
  , labelTaken     = "agents/taken"
  , labelWait      = "agents/wait"
  , labelAgentPr   = "agents/agent-pr"
  }

data AgqConfig = AgqConfig
  { queueDb          :: FilePath
  , taskDir          :: FilePath
  , sessionsDir      :: FilePath
  , baseBranch       :: Text
  , githubUsername   :: Text
  , pollSeconds      :: Int
  , lockStaleSeconds :: Int
  , defaultTries     :: Int            -- default tries_remaining for new tasks
  , projects         :: Map Text Text
  , agents           :: Map Text Text
  , hooks            :: Map Text Text  -- label -> hook script path (relative to worktree project dir)
  , labels           :: AgqLabels
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
  , defaultTries     = 3
  , projects         = Map.fromList [("root", "."), ("architect", ".")]
  , agents           = Map.fromList
      [ ("default",   "tasks-agents/kimi-agent-oneshot.json")
      , ("architect", "tasks-agents/kimi-architect.json")
      ]
  , hooks            = Map.fromList
      [ ("default", "git-agent-task.sh") ]
  , labels           = defaultLabels
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
