{-# LANGUAGE DeriveGeneric #-}

-- | load agents from the filesystem
module System.Agents.FileLoader.Base where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data AgentDescription
    = OpenAIAgentDescription OpenAIAgent
    | Unspecified Aeson.Value
    deriving (Show, Ord, Eq, Generic)
instance ToJSON AgentDescription
instance FromJSON AgentDescription

data OpenAIAgent
    = OpenAIAgent
    { slug :: Text
    , modelName :: Text
    , announce :: Text
    , systemPrompt :: [Text]
    , toolDirectory :: FilePath
    }
    deriving (Show, Ord, Eq, Generic)
instance ToJSON OpenAIAgent
instance FromJSON OpenAIAgent
