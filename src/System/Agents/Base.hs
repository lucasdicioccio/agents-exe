{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

module System.Agents.Base where

import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)

type AgentSlug = Text
type AgentAnnounce = Text

newtype AgentId = AgentId UUID
    deriving (Show, Ord, Eq, FromJSON, ToJSON)

newAgentId :: IO AgentId
newAgentId =
    AgentId <$> UUID.nextRandom

-------------------------------------------------------------------------------
newtype ConversationId = ConversationId UUID
    deriving (Show, Ord, Eq, FromJSON, ToJSON)

newConversationId :: IO ConversationId
newConversationId =
    ConversationId <$> UUID.nextRandom

-------------------------------------------------------------------------------
newtype StepId = StepId UUID
    deriving (Show, Ord, Eq, FromJSON, ToJSON)

newStepId :: IO StepId
newStepId =
    StepId <$> UUID.nextRandom

-------------------------------------------------------------------------------
-- Note: SessionId and TurnId are defined in System.Agents.Session.Types
-- to avoid circular dependencies between Session.Base and Tools.Context.
-- They are re-exported from Session.Base for convenience.
-------------------------------------------------------------------------------

-- | Reference to an additional agent outside the toolDirectory hierarchy
--
-- This enables agents to reference other agents explicitly, supporting
-- self-references and mutual recursion in the agent tree.
data ExtraAgentRef
    = ExtraAgentRef
    { extraAgentSlug :: AgentSlug
    -- ^ The slug to use when referring to this agent
    , extraAgentPath :: FilePath
    -- ^ Path to the agent's JSON configuration file
    }
    deriving (Show, Ord, Eq, Generic)

-- | Custom JSON options for ExtraAgentRef to use hyphenated field names
extraAgentRefOptions :: Aeson.Options
extraAgentRefOptions = Aeson.defaultOptions
    { Aeson.fieldLabelModifier = kebabCase . dropPrefix "extraAgent"
    }
  where
    -- Convert camelCase to kebab-case
    kebabCase [] = []
    kebabCase (c:cs) = toLower c : go cs
      where
        go [] = []
        go (x:xs)
            | x `elem` ['A'..'Z'] = '-' : toLower x : go xs
            | otherwise = x : go xs
    -- Drop the "extraAgent" prefix from field names
    dropPrefix prefix str
        | take (length prefix) str == prefix = drop (length prefix) str
        | otherwise = str

instance ToJSON ExtraAgentRef where
    toJSON = Aeson.genericToJSON extraAgentRefOptions
    toEncoding = Aeson.genericToEncoding extraAgentRefOptions

instance FromJSON ExtraAgentRef where
    parseJSON = Aeson.genericParseJSON extraAgentRefOptions

data Agent
    = Agent
    { slug :: Text
    , apiKeyId :: Text
    , flavor :: Text
    , modelUrl :: Text
    , modelName :: Text
    , announce :: Text
    , systemPrompt :: [Text]
    , toolDirectory :: FilePath
    , mcpServers :: Maybe [McpServerDescription]
    , extraAgents :: Maybe [ExtraAgentRef]
    }
    deriving (Show, Ord, Eq, Generic)

-- | Custom JSON options for Agent to use hyphenated field names
agentOptions :: Aeson.Options
agentOptions = Aeson.defaultOptions
    { Aeson.fieldLabelModifier = kebabCase
    , Aeson.omitNothingFields = True
    }
  where
    -- Convert camelCase to kebab-case
    kebabCase [] = []
    kebabCase (c:cs) = toLower c : go cs
      where
        go [] = []
        go (x:xs)
            | x `elem` ['A'..'Z'] = '-' : toLower x : go xs
            | otherwise = x : go xs

instance ToJSON Agent where
    toJSON = Aeson.genericToJSON agentOptions
    toEncoding = Aeson.genericToEncoding agentOptions

instance FromJSON Agent where
    parseJSON = Aeson.genericParseJSON agentOptions

data AgentDescription
    = AgentDescription Agent
    deriving (Show, Ord, Eq, Generic)
instance ToJSON AgentDescription where
    toJSON (AgentDescription val) =
        Aeson.object
            [ "tag" .= ("OpenAIAgentDescription" :: Text)
            , "contents" .= val
            ]

instance FromJSON AgentDescription where
    parseJSON = Aeson.withObject "AgentDescription" $ \v -> do
        tag <- v .: "tag"
        case (tag :: Text) of
            "OpenAIAgentDescription" ->
                AgentDescription <$> v .: "contents"
            _ -> fail "expecting OpenAIAgentDescription 'tag'"

-------------------------------------------------------------------------------
data McpSimpleBinaryConfiguration
    = McpSimpleBinaryConfiguration
    { name :: Text
    , executable :: FilePath
    , args :: [Text]
    }
    deriving (Show, Ord, Eq, Generic)

instance FromJSON McpSimpleBinaryConfiguration
instance ToJSON McpSimpleBinaryConfiguration
data McpServerDescription
    = McpSimpleBinary McpSimpleBinaryConfiguration
    deriving (Show, Ord, Eq, Generic)
instance ToJSON McpServerDescription where
    toJSON (McpSimpleBinary val) =
        Aeson.object
            [ "tag" .= ("McpSimpleBinary" :: Text)
            , "contents" .= val
            ]

instance FromJSON McpServerDescription where
    parseJSON = Aeson.withObject "McpServerDescription" $ \v -> do
        tag <- v .: "tag"
        case (tag :: Text) of
            "McpSimpleBinary" ->
                McpSimpleBinary <$> v .: "contents"
            _ -> fail "expecting McpSimpleBinary 'tag'"

