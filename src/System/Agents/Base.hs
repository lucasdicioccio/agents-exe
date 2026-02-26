{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

module System.Agents.Base where

import Data.Aeson (FromJSON, ToJSON, (.:), (.=), (.:?))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)

import qualified System.Agents.Tools.OpenApi as OpenApi

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
    , openApiServers :: Maybe [OpenApiServerDescription]
    }
    deriving (Show, Ord, Eq, Generic)
instance ToJSON Agent
instance FromJSON Agent

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

-------------------------------------------------------------------------------
-- | Description of an OpenAPI server to load tools from
-- 
-- The 'openApiPredicates' field contains a list of conjunction predicates.
-- Each conjunction is a list of predicates that all must match for a tool
-- to be included. If any conjunction matches, the tool is included.
-- An empty list means "match all" (include all endpoints).
data OpenApiServerDescription
    = OpenApiServerDescription
    { openApiName :: Text                    -- ^ Name for this toolbox (e.g., "petstore")
    , openApiBaseUrl :: Text                 -- ^ Base URL for API calls (e.g., "https://api.example.com/v1")
    , openApiSpecUrl :: Text                 -- ^ URL to fetch the OpenAPI spec from
    , openApiAuthToken :: Maybe Text         -- ^ Optional Bearer token for authentication
    , openApiPredicates :: [[OpenApi.ToolPredicate]]  -- ^ List of conjunction predicates for filtering
    }
    deriving (Show, Ord, Eq, Generic)

instance FromJSON OpenApiServerDescription where
    parseJSON = Aeson.withObject "OpenApiServerDescription" $ \v ->
        OpenApiServerDescription
            <$> v .: "name"
            <*> v .: "baseUrl"
            <*> v .: "specUrl"
            <*> v .:? "authToken"
            <*> v .:? "predicates" Aeson..!= []

instance ToJSON OpenApiServerDescription where
    toJSON desc =
        Aeson.object $
            [ "name" .= desc.openApiName
            , "baseUrl" .= desc.openApiBaseUrl
            , "specUrl" .= desc.openApiSpecUrl
            ] <> maybe [] (\t -> ["authToken" .= t]) desc.openApiAuthToken
              <> ["predicates" .= desc.openApiPredicates | not (null desc.openApiPredicates)]

