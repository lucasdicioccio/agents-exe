{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

module System.Agents.Base where

import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
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
data PingPongQuery
    = SomeQueryToAnswer Text
    | GaveToolAnswers
    | NoQuery
    deriving (Show)

getQueryToAnswer :: PingPongQuery -> Maybe Text
getQueryToAnswer (SomeQueryToAnswer t) = Just t
getQueryToAnswer _ = Nothing
