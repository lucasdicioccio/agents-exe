{-# LANGUAGE OverloadedRecordDot #-}

module System.Agents.ApiKeys where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Text as Text

data ApiKey
    = ApiKey
    { apiKeyId :: Text
    , apiKeyValue :: Text
    }
instance ToJSON ApiKey where
    toJSON k = Aeson.object ["id" .= k.apiKeyId, "value" .= k.apiKeyValue]
instance FromJSON ApiKey where
    parseJSON = Aeson.withObject "ApiKey" $ \o ->
        ApiKey <$> o .: "id" <*> o .: "value"

data ApiKeys = ApiKeys [ApiKey]
instance ToJSON ApiKeys where
    toJSON (ApiKeys xs) = Aeson.object ["keys" .= xs]
instance FromJSON ApiKeys where
    parseJSON = Aeson.withObject "ApiKeys" $ \o ->
        ApiKeys <$> o .: "keys"
