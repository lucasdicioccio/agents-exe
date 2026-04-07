{-# LANGUAGE OverloadedRecordDot #-}

module System.Agents.ApiKeys (
    -- * API Key types
    ApiKey (..),
    ApiKeys (..),

    -- * Loaded API keys
    LoadedApiKeys,
    readOpenApiKeysFile,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding

import qualified System.Agents.LLMs.OpenAI as OpenAI

data ApiKey = ApiKey
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

-- | Type alias for loaded API keys mapped to OpenAI API key format.
type LoadedApiKeys = [(Text, OpenAI.ApiKey)]

-- | Read and parse an API keys file, returning a list of key ID to OpenAI API key pairs.
readOpenApiKeysFile :: FilePath -> IO LoadedApiKeys
readOpenApiKeysFile keysPath =
    maybe [] flattenOpenAIKeys <$> readApiKeys keysPath
  where
    flattenOpenAIKeys :: ApiKeys -> [(Text, OpenAI.ApiKey)]
    flattenOpenAIKeys (ApiKeys keys) =
        [(k.apiKeyId, OpenAI.ApiKey $ TextEncoding.encodeUtf8 (k.apiKeyValue)) | k <- keys]

    readApiKeys :: FilePath -> IO (Maybe ApiKeys)
    readApiKeys path =
        Aeson.decode <$> LByteString.readFile path

