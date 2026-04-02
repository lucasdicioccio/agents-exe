{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{- | Secrets management for HTTP endpoint configurations.

This module provides a flexible system for managing sensitive data (API keys,
tokens, passwords) in HTTP endpoint configurations like OpenAPI and PostgREST.

Key features:
* Multiple secret sources: inline values, environment variables, files,
  API key files, or command output
* Multiple encodings: clear text, Base64, Hexadecimal
* Flexible serialization: HTTP headers (with optional templating) or
  query string parameters

Secrets are resolved at toolbox initialization time, ensuring that sensitive
data is read and decoded before any HTTP requests are made.

Example configuration:

@
{
  "secrets": [
    {
      "source": {"tag": "EnvVar", "contents": "API_TOKEN"},
      "decoder": {"tag": "Clear", "contents": true},
      "serializer": {"tag": "Header", "contents": ["Authorization", "Bearer {{secret}}"]}
    },
    {
      "source": {"tag": "FileSystem", "contents": "/run/secrets/api_key"},
      "decoder": {"tag": "Base64"},
      "serializer": {"tag": "Header", "contents": ["X-API-Key", null]}
    }
  ]
}
@

TODO: Consider the risk of logging secrets when using non-standard
serialization locations (e.g., custom headers that might be logged
by proxies or middleware).
-}
module System.Agents.Tools.Secrets (
    -- * Secret types
    SecretSource (..),
    SecretDecoder (..),
    SecretSerializer (..),
    Secret (..),
    ResolvedSecret (..),
    SecretResolutionError (..),

    -- * Secret resolution
    resolveSecrets,
    resolveSecret,
    resolveSecretSource,
    applySecretsToHeaders,
    applySecretsToQueryString,

    -- * Decoding
    decodeSecret,

    -- * Serialization
    serializeSecret,

    -- * Template handling
    applyTemplate,
) where

import Control.Exception (IOException, try)
import Data.ByteString.Base64 as Base64
import Data.Char (digitToInt, isHexDigit, isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import Data.Word (Word8)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO.Error (isDoesNotExistError)
import System.Process (readProcessWithExitCode)

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import System.Agents.ApiKeys (ApiKey (..), ApiKeys (..))

-- -------------------------------------------------------------------------
-- Secret Source Types
-- -------------------------------------------------------------------------

{- | Source location for a secret value.

The secret can come from various locations:

* 'Given': Inline text value (useful for testing or non-sensitive values)
* 'ApiKey': Reference to a key in the API keys file (same as LLM completion keys)
* 'FileSystem': Read from a file path (useful for Docker secrets, systemd credentials)
* 'EnvVar': Read from an environment variable
* 'Command': Execute a command and use its stdout as the secret
-}
data SecretSource
    = -- | An inline text value
      Given Text
    | -- | Reference to an API key by ID
      ApiKeySource Text
    | -- | Read from a file path
      FileSystem FilePath
    | -- | Read from an environment variable
      EnvVar Text
    | -- | Execute a command with arguments
      Command Text [Text]
    deriving (Show, Eq, Ord)

-- -------------------------------------------------------------------------
-- Secret Decoder Types
-- -------------------------------------------------------------------------

{- | Decoder for transforming raw secret bytes to the final value.

* 'Clear Bool': Plain text. The Bool indicates whether to trim trailing
  newlines from the raw value (useful for file-based secrets that often
  include a trailing newline).
* 'Base64': Decode from Base64 encoding
* 'Hexadecimal': Decode from hexadecimal encoding
-}
data SecretDecoder
    = -- | Plain text, with bool indicating whether to trim trailing newlines
      Clear Bool
    | -- | Base64 encoded
      Base64
    | -- | Hexadecimal encoded
      Hexadecimal
    deriving (Show, Eq, Ord)

-- -------------------------------------------------------------------------
-- Secret Serializer Types
-- -------------------------------------------------------------------------

{- | Template for formatting secret values in headers.

A template uses @{{secret}}@ as a placeholder for the secret value.
For example: @"Bearer {{secret}}"@ or @"ApiKey {{secret}}"@.

If the template is 'Nothing', only the secret value is used.
-}
type Template = Text

{- | How to serialize the secret in HTTP requests.

* 'Header Name (Maybe Template)': Add as an HTTP header. If a template
  is provided, the secret is substituted into it.
* 'QueryString Name': Add as a query string parameter

Note: Using 'QueryString' for secrets is generally discouraged as URLs
may be logged by proxies and servers. However, some legacy APIs require it.
-}
data SecretSerializer
    = -- | Serialize as HTTP header with optional template
      Header Text (Maybe Template)
    | -- | Serialize as query string parameter
      QueryString Text
    deriving (Show, Eq, Ord)

-- -------------------------------------------------------------------------
-- Secret Definition
-- -------------------------------------------------------------------------

{- | A complete secret configuration.

Defines where to get the secret, how to decode it, and where to
place it in HTTP requests.
-}
data Secret = Secret
    { secretSource :: SecretSource
    -- ^ Where to read the secret from
    , secretDecoder :: SecretDecoder
    -- ^ How to decode the secret value
    , secretSerializer :: SecretSerializer
    -- ^ How to serialize the secret in HTTP requests
    }
    deriving (Show, Eq, Ord)

-- -------------------------------------------------------------------------
-- JSON Instances
-- -------------------------------------------------------------------------

instance FromJSON SecretSource where
    parseJSON = Aeson.withObject "SecretSource" $ \o -> do
        tag <- o .: "tag"
        case (tag :: Text) of
            "Given" -> Given <$> o .: "contents"
            "ApiKey" -> ApiKeySource <$> o .: "contents"
            "FileSystem" -> FileSystem <$> o .: "contents"
            "EnvVar" -> EnvVar <$> o .: "contents"
            "Command" -> do
                contents <- o .: "contents"
                case contents of
                    (name : args) -> pure $ Command name args
                    [] -> fail "Command source requires at least a command name"
            _ -> fail "Unknown SecretSource tag. Expected: Given, ApiKey, FileSystem, EnvVar, Command"

instance ToJSON SecretSource where
    toJSON (Given txt) =
        Aeson.object ["tag" .= ("Given" :: Text), "contents" .= txt]
    toJSON (ApiKeySource keyId) =
        Aeson.object ["tag" .= ("ApiKey" :: Text), "contents" .= keyId]
    toJSON (FileSystem path) =
        Aeson.object ["tag" .= ("FileSystem" :: Text), "contents" .= path]
    toJSON (EnvVar var) =
        Aeson.object ["tag" .= ("EnvVar" :: Text), "contents" .= var]
    toJSON (Command name args) =
        Aeson.object
            [ "tag" .= ("Command" :: Text)
            , "contents" .= (name : args)
            ]

instance FromJSON SecretDecoder where
    parseJSON = Aeson.withObject "SecretDecoder" $ \o -> do
        tag <- o .: "tag"
        case (tag :: Text) of
            "Clear" -> Clear <$> o .: "contents"
            "Base64" -> pure Base64
            "Hexadecimal" -> pure Hexadecimal
            _ -> fail "Unknown SecretDecoder tag. Expected: Clear, Base64, Hexadecimal"

instance ToJSON SecretDecoder where
    toJSON (Clear trim) =
        Aeson.object ["tag" .= ("Clear" :: Text), "contents" .= trim]
    toJSON Base64 =
        Aeson.object ["tag" .= ("Base64" :: Text)]
    toJSON Hexadecimal =
        Aeson.object ["tag" .= ("Hexadecimal" :: Text)]

instance FromJSON SecretSerializer where
    parseJSON = Aeson.withObject "SecretSerializer" $ \o -> do
        tag <- o .: "tag"
        case (tag :: Text) of
            "Header" -> do
                contents <- o .: "contents"
                case contents of
                    Aeson.Array arr ->
                        if Vector.length arr == 2
                            then do
                                let nameVal = arr Vector.! 0
                                    templateVal = arr Vector.! 1
                                name <- parseJSON nameVal
                                mTemplate <- parseJSON templateVal
                                pure $ Header name mTemplate
                            else fail "Header serializer requires [name, template] array"
                    _ -> fail "Header serializer requires an array"
            "QueryString" -> QueryString <$> o .: "contents"
            _ -> fail "Unknown SecretSerializer tag. Expected: Header, QueryString"

instance ToJSON SecretSerializer where
    toJSON (Header name mTemplate) =
        Aeson.object
            [ "tag" .= ("Header" :: Text)
            , "contents" .= Aeson.Array (Vector.fromList [toJSON name, toJSON mTemplate])
            ]
    toJSON (QueryString name) =
        Aeson.object ["tag" .= ("QueryString" :: Text), "contents" .= name]

instance FromJSON Secret where
    parseJSON = Aeson.withObject "Secret" $ \o ->
        Secret
            <$> o .: "source"
            <*> o .: "decoder"
            <*> o .: "serializer"

instance ToJSON Secret where
    toJSON secret =
        Aeson.object
            [ "source" .= secretSource secret
            , "decoder" .= secretDecoder secret
            , "serializer" .= secretSerializer secret
            ]

-- -------------------------------------------------------------------------
-- Secret Resolution
-- -------------------------------------------------------------------------

{- | A resolved secret with its final value and serializer.

The 'resolvedValue' is the decoded, ready-to-use secret value.
The 'resolvedSerializer' determines how it should be serialized in requests.
-}
data ResolvedSecret = ResolvedSecret
    { resolvedValue :: Text
    , resolvedSerializer :: SecretSerializer
    }
    deriving (Show, Eq)

-- | Errors that can occur during secret resolution.
data SecretResolutionError
    = -- | The secret source was not found (file, env var, etc.)
      SourceNotFound Text
    | -- | Error reading from the secret source
      SourceReadError Text
    | -- | Error decoding the secret (invalid Base64, hex, etc.)
      DecodeError Text
    | -- | The referenced API key was not found
      ApiKeyNotFound Text
    | -- | Command failed with exit code and stderr
      CommandFailed Text Int Text
    deriving (Show, Eq)

{- | Resolve all secrets in a list.

Returns either the first error encountered or the list of resolved secrets.
-}
resolveSecrets ::
    -- | Path to API keys file (for ApiKey source)
    FilePath ->
    -- | Secrets to resolve
    [Secret] ->
    IO (Either SecretResolutionError [ResolvedSecret])
resolveSecrets apiKeysFile secrets =
    sequence <$> traverse (resolveSecret apiKeysFile) secrets

{- | Resolve a single secret.

Reads from the source, decodes the value, and prepares it for serialization.
-}
resolveSecret ::
    FilePath ->
    Secret ->
    IO (Either SecretResolutionError ResolvedSecret)
resolveSecret apiKeysFile Secret{..} = do
    eRawValue <- resolveSecretSource apiKeysFile secretSource
    case eRawValue of
        Left err -> pure $ Left err
        Right rawValue -> do
            case decodeSecret secretDecoder rawValue of
                Left err -> pure $ Left err
                Right decodedValue ->
                    pure $ Right $ ResolvedSecret decodedValue secretSerializer

-- | Resolve a secret source to a raw (possibly encoded) value.
resolveSecretSource ::
    FilePath ->
    SecretSource ->
    IO (Either SecretResolutionError Text)
resolveSecretSource _ (Given txt) = pure $ Right txt
resolveSecretSource _ (EnvVar varName) = do
    mValue <- lookupEnv (Text.unpack varName)
    case mValue of
        Nothing -> pure $ Left $ SourceNotFound $ "Environment variable not found: " <> varName
        Just value -> pure $ Right $ Text.pack value
resolveSecretSource _ (FileSystem path) = do
    result <- try @IOException $ LByteString.readFile path
    case result of
        Left e
            | isDoesNotExistError e ->
                pure $ Left $ SourceNotFound $ "File not found: " <> Text.pack path
            | otherwise ->
                pure $ Left $ SourceReadError $ "Error reading file " <> Text.pack path <> ": " <> Text.pack (show e)
        Right content -> pure $ Right $ Text.decodeUtf8 $ LByteString.toStrict content
resolveSecretSource apiKeysFile (ApiKeySource keyId) = do
    result <- try @IOException $ LByteString.readFile apiKeysFile
    case result of
        Left e ->
            pure $ Left $ SourceReadError $ "Error reading API keys file: " <> Text.pack (show e)
        Right content -> do
            case Aeson.eitherDecode content of
                Left err ->
                    pure $ Left $ DecodeError $ "Failed to parse API keys file: " <> Text.pack err
                Right (ApiKeys keys) ->
                    case filter (\k -> apiKeyId k == keyId) keys of
                        [] -> pure $ Left $ ApiKeyNotFound $ "API key not found: " <> keyId
                        (key : _) -> pure $ Right $ apiKeyValue key
resolveSecretSource _ (Command name args) = do
    let cmd = Text.unpack name
        cmdArgs = map Text.unpack args
    (exitCode, stdout, stderr) <- readProcessWithExitCode cmd cmdArgs ""
    case exitCode of
        ExitSuccess -> pure $ Right $ Text.pack stdout
        ExitFailure code ->
            pure $ Left $ CommandFailed name code (Text.pack stderr)

-- -------------------------------------------------------------------------
-- Secret Decoding
-- -------------------------------------------------------------------------

-- | Decode a raw secret value using the specified decoder.
decodeSecret :: SecretDecoder -> Text -> Either SecretResolutionError Text
decodeSecret (Clear trimNewlines) rawValue =
    let value = if trimNewlines then trimTrailingNewlines rawValue else rawValue
     in Right value
decodeSecret Base64 encoded =
    case Base64.decode (Text.encodeUtf8 encoded) of
        Left err -> Left $ DecodeError $ "Invalid Base64: " <> Text.pack err
        Right decoded -> Right $ Text.decodeUtf8 decoded
decodeSecret Hexadecimal encoded =
    case decodeHex (Text.unpack encoded) of
        Just decoded -> Right $ Text.decodeUtf8 $ ByteString.pack decoded
        Nothing -> Left $ DecodeError "Invalid hexadecimal encoding"

-- | Decode a hex string to bytes.
decodeHex :: String -> Maybe [Word8]
decodeHex [] = Just []
decodeHex (c1 : c2 : cs)
    | isHexDigit c1 && isHexDigit c2 =
        let byte = fromIntegral (digitToInt c1 * 16 + digitToInt c2) :: Word8
         in (byte :) <$> decodeHex cs
decodeHex _ = Nothing

-- | Trim trailing newlines and whitespace from text.
trimTrailingNewlines :: Text -> Text
trimTrailingNewlines = Text.dropWhileEnd isSpace

-- -------------------------------------------------------------------------
-- Secret Serialization
-- -------------------------------------------------------------------------

{- | Apply template substitution to a secret value.

The template should contain @{{secret}}@ as a placeholder.
If no template is provided, just the secret value is returned.

Examples:
>>> applyTemplate (Just "Bearer {{secret}}") "abc123"
"Bearer abc123"
>>> applyTemplate Nothing "abc123"
"abc123"
-}
applyTemplate :: Maybe Template -> Text -> Text
applyTemplate Nothing secret = secret
applyTemplate (Just template) secret = Text.replace "{{secret}}" secret template

{- | Serialize a resolved secret to a header name/value pair.

Returns 'Nothing' if the serializer is not a Header.
-}
serializeSecret :: ResolvedSecret -> Maybe (Text, Text)
serializeSecret ResolvedSecret{..} =
    case resolvedSerializer of
        Header name mTemplate ->
            Just (name, applyTemplate mTemplate resolvedValue)
        QueryString _ -> Nothing

{- | Get query string parameter from a resolved secret.

Returns 'Nothing' if the serializer is not a QueryString.
-}
serializeSecretToQuery :: ResolvedSecret -> Maybe (Text, Text)
serializeSecretToQuery ResolvedSecret{..} =
    case resolvedSerializer of
        Header _ _ -> Nothing
        QueryString name -> Just (name, resolvedValue)

-- -------------------------------------------------------------------------
-- Apply Secrets to HTTP Components
-- -------------------------------------------------------------------------

{- | Apply resolved secrets to a map of HTTP headers.

Secrets configured as 'Header' serializers are added to the header map.
Existing headers with the same name are overwritten.
-}
applySecretsToHeaders ::
    [ResolvedSecret] ->
    Map Text Text ->
    Map Text Text
applySecretsToHeaders secrets baseHeaders =
    let headerSecrets = Map.fromList $ mapMaybe serializeSecret secrets
     in Map.union headerSecrets baseHeaders

{- | Apply resolved secrets to a map of query string parameters.

Secrets configured as 'QueryString' serializers are added to the map.
Existing parameters with the same name are overwritten.

TODO: Consider logging risk when using query string for secrets.
-}
applySecretsToQueryString ::
    [ResolvedSecret] ->
    Map Text Text ->
    Map Text Text
applySecretsToQueryString secrets baseParams =
    let querySecrets = Map.fromList $ mapMaybe serializeSecretToQuery secrets
     in Map.union querySecrets baseParams
