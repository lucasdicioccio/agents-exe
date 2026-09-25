{-# LANGUAGE OverloadedStrings #-}

{- | Bearer-token authentication.

A tokens file maps tokens to owners:

> {"tokens": [
>   {"owner": "alice", "sha256": "42f5d7b…"},
>   {"owner": "bob", "token": "a-plain-token"}
> ]}

@sha256@ is the lowercase hex SHA-256 of the token, so the file need not
hold the token itself (@printf %s TOKEN | sha256sum@). Several tokens may
share an owner.
-}
module AgentsServer.Auth (
    AuthTokens,
    loadAuthTokens,
    authTokensFromList,
    authenticate,
    bearerToken,
    tokenDigest,
    mintSessionToken,
) where

import Control.Exception (throwIO)
import Control.Monad (forM, when)
import Crypto.Hash (Digest, SHA256 (..), hashWith)
import Crypto.Random (getRandomBytes)
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.Char (isHexDigit, toLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | Owners by token digest.
newtype AuthTokens = AuthTokens (Map Text Text)

data TokenEntry = TokenEntry Text (Either Text Text)

instance Aeson.FromJSON TokenEntry where
    parseJSON = Aeson.withObject "token" $ \o -> do
        owner <- o .: "owner"
        hashed <- o .:? "sha256"
        plain <- o .:? "token"
        case (hashed, plain) of
            (Just h, Nothing) -> pure $ TokenEntry owner (Left h)
            (Nothing, Just t) -> pure $ TokenEntry owner (Right t)
            _ -> fail "each token needs exactly one of \"sha256\" and \"token\""

newtype TokensFile = TokensFile [TokenEntry]

instance Aeson.FromJSON TokensFile where
    parseJSON = Aeson.withObject "tokens file" $ \o -> TokensFile <$> o .: "tokens"

loadAuthTokens :: FilePath -> IO AuthTokens
loadAuthTokens path = do
    TokensFile entries <- either (fail . (("cannot read " <> path <> ": ") <>)) pure =<< Aeson.eitherDecodeFileStrict path
    when (null entries) $ throwIO $ userError (path <> ": no tokens")
    pairs <- forM entries $ \(TokenEntry owner secret) -> do
        when (Text.null owner) $ throwIO $ userError (path <> ": empty owner")
        digest <- case secret of
            Right token
                | Text.null token -> throwIO $ userError (path <> ": empty token for " <> Text.unpack owner)
                | otherwise -> pure $ tokenDigest (Text.encodeUtf8 token)
            Left hex
                | Text.length hex == 64 && Text.all isHexDigit hex -> pure $ Text.map toLower hex
                | otherwise -> throwIO $ userError (path <> ": sha256 for " <> Text.unpack owner <> " is not 64 hex digits")
        pure (digest, owner)
    pure $ AuthTokens (Map.fromList pairs)

-- | Tokens and their owners, e.g. for tests.
authTokensFromList :: [(Text, Text)] -> AuthTokens
authTokensFromList pairs = AuthTokens $ Map.fromList [(tokenDigest (Text.encodeUtf8 token), owner) | (token, owner) <- pairs]

-- | The owner of a token. Tokens are compared by digest.
authenticate :: AuthTokens -> ByteString.ByteString -> Maybe Text
authenticate (AuthTokens owners) token = Map.lookup (tokenDigest token) owners

-- | The token of an @Authorization: Bearer …@ header value.
bearerToken :: ByteString.ByteString -> Maybe ByteString.ByteString
bearerToken header = case Char8.words header of
    [scheme, token] | Char8.map toLower scheme == "bearer" -> Just token
    _ -> Nothing

-- | Lowercase hex SHA-256.
tokenDigest :: ByteString.ByteString -> Text
tokenDigest = Text.pack . show . (hashWith SHA256 :: ByteString.ByteString -> Digest SHA256)

{- | A fresh session token: 32 random bytes from the system's generator, in
hex, prefixed @st_@ so it is recognisable in a log or a secret scanner. Only
its 'tokenDigest' is ever stored.
-}
mintSessionToken :: IO Text
mintSessionToken = do
    bytes <- getRandomBytes 32 :: IO ByteString.ByteString
    pure $ "st_" <> Text.pack (concatMap hex2 (ByteString.unpack bytes))
  where
    hex2 w = [digit (w `div` 16), digit (w `mod` 16)]
    digit n = "0123456789abcdef" !! fromIntegral n
