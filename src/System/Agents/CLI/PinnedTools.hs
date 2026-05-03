{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for pinned tool functionality.

Pinned tools allow users to create pre-configured tool variants with fixed
arguments. This is useful for:

- Creating convenient shortcuts for common tool invocations
- Ensuring consistent tool behavior with specific arguments
- Simplifying repetitive tool calls

Example pinned tool configuration:

>
> {
>   "pinnedTools": [
>     {
>       "name": "bash-in-src",
>       "tool": "bash",
>       "args": {
>         "cwd": "/project/src"
>       }
>     }
>   ]
> }

Usage:

> # Create a pinned variant
> agents-exe tool pin bash --pin-arg "cwd=/project/src" --name "bash-in-src"
>
> # Use pinned variant
> agents-exe tool call bash-in-src --arg '{"cmd": "ls -la"}'
>
> # List pinned tools
> agents-exe tool list
>
> # Remove a pinned tool
> agents-exe tool unpin bash-in-src
-}
module System.Agents.CLI.PinnedTools (
    -- * Types
    PinnedTool (..),
    PinnedToolsConfig (..),
    PinnedArg (..),

    -- * Configuration
    getPinnedToolsPath,
    loadPinnedTools,
    savePinnedTools,

    -- * Pinned tool operations
    findPinnedTool,
    addPinnedTool,
    removePinnedTool,
    listPinnedTools,

    -- * Argument handling
    applyPinnedArgs,
    parsePinnedArg,
    mergeArgs,

    -- * Validation
    validatePinnedToolName,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.FilePath ((</>))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

{- | A single pinned argument with name and value.

The value is stored as JSON to support various types (strings, numbers, booleans, etc.).
-}
data PinnedArg = PinnedArg
    { argName :: Text
    -- ^ Name of the argument (e.g., "cwd", "timeout")
    , argValue :: Aeson.Value
    -- ^ Value as JSON (supports strings, numbers, booleans, objects, arrays)
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON PinnedArg where
    toJSON arg =
        Aeson.object
            [ "name" Aeson..= arg.argName
            , "value" Aeson..= arg.argValue
            ]

instance Aeson.FromJSON PinnedArg where
    parseJSON = Aeson.withObject "PinnedArg" $ \v ->
        PinnedArg
            <$> v Aeson..: "name"
            <*> v Aeson..: "value"

{- | A pinned tool definition.

A pinned tool is a named configuration that wraps an existing tool
with pre-configured arguments.
-}
data PinnedTool = PinnedTool
    { pinnedName :: Text
    -- ^ Unique name for this pinned configuration
    , pinnedTool :: Text
    -- ^ Name of the underlying tool (e.g., "bash", "curl")
    , pinnedArgs :: Map Text Aeson.Value
    -- ^ Map of argument names to their pinned JSON values
    , pinnedDescription :: Maybe Text
    -- ^ Optional description of what this pinned tool does
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON PinnedTool where
    toJSON pt =
        Aeson.object $
            [ "name" Aeson..= pt.pinnedName
            , "tool" Aeson..= pt.pinnedTool
            , "args" Aeson..= pt.pinnedArgs
            ]
                ++ maybe [] (\d -> ["description" Aeson..= d]) pt.pinnedDescription

instance Aeson.FromJSON PinnedTool where
    parseJSON = Aeson.withObject "PinnedTool" $ \v ->
        PinnedTool
            <$> v Aeson..: "name"
            <*> v Aeson..: "tool"
            <*> v Aeson..:? "args" Aeson..!= Map.empty
            <*> v Aeson..:? "description"

{- | Top-level configuration container for pinned tools.
-}
newtype PinnedToolsConfig = PinnedToolsConfig
    { pinnedTools :: [PinnedTool]
    -- ^ List of pinned tool configurations
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON PinnedToolsConfig where
    toJSON cfg = Aeson.object ["pinnedTools" Aeson..= cfg.pinnedTools]

instance Aeson.FromJSON PinnedToolsConfig where
    parseJSON = Aeson.withObject "PinnedToolsConfig" $ \v ->
        PinnedToolsConfig
            <$> v Aeson..:? "pinnedTools" Aeson..!= []

-------------------------------------------------------------------------------
-- Configuration File Operations
-------------------------------------------------------------------------------

{- | Get the path to the pinned tools configuration file.

The file is stored in the user's config directory under ~/.config/agents-exe/pinned-tools.json
-}
getPinnedToolsPath :: IO FilePath
getPinnedToolsPath = do
    homeDir <- getHomeDirectory
    pure $ homeDir </> ".config" </> "agents-exe" </> "pinned-tools.json"

{- | Load the pinned tools configuration.

Returns an empty configuration if the file doesn't exist.
Returns an error if the file exists but contains invalid JSON.
-}
loadPinnedTools :: IO (Either String PinnedToolsConfig)
loadPinnedTools = do
    path <- getPinnedToolsPath
    exists <- doesFileExist path
    if not exists
        then pure $ Right (PinnedToolsConfig [])
        else do
            result <- LByteString.readFile path
            pure $ Aeson.eitherDecode result

{- | Save the pinned tools configuration.

Creates the config directory if it doesn't exist.
-}
savePinnedTools :: PinnedToolsConfig -> IO ()
savePinnedTools cfg = do
    path <- getPinnedToolsPath
    createDirectoryIfMissing True (takeDir path)
    LByteString.writeFile path (Aeson.encodePretty cfg)
  where
    takeDir = reverse . dropWhile (/= '/') . reverse

-------------------------------------------------------------------------------
-- Pinned Tool Operations
-------------------------------------------------------------------------------

{- | Find a pinned tool by name.
-}
findPinnedTool :: Text -> PinnedToolsConfig -> Maybe PinnedTool
findPinnedTool name cfg = find ((== name) . pinnedName) cfg.pinnedTools
  where
    find _ [] = Nothing
    find p (x : xs) = if p x then Just x else find p xs

{- | Add a new pinned tool to the configuration.

If a tool with the same name already exists, it will be replaced.
-}
addPinnedTool :: PinnedTool -> PinnedToolsConfig -> PinnedToolsConfig
addPinnedTool newTool cfg =
    let filtered = filter ((/= newTool.pinnedName) . pinnedName) cfg.pinnedTools
     in PinnedToolsConfig (newTool : filtered)

{- | Remove a pinned tool by name.

Returns Nothing if the tool wasn't found.
-}
removePinnedTool :: Text -> PinnedToolsConfig -> Maybe PinnedToolsConfig
removePinnedTool name cfg =
    let (removed, remaining) = partition ((== name) . pinnedName) cfg.pinnedTools
     in if null removed then Nothing else Just (PinnedToolsConfig remaining)
  where
    partition _ [] = ([], [])
    partition p (x : xs) =
        let (ys, zs) = partition p xs
         in if p x then (x : ys, zs) else (ys, x : zs)

{- | List all pinned tools.
-}
listPinnedTools :: PinnedToolsConfig -> [PinnedTool]
listPinnedTools = pinnedTools

-------------------------------------------------------------------------------
-- Argument Handling
-------------------------------------------------------------------------------

{- | Parse a pinned argument string.

Supports formats:
- @name=value@ - String value
- @name=true@ / @name=false@ - Boolean value
- @name=123@ - Number value
- @name={...}@ / @name=[...]@ - JSON object/array value

Examples:
- @"cwd=/project/src"@ → PinnedArg "cwd" (String "/project/src")
- @"timeout=30"@ → PinnedArg "timeout" (Number 30)
- @"verbose=true"@ → PinnedArg "verbose" (Bool True)
-}
parsePinnedArg :: String -> Either String PinnedArg
parsePinnedArg input =
    case break (== '=') input of
        (namePart, '=' : valuePart) ->
            let name = Text.pack namePart
             in case parseValue valuePart of
                    Left err -> Left $ "Invalid value for '" ++ namePart ++ "': " ++ err
                    Right val -> Right $ PinnedArg name val
        _ -> Left $ "Invalid pin-arg format: " ++ input ++ ". Expected: name=value"
  where
    parseValue :: String -> Either String Aeson.Value
    parseValue str
        -- Try to parse as JSON first (for objects, arrays, numbers)
        | headMay str `elem` [Just '{', Just '['] =
            case Aeson.eitherDecode (LByteString.fromStrict $ Text.Encoding.encodeUtf8 $ Text.pack str) of
                Left _ -> Right $ Aeson.String (Text.pack str)
                Right val -> Right val
        | str == "true" = Right $ Aeson.Bool True
        | str == "false" = Right $ Aeson.Bool False
        | str == "null" = Right Aeson.Null
        | all isDigit str =
            case reads str of
                [(n, "")] -> Right $ Aeson.Number (fromIntegral (n :: Int))
                _ -> Right $ Aeson.String (Text.pack str)
        | all isFloat str =
            case reads str of
                [(f, "")] -> Right $ Aeson.Number (realToFrac (f :: Double))
                _ -> Right $ Aeson.String (Text.pack str)
        | otherwise = Right $ Aeson.String (Text.pack str)

    headMay [] = Nothing
    headMay (x : _) = Just x

    isDigit c = c >= '0' && c <= '9'

    isFloat c = isDigit c || c == '.' || c == '-' || c == 'e' || c == 'E' || c == '+'

{- | Apply pinned arguments to a base set of arguments.

Pinned arguments are merged with the provided arguments, with the
provided arguments taking precedence (unless the pinned tool
explicitly disallows overrides - not yet implemented).

The merge is shallow - nested objects are not deeply merged.
-}
applyPinnedArgs :: Map Text Aeson.Value -> Aeson.Object -> Aeson.Object
applyPinnedArgs pinned userArgs =
    -- User args take precedence over pinned args
    let pinnedAsObject = KeyMap.fromMapText pinned
     in KeyMap.union userArgs pinnedAsObject

{- | Merge two JSON objects, with the second taking precedence.
-}
mergeArgs :: Aeson.Object -> Aeson.Object -> Aeson.Object
mergeArgs = KeyMap.union

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

{- | Validate a pinned tool name.

Rules:
- Must be non-empty
- Must start with a letter or underscore
- Can contain letters, digits, underscores, and hyphens
- Must not exceed 64 characters
-}
validatePinnedToolName :: Text -> Either String ()
validatePinnedToolName name
    | Text.null name = Left "Pinned tool name cannot be empty"
    | Text.length name > 64 = Left "Pinned tool name cannot exceed 64 characters"
    | not (isValidFirst (Text.head name)) =
        Left "Pinned tool name must start with a letter or underscore"
    | not (Text.all isValidChar name) =
        Left "Pinned tool name can only contain letters, digits, underscores, and hyphens"
    | otherwise = Right ()
  where
    isValidFirst c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
    isValidChar c = isValidFirst c || (c >= '0' && c <= '9') || c == '-'

