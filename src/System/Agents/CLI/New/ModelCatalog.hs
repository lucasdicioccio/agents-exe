{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Model catalog for the 'new agent' command.

The catalog maps model-name patterns to provider presets.  It can be
embedded in the binary, loaded from a local JSON file, or downloaded from
a remote URL so that users can keep the mapping up to date without
recompiling.
-}
module System.Agents.CLI.New.ModelCatalog (
    -- * Catalog types
    ModelCatalog (..),
    CatalogEntry (..),
    MatchPattern (..),

    -- * Built-in defaults
    defaultModelCatalog,
    defaultModelCatalogUrl,
    modelCatalogPath,

    -- * Persistence and network updates
    loadModelCatalog,
    saveModelCatalog,
    updateModelCatalogFromUrl,

    -- * Lookup and display
    lookupPresetForModel,
    listCatalogEntries,
    catalogEntriesText,
) where

import Control.Exception (try)
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.List (find)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))

import qualified System.Agents.HttpClient as HttpClient
import Prod.Tracer (silent)

-- | A simple glob-like pattern for matching model names.
--
-- * @"*"@ matches any sequence of characters.
-- * Matching is case-insensitive.
--
-- Examples: @"kimi-*"@, @"gpt-*"@, @"*llama*"@.
newtype MatchPattern = MatchPattern {getMatchPattern :: Text}
    deriving (Show, Eq, Ord)

instance Aeson.ToJSON MatchPattern where
    toJSON = Aeson.toJSON . getMatchPattern

instance Aeson.FromJSON MatchPattern where
    parseJSON = Aeson.withText "MatchPattern" (pure . MatchPattern)

-- | A catalog entry mapping one or more model patterns to a preset name.
data CatalogEntry = CatalogEntry
    { entryPatterns :: [MatchPattern]
    -- ^ Patterns that identify models covered by this entry.
    , entryPreset :: Text
    -- ^ Name of the 'System.Agents.CLI.New' preset to use.
    , entryDescription :: Maybe Text
    -- ^ Optional human-readable description.
    }
    deriving (Show, Eq)

instance Aeson.ToJSON CatalogEntry where
    toJSON e =
        Aeson.object $
            [ "patterns" Aeson..= e.entryPatterns
            , "preset" Aeson..= e.entryPreset
            ]
                ++ maybe [] (\d -> ["description" Aeson..= d]) e.entryDescription

instance Aeson.FromJSON CatalogEntry where
    parseJSON = Aeson.withObject "CatalogEntry" $ \v ->
        CatalogEntry
            <$> v Aeson..: "patterns"
            <*> v Aeson..: "preset"
            <*> v Aeson..:? "description"

-- | The full model catalog.
newtype ModelCatalog = ModelCatalog
    { catalogEntries :: [CatalogEntry]
    }
    deriving (Show, Eq)

instance Aeson.ToJSON ModelCatalog where
    toJSON c =
        Aeson.object
            [ "version" Aeson..= (1 :: Int)
            , "entries" Aeson..= c.catalogEntries
            ]

instance Aeson.FromJSON ModelCatalog where
    parseJSON = Aeson.withObject "ModelCatalog" $ \v -> do
        version <- v Aeson..:? "version" Aeson..!= (1 :: Int)
        when (version /= 1) $
            fail $
                "Unsupported model catalog version: " ++ show version
        ModelCatalog <$> v Aeson..: "entries"

-- | Default remote URL used to download an up-to-date model catalog.
--
-- Users can override this with the @--url@ option of
-- @agents-exe new models update@.
defaultModelCatalogUrl :: Text
defaultModelCatalogUrl =
    "https://raw.githubusercontent.com/lucasdicioccio/agents/main/models.json"

-- | Default path for the local model catalog inside the config directory.
modelCatalogPath :: FilePath -> FilePath
modelCatalogPath configDir = configDir </> "models.json"

-- | Built-in model catalog, used when no local file exists.
--
-- The patterns are intentionally broad so that common model families are
-- routed to the right provider preset without forcing the user to
-- remember provider URLs.
defaultModelCatalog :: ModelCatalog
defaultModelCatalog =
    ModelCatalog
        [ CatalogEntry
            { entryPatterns = map MatchPattern ["kimi-*"]
            , entryPreset = "kimi"
            , entryDescription = Just "Moonshot/Kimi models (api.moonshot.ai)"
            }
        , CatalogEntry
            { entryPatterns = map MatchPattern ["gpt-*", "o1*", "o3*", "chatgpt-*"]
            , entryPreset = "openai"
            , entryDescription = Just "OpenAI models (api.openai.com)"
            }
        , CatalogEntry
            { entryPatterns = map MatchPattern ["mistral-*", "codestral-*", "pixtral-*"]
            , entryPreset = "mistral"
            , entryDescription = Just "Mistral models (api.mistral.ai)"
            }
        , CatalogEntry
            { entryPatterns = map MatchPattern ["llama*", "qwen*", "gemma*", "mixtral*"]
            , entryPreset = "ollama"
            , entryDescription = Just "Common local models via Ollama"
            }
        ]

-- | Load a model catalog from disk, falling back to the built-in defaults
-- if the file does not exist.
loadModelCatalog :: FilePath -> IO (Either String ModelCatalog)
loadModelCatalog path = do
    exists <- doesFileExist path
    if not exists
        then pure $ Right defaultModelCatalog
        else do
            result <- try @IOError (LByteString.readFile path)
            case result of
                Left err ->
                    pure . Left $
                        "Cannot read model catalog at " ++ path ++ ": " ++ show err
                Right bs ->
                    case Aeson.eitherDecode' bs of
                        Left err -> pure . Left $ "Invalid model catalog JSON: " ++ err
                        Right cat -> pure $ Right cat

-- | Save a model catalog to disk, creating parent directories if needed.
saveModelCatalog :: FilePath -> ModelCatalog -> IO ()
saveModelCatalog path catalog = do
    createDirectoryIfMissing True (takeDirectory path)
    LByteString.writeFile path (Aeson.encodePretty catalog)

-- | Download a model catalog from a URL.
updateModelCatalogFromUrl :: Text -> IO (Either String ModelCatalog)
updateModelCatalogFromUrl url = do
    rt <- HttpClient.newRuntime HttpClient.NoToken
    rsp <- HttpClient.get rt silent url
    case rsp of
        Left (HttpClient.SomeError err) ->
            pure . Left $ "Download failed: " ++ err
        Right r ->
            case HttpClient.decodeBody (HttpClient.J r) of
                Nothing -> pure $ Left "Downloaded catalog is not valid JSON"
                Just catalog -> pure $ Right catalog

-- | Check whether a model name matches a single pattern.
--
-- Patterns without @*@ are matched exactly.  Patterns containing @*@ use
-- simple glob semantics: @*@ matches any (possibly empty) sequence of
-- characters.
matchesPattern :: Text -> MatchPattern -> Bool
matchesPattern model (MatchPattern pat) =
    let model' = Text.toLower model
        pat' = Text.toLower pat
        segments = Text.splitOn "*" pat'
     in case NonEmpty.nonEmpty segments of
            Nothing -> True
            Just ne
                | null (NonEmpty.tail ne) ->
                    if "*" `Text.isInfixOf` pat'
                        then NonEmpty.head ne `Text.isInfixOf` model'
                        else model' == NonEmpty.head ne
                | otherwise ->
                    let startsWithStar = Text.isPrefixOf "*" pat'
                        endsWithStar = Text.isSuffixOf "*" pat'
                        firstSeg = NonEmpty.head ne
                        lastSeg = NonEmpty.last ne
                        prefixOk =
                            if startsWithStar
                                then True
                                else Text.isPrefixOf firstSeg model'
                        suffixOk =
                            if endsWithStar
                                then True
                                else Text.isSuffixOf lastSeg model'
                        -- Empty segments can appear when a pattern starts
                        -- or ends with @*@, or contains consecutive @*@s
                        -- (e.g. @"kimi-*"@ splits into @["kimi-", ""]@).
                        -- They represent a zero-width match and must not be
                        -- passed to 'Text.breakOn', which errors on an empty
                        -- needle.
                        middle =
                            filter (not . Text.null) $
                                drop (if startsWithStar then 0 else 1) $
                                    take
                                        (length ne - if endsWithStar then 0 else 1)
                                        (NonEmpty.toList ne)
                     in prefixOk && suffixOk && goMiddle model' middle
  where
    goMiddle :: Text -> [Text] -> Bool
    goMiddle _ [] = True
    goMiddle t (s : ss)
        | Text.null s = goMiddle t ss
        | otherwise =
            case Text.breakOn s t of
                (_, "") -> False
                (_, rest) -> goMiddle (Text.drop (Text.length s) rest) ss

-- | Look up the preset name and matching catalog entry for a model.
lookupPresetForModel :: ModelCatalog -> Text -> Maybe (Text, CatalogEntry)
lookupPresetForModel catalog model =
    fmap (\e -> (e.entryPreset, e)) $
        find (\e -> any (matchesPattern model) e.entryPatterns) catalog.catalogEntries

-- | Human-readable list of catalog entries.
listCatalogEntries :: ModelCatalog -> [(Text, Text, Maybe Text)]
listCatalogEntries catalog =
    [ (patterns, e.entryPreset, e.entryDescription)
    | e <- catalog.catalogEntries
    , let patterns = Text.intercalate ", " (map getMatchPattern e.entryPatterns)
    ]

-- | Render the catalog as a multi-line 'Text' table.
catalogEntriesText :: ModelCatalog -> Text
catalogEntriesText catalog =
    Text.unlines $
        "Model patterns -> preset:" :
            [ patterns <> " -> " <> preset <> maybe "" (\d -> " (" <> d <> ")") desc
            | (patterns, preset, desc) <- listCatalogEntries catalog
            ]

