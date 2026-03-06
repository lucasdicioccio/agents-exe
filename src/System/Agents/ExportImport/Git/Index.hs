{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Index file operations for git-based agent registries.
--
-- The index file provides a registry of all agents, tools, and MCP servers
-- in a repository, enabling fast lookups and explicit registration.
module System.Agents.ExportImport.Git.Index where

import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import System.Agents.ExportImport.Types

-------------------------------------------------------------------------------
-- Index Types
-------------------------------------------------------------------------------

-- | The root index file structure.
data AgentsIndex = AgentsIndex
    { idxVersion :: Text
    , idxLastUpdated :: UTCTime
    , idxAgents :: [AgentIndexEntry]
    , idxTools :: [ToolIndexEntry]
    , idxMcpServers :: [McpServerIndexEntry]
    }
    deriving (Show, Eq, Generic)

instance ToJSON AgentsIndex where
    toJSON idx = Aeson.object
        [ "version" .= idx.idxVersion
        , "lastUpdated" .= idx.idxLastUpdated
        , "agents" .= idx.idxAgents
        , "tools" .= idx.idxTools
        , "mcpServers" .= idx.idxMcpServers
        ]

instance FromJSON AgentsIndex where
    parseJSON = Aeson.withObject "AgentsIndex" $ \v -> AgentsIndex
        <$> v .: "version"
        <*> v .: "lastUpdated"
        <*> v .: "agents"
        <*> v .: "tools"
        <*> v .: "mcpServers"

-- | An entry in the agents index.
data AgentIndexEntry = AgentIndexEntry
    { agentNs :: Namespace
    , agentPath :: FilePath
    }
    deriving (Show, Eq, Generic)

instance ToJSON AgentIndexEntry where
    toJSON entry = Aeson.object
        [ "namespace" .= entry.agentNs
        , "path" .= entry.agentPath
        ]

instance FromJSON AgentIndexEntry where
    parseJSON = Aeson.withObject "AgentIndexEntry" $ \v -> AgentIndexEntry
        <$> v .: "namespace"
        <*> v .: "path"

-- | An entry in the tools index.
data ToolIndexEntry = ToolIndexEntry
    { toolNs :: Namespace
    , toolPath :: FilePath
    }
    deriving (Show, Eq, Generic)

instance ToJSON ToolIndexEntry where
    toJSON entry = Aeson.object
        [ "namespace" .= entry.toolNs
        , "path" .= entry.toolPath
        ]

instance FromJSON ToolIndexEntry where
    parseJSON = Aeson.withObject "ToolIndexEntry" $ \v -> ToolIndexEntry
        <$> v .: "namespace"
        <*> v .: "path"

-- | An entry in the MCP servers index.
data McpServerIndexEntry = McpServerIndexEntry
    { mcpName :: Text
    , mcpPath :: FilePath
    }
    deriving (Show, Eq, Generic)

instance ToJSON McpServerIndexEntry where
    toJSON entry = Aeson.object
        [ "name" .= entry.mcpName
        , "path" .= entry.mcpPath
        ]

instance FromJSON McpServerIndexEntry where
    parseJSON = Aeson.withObject "McpServerIndexEntry" $ \v -> McpServerIndexEntry
        <$> v .: "name"
        <*> v .: "path"

-------------------------------------------------------------------------------
-- Index Operations
-------------------------------------------------------------------------------

-- | Read the index file from a repository if it exists.
readIndex :: FilePath -> IO (Maybe AgentsIndex)
readIndex repoPath = do
    let path = repoPath </> indexFilePath
    exists <- doesFileExist path
    if exists
        then do
            result <- Aeson.eitherDecodeFileStrict' path
            case result of
                Left _ -> pure Nothing
                Right idx -> pure (Just idx)
        else pure Nothing

-- | Write the index file to a repository.
writeIndex :: FilePath -> AgentsIndex -> IO ()
writeIndex repoPath idx = do
    let path = repoPath </> indexFilePath
    BSL.writeFile path (AesonPretty.encodePretty idx)

-- | Build an index from an export package.
buildIndex :: UTCTime -> ExportPackage -> AgentsIndex
buildIndex now pkg = AgentsIndex
    { idxVersion = "1.0.0"
    , idxLastUpdated = now
    , idxAgents = map toAgentEntry (pkgAgents pkg)
    , idxTools = map toToolEntry (pkgTools pkg)
    , idxMcpServers = map toMcpEntry (pkgMcpServers pkg)
    }
  where
    toAgentEntry (ns, _) = AgentIndexEntry
        { agentNs = ns
        , agentPath = resolveAgentPath ns
        }
    toToolEntry (ns, _) = ToolIndexEntry
        { toolNs = ns
        , toolPath = resolveToolPath ns
        }
    toMcpEntry (name, _) = McpServerIndexEntry
        { mcpName = name
        , mcpPath = resolveMcpServerPath name
        }

-- | Find agents matching a namespace prefix in the index.
findAgentsByPrefix :: AgentsIndex -> Namespace -> [AgentIndexEntry]
findAgentsByPrefix idx prefix =
    filter (\e -> prefix `namespaceIsPrefixOf` e.agentNs) idx.idxAgents

-- | Find tools matching a namespace prefix in the index.
findToolsByPrefix :: AgentsIndex -> Namespace -> [ToolIndexEntry]
findToolsByPrefix idx prefix =
    filter (\e -> prefix `namespaceIsPrefixOf` e.toolNs) idx.idxTools

-- | Find an MCP server by name in the index.
findMcpServer :: AgentsIndex -> Text -> Maybe McpServerIndexEntry
findMcpServer idx name =
    case filter (\e -> e.mcpName == name) idx.idxMcpServers of
        (x:_) -> Just x
        [] -> Nothing

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

-- | Filter an index to only include entries matching the given namespaces.
filterIndex :: AgentsIndex -> [Namespace] -> AgentsIndex
filterIndex idx namespaces = idx
    { idxAgents = filter (matchesAnyNamespace . agentNs) idx.idxAgents
    , idxTools = filter (matchesAnyNamespace . toolNs) idx.idxTools
    }
  where
    matchesAnyNamespace ns = any (\prefix -> prefix `namespaceIsPrefixOf` ns) namespaces

-- | Convert index entries to file paths for sparse checkout.
sparseCheckoutPaths :: AgentsIndex -> [FilePath]
sparseCheckoutPaths idx =
    map agentPath idx.idxAgents ++
    map toolPath idx.idxTools ++
    map mcpPath idx.idxMcpServers ++
    [indexFilePath]

