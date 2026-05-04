{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module System.Agents.Base where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)

import System.Agents.Tools.Activation (Activation)
import System.Agents.Tools.EndpointPredicate (EndpointPredicate)
import System.Agents.Tools.PostgREST.Types (HttpMethod (..))
import System.Agents.Tools.Secrets (Secret)
import System.Agents.Tools.Skills.Types (SkillName, SkillSource)

-- Import FileSandbox types for unified sandboxing
import System.Agents.FileSandbox.Predicate (PathPredicate (..))

-- Note: FileSandboxConfig is defined here to avoid circular dependencies
-- The full implementation is in System.Agents.FileSandbox

{- | Configuration for a file sandbox.
This is a simplified version; the full implementation is in FileSandbox module.
-}
data FileSandboxConfig = FileSandboxConfig
    { fsbPredicate :: PathPredicate
    -- ^ The predicate defining allowed file access
    , fsbMaxFileSize :: Maybe Integer
    -- ^ Maximum file size in bytes (Nothing = no limit)
    , fsbName :: Maybe Text
    -- ^ Optional human-readable name for the sandbox
    }
    deriving (Show, Ord, Eq, Generic)

instance FromJSON FileSandboxConfig
instance ToJSON FileSandboxConfig

-- | Default sandbox configuration (denies all access).
defaultFileSandboxConfig :: FileSandboxConfig
defaultFileSandboxConfig =
    FileSandboxConfig
        { fsbPredicate = AlwaysDeny
        , fsbMaxFileSize = Just (50 * 1024 * 1024) -- 50MB default
        , fsbName = Nothing
        }

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

{- | Reference to an additional agent outside the toolDirectory hierarchy

This enables agents to reference other agents explicitly, supporting
self-references and mutual recursion in the agent tree.
-}
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
extraAgentRefOptions =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = kebabCase . dropPrefix "extraAgent"
        }
  where
    -- Convert camelCase to kebab-case
    kebabCase [] = []
    kebabCase (c : cs) = toLower c : go cs
      where
        go [] = []
        go (x : xs)
            | x `elem` ['A' .. 'Z'] = '-' : toLower x : go xs
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

-------------------------------------------------------------------------------
-- Bash Toolbox Configuration
-------------------------------------------------------------------------------

{- | Configuration for a filesystem directory as a bash tool source.

Describes a directory containing executable bash tools that follow
the standard bash-tool protocol (describe|run).

Example configuration:

@
{
  "tag": "FileSystemDirectory",
  "contents": {
    "path": "./tools",
    "basenameFilter": null,
    "lifetime": "conversation",
    "activation": "always"
  }
}
@

The optional 'basenameFilter' field allows filtering tools by their
filename. If specified, only executables whose names contain the
filter string will be loaded.

The 'activation' field controls progressive disclosure (default: 'AlwaysActivated').

Note: Relative paths are resolved relative to the execution's current
working directory.
-}
data FileSystemDirectoryDescription
    = FileSystemDirectoryDescription
    { fsDirRoot :: Maybe FilePath
    -- ^ Root path, this field is here to support legacy tools definitions
    , fsDirPath :: FilePath
    -- ^ Path to the directory containing bash tools
    , fsDirBasenameFilter :: Maybe Text
    -- ^ Optional filter for tool filenames (e.g., ".sh" to load only .sh files)
    , fsDirActivation :: Maybe Activation
    -- ^ Optional activation mode (default: AlwaysActivated)
    }
    deriving (Show, Ord, Eq, Generic)

-- | Custom JSON options for FileSystemDirectoryDescription
fsDirOptions :: Aeson.Options
fsDirOptions =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = dropPrefix "fsDir"
        , Aeson.omitNothingFields = True
        }
  where
    dropPrefix prefix str
        | take (length prefix) str == prefix = drop (length prefix) str
        | otherwise = str

instance ToJSON FileSystemDirectoryDescription where
    toJSON = Aeson.genericToJSON fsDirOptions
    toEncoding = Aeson.genericToEncoding fsDirOptions

instance FromJSON FileSystemDirectoryDescription where
    parseJSON = Aeson.genericParseJSON fsDirOptions

{- | Configuration for a single tool executable.

Describes a single executable file to be loaded as a bash tool.
This is useful when you want to add a specific tool without loading
an entire directory.

Example configuration:

@
{
  "tag": "SingleTool",
  "contents": {
    "path": "/path/to/my-tool.sh",
    "lifetime": "conversation",
    "activation": "always"
  }
}
@

Note: Relative paths are resolved relative to the execution's current
working directory.
-}
data SingleToolDescription = SingleToolDescription
    { singleToolPath :: FilePath
    -- ^ Path to the single executable tool
    , singleToolActivation :: Maybe Activation
    -- ^ Optional activation mode (default: AlwaysActivated)
    }
    deriving (Show, Ord, Eq, Generic)

-- | Custom JSON options for SingleToolDescription
singleToolOptions :: Aeson.Options
singleToolOptions =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = dropPrefix "singleTool"
        , Aeson.omitNothingFields = True
        }
  where
    dropPrefix prefix str
        | take (length prefix) str == prefix = drop (length prefix) str
        | otherwise = str

instance ToJSON SingleToolDescription where
    toJSON = Aeson.genericToJSON singleToolOptions
    toEncoding = Aeson.genericToEncoding singleToolOptions

instance FromJSON SingleToolDescription where
    parseJSON = Aeson.genericParseJSON singleToolOptions

{- | Wrapper type for bash toolbox descriptions with tag-based JSON serialization.

This is a tagged union type that allows extensible bash toolbox types.
New bash toolbox types can be added in the future without breaking
existing configurations.

Example configuration:

@
{
  "bashToolboxes": [
    {"tag": "FileSystemDirectory", "contents": {"path": "./tools", "basenameFilter": null}},
    {"tag": "FileSystemDirectory", "contents": {"path": "./extra-tools", "basenameFilter": ".sh"}},
    {"tag": "SingleTool", "contents": {"path": "/path/to/special-tool.sh"}}
  ]
}
@

The old 'tools' key (singular directory path) is still supported for
backward compatibility. If both 'tools' and 'bashToolboxes' are specified,
both will be used.

Note: Relative paths are resolved relative to the execution's current
working directory.
-}
data BashToolboxDescription
    = FileSystemDirectory FileSystemDirectoryDescription
    | SingleTool SingleToolDescription
    deriving (Show, Ord, Eq, Generic)

instance ToJSON BashToolboxDescription where
    toJSON (FileSystemDirectory val) =
        Aeson.object
            [ "tag" .= ("FileSystemDirectory" :: Text)
            , "contents" .= val
            ]
    toJSON (SingleTool val) =
        Aeson.object
            [ "tag" .= ("SingleTool" :: Text)
            , "contents" .= val
            ]

instance FromJSON BashToolboxDescription where
    parseJSON = Aeson.withObject "BashToolboxDescription" $ \v -> do
        tag <- v .: "tag"
        case (tag :: Text) of
            "FileSystemDirectory" ->
                FileSystemDirectory <$> v .: "contents"
            "SingleTool" ->
                SingleTool <$> v .: "contents"
            _ -> fail "expecting 'FileSystemDirectory' or 'SingleTool' tag"

-------------------------------------------------------------------------------
-- OpenAPI Toolbox Configuration
-------------------------------------------------------------------------------

{- | Configuration for an OpenAPI toolbox.

This describes an OpenAPI specification to load as a toolbox.
The toolbox will fetch the spec from 'openApiSpecUrl' and make
all operations available as tools to the agent.

The spec URL can be either:
* An HTTP/HTTPS URL (e.g., "https://api.example.com/openapi.json")
* A file URL (e.g., "file:///path/to/openapi.json")

Example configuration:

@
{
  "tag": "OpenAPIServer",
  "contents": {
    "specUrl": "https://api.example.com/openapi.json",
    "baseUrl": "https://api.example.com",
    "headers": {"X-API-Version": "v1"},
    "token": "${API_TOKEN}",
    "filter": {"tag": "PathPrefix", "contents": "/api/v1"},
    "lifetime": "conversation",
    "activation": "always",
    "secrets": [
      {
        "source": {"tag": "EnvVar", "contents": "API_KEY"},
        "decoder": {"tag": "Clear", "contents": true},
        "serializer": {"tag": "Header", "contents": ["X-API-Key", null]}
      }
    ]
  }
}
@

The 'secrets' field allows flexible secret management with support for
multiple sources (environment variables, files, commands), encodings
(Base64, hex), and serialization locations (headers, query string).

The optional 'filter' field allows restricting which endpoints are
exposed as tools. See 'EndpointPredicate' for the available filter
predicates. If no filter is specified, all endpoints are included.

The 'activation' field controls progressive disclosure (default: 'AlwaysActivated').
-}
data OpenAPIServerDescription
    = OpenAPIServerDescription
    { openApiSpecUrl :: Text
    -- ^ URL to fetch the OpenAPI specification from (can be http/https or file://)
    , openApiBaseUrl :: Text
    -- ^ Base URL for API calls (can be different from spec URL)
    , openApiHeaders :: Maybe (Map Text Text)
    -- ^ Optional static headers to include in all requests
    , openApiToken :: Maybe Text
    -- ^ Optional Bearer token for authentication (legacy, prefer using 'secrets')
    , openApiFilter :: Maybe EndpointPredicate
    -- ^ Optional filter to restrict which endpoints are exposed as tools
    , openApiSecrets :: Maybe [Secret]
    -- ^ Optional list of secrets to resolve and include in requests
    , openApiActivation :: Maybe Activation
    -- ^ Optional activation mode (default: AlwaysActivated)
    }
    deriving (Show, Ord, Eq, Generic)

{- | Custom JSON options for OpenAPIServerDescription to use camelCase field names
(matching the existing Agent JSON style)
-}
openApiServerOptions :: Aeson.Options
openApiServerOptions =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = dropPrefix "openApi"
        , Aeson.omitNothingFields = True
        }
  where
    dropPrefix prefix str
        | take (length prefix) str == prefix = drop (length prefix) str
        | otherwise = str

instance ToJSON OpenAPIServerDescription where
    toJSON = Aeson.genericToJSON openApiServerOptions
    toEncoding = Aeson.genericToEncoding openApiServerOptions

instance FromJSON OpenAPIServerDescription where
    parseJSON = Aeson.genericParseJSON openApiServerOptions

{- | Reference to an OpenAPI server configuration stored on disk.

This allows the entire OpenAPI server configuration to be stored in a file
rather than embedded directly in the agent configuration. The file should
contain a JSON object matching the 'OpenAPIServerDescription' structure.

Example configuration:

@
{
  "tag": "OpenAPIServerOnDisk",
  "contents": "/path/to/openapi-config.json"
}
@

The referenced file should contain:

@
{
  "specUrl": "https://api.example.com/openapi.json",
  "baseUrl": "https://api.example.com",
  "headers": {"X-API-Version": "v1"},
  "token": "${API_TOKEN}",
  "filter": {"tag": "PathPrefix", "contents": "/api/v1"},
  "lifetime": "conversation",
  "activation": "always",
  "secrets": []
}
@
-}
newtype OpenAPIServerOnDisk = OpenAPIServerOnDisk
    { openApiConfigPath :: FilePath
    -- ^ Path to the JSON file containing the OpenAPI server configuration
    }
    deriving (Show, Ord, Eq, Generic)

instance ToJSON OpenAPIServerOnDisk where
    toJSON (OpenAPIServerOnDisk path) = Aeson.toJSON path

instance FromJSON OpenAPIServerOnDisk where
    parseJSON v = OpenAPIServerOnDisk <$> Aeson.parseJSON v

{- | Wrapper type for JSON serialization with tag.
Similar to McpServerDescription, this allows extensible toolbox types.
-}
data OpenAPIToolboxDescription
    = OpenAPIServer OpenAPIServerDescription
    | OpenAPIServerOnDiskDescription OpenAPIServerOnDisk
    deriving (Show, Ord, Eq, Generic)

instance ToJSON OpenAPIToolboxDescription where
    toJSON (OpenAPIServer val) =
        Aeson.object
            [ "tag" .= ("OpenAPIServer" :: Text)
            , "contents" .= val
            ]
    toJSON (OpenAPIServerOnDiskDescription val) =
        Aeson.object
            [ "tag" .= ("OpenAPIServerOnDisk" :: Text)
            , "contents" .= val
            ]

instance FromJSON OpenAPIToolboxDescription where
    parseJSON = Aeson.withObject "OpenAPIToolboxDescription" $ \v -> do
        tag <- v .: "tag"
        case (tag :: Text) of
            "OpenAPIServer" ->
                OpenAPIServer <$> v .: "contents"
            "OpenAPIServerOnDisk" ->
                OpenAPIServerOnDiskDescription <$> v .: "contents"
            _ -> fail "expecting 'OpenAPIServer' or 'OpenAPIServerOnDisk' tag"

-------------------------------------------------------------------------------
-- PostgREST Toolbox Configuration
-------------------------------------------------------------------------------

{- | Configuration for a PostgREST toolbox.

This describes a PostgREST API to load as a toolbox.
PostgREST exposes PostgreSQL databases as REST APIs and provides
an OpenAPI/Swagger spec at the root endpoint.

The toolbox will fetch the spec from 'postgrestSpecUrl' and make
all table endpoints available as tools to the agent, with special
handling for row filters, pagination, ordering, and column selection.

The spec URL can be either:
* An HTTP/HTTPS URL (e.g., "http://localhost:3000/")
* A file URL (e.g., "file:///path/to/postgrest-spec.json")

By default, only read-only methods (GET, HEAD, OPTIONS) are exposed
for safety. Use 'allowedMethods' to enable write operations.

Example configuration:

@
{
  "tag": "PostgRESTServer",
  "contents": {
    "specUrl": "http://localhost:3000/",
    "baseUrl": "http://localhost:3000",
    "headers": {"Accept-Profile": "myschema"},
    "token": "${POSTGREST_TOKEN}",
    "allowedMethods": ["GET", "POST", "PATCH"],
    "filter": {"tag": "PathPrefix", "contents": "/public"},
    "lifetime": "conversation",
    "activation": "always",
    "secrets": [
      {
        "source": {"tag": "FileSystem", "contents": "/run/secrets/jwt_token"},
        "decoder": {"tag": "Clear", "contents": true},
        "serializer": {"tag": "Header", "contents": ["Authorization", "Bearer {{secret}}"]}
      }
    ]
  }
}
@

The 'secrets' field allows flexible secret management with support for
multiple sources (environment variables, files, commands), encodings
(Base64, hex), and serialization locations (headers, query string).

The optional 'filter' field allows restricting which tables are
exposed as tools. See 'EndpointPredicate' for the available filter
predicates. If no filter is specified, all tables are included.

The 'activation' field controls progressive disclosure (default: 'AlwaysActivated').
-}
data PostgRESTServerDescription
    = PostgRESTServerDescription
    { postgrestSpecUrl :: Text
    {- ^ URL to fetch the PostgREST OpenAPI specification from
    (usually the root endpoint, e.g., "http://localhost:3000/")
    -}
    , postgrestBaseUrl :: Text
    -- ^ Base URL for API calls (can be different from spec URL)
    , postgrestHeaders :: Maybe (Map Text Text)
    {- ^ Optional static headers to include in all requests
    (e.g., for schema selection via Accept-Profile)
    -}
    , postgrestToken :: Maybe Text
    -- ^ Optional Bearer token for JWT authentication (legacy, prefer using 'secrets')
    , postgrestAllowedMethods :: Maybe [HttpMethod]
    {- ^ Optional list of HTTP methods to expose as tools.
    Default: read-only methods [GET, HEAD, OPTIONS] for safety.
    To enable full CRUD: ["GET", "HEAD", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"]
    -}
    , postgrestFilter :: Maybe EndpointPredicate
    -- ^ Optional filter to restrict which tables/endpoints are exposed as tools
    , postgrestSecrets :: Maybe [Secret]
    -- ^ Optional list of secrets to resolve and include in requests
    , postgrestActivation :: Maybe Activation
    -- ^ Optional activation mode (default: AlwaysActivated)
    }
    deriving (Show, Ord, Eq, Generic)

-- | Custom JSON options for PostgRESTServerDescription to use camelCase field names
postgrestServerOptions :: Aeson.Options
postgrestServerOptions =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = dropPrefix "postgrest"
        , Aeson.omitNothingFields = True
        }
  where
    dropPrefix prefix str
        | take (length prefix) str == prefix = drop (length prefix) str
        | otherwise = str

instance ToJSON PostgRESTServerDescription where
    toJSON = Aeson.genericToJSON postgrestServerOptions
    toEncoding = Aeson.genericToEncoding postgrestServerOptions

instance FromJSON PostgRESTServerDescription where
    parseJSON = Aeson.genericParseJSON postgrestServerOptions

{- | Reference to a PostgREST server configuration stored on disk.

This allows the entire PostgREST server configuration to be stored in a file
rather than embedded directly in the agent configuration. The file should
contain a JSON object matching the 'PostgRESTServerDescription' structure.

Example configuration:

@
{
  "tag": "PostgRESTServerOnDisk",
  "contents": "/path/to/postgrest-config.json"
}
@

The referenced file should contain:

@
{
  "specUrl": "http://localhost:3000/",
  "baseUrl": "http://localhost:3000",
  "headers": {"Accept-Profile": "myschema"},
  "token": "${POSTGREST_TOKEN}",
  "allowedMethods": ["GET", "POST", "PATCH"],
  "filter": {"tag": "Not", "contents": {"tag": "PathContains", "contents": "_internal"}},
  "lifetime": "conversation",
  "activation": "always",
  "secrets": []
}
@
-}
newtype PostgRESTServerOnDisk = PostgRESTServerOnDisk
    { postgrestConfigPath :: FilePath
    -- ^ Path to the JSON file containing the PostgREST server configuration
    }
    deriving (Show, Ord, Eq, Generic)

instance ToJSON PostgRESTServerOnDisk where
    toJSON (PostgRESTServerOnDisk path) = Aeson.toJSON path

instance FromJSON PostgRESTServerOnDisk where
    parseJSON v = PostgRESTServerOnDisk <$> Aeson.parseJSON v

{- | Wrapper type for JSON serialization with tag.
Similar to OpenAPIToolboxDescription, this allows extensible toolbox types.
-}
data PostgRESTToolboxDescription
    = PostgRESTServer PostgRESTServerDescription
    | PostgRESTServerOnDiskDescription PostgRESTServerOnDisk
    deriving (Show, Ord, Eq, Generic)

instance ToJSON PostgRESTToolboxDescription where
    toJSON (PostgRESTServer val) =
        Aeson.object
            [ "tag" .= ("PostgRESTServer" :: Text)
            , "contents" .= val
            ]
    toJSON (PostgRESTServerOnDiskDescription val) =
        Aeson.object
            [ "tag" .= ("PostgRESTServerOnDisk" :: Text)
            , "contents" .= val
            ]

instance FromJSON PostgRESTToolboxDescription where
    parseJSON = Aeson.withObject "PostgRESTToolboxDescription" $ \v -> do
        tag <- v .: "tag"
        case (tag :: Text) of
            "PostgRESTServer" ->
                PostgRESTServer <$> v .: "contents"
            "PostgRESTServerOnDisk" ->
                PostgRESTServerOnDiskDescription <$> v .: "contents"
            _ -> fail "expecting 'PostgRESTServer' or 'PostgRESTServerOnDisk' tag"

-------------------------------------------------------------------------------
-- Builtin Toolbox Configuration
-------------------------------------------------------------------------------

{- | Sharing scope for versioned SQLite databases.

Controls at what granularity the "current head" (latest written snapshot) is shared:
* 'SharingGlobal': One persistent chain shared across all conversations — never resets
* 'SharingConversation': One chain per conversation — resets to basePath each new session
* 'SharingTurn': One chain per turn — resets to basePath each new turn
* 'SharingToolCall': Fully isolated per tool call — always starts from basePath, no chaining
-}
data SqliteSharing
    = SharingGlobal
    | SharingConversation
    | SharingTurn
    | SharingToolCall
    deriving (Show, Eq, Ord, Generic)

instance ToJSON SqliteSharing where
    toJSON SharingGlobal       = Aeson.String "global"
    toJSON SharingConversation = Aeson.String "conversation"
    toJSON SharingTurn         = Aeson.String "turn"
    toJSON SharingToolCall     = Aeson.String "tool-call"

instance FromJSON SqliteSharing where
    parseJSON = Aeson.withText "SqliteSharing" $ \txt ->
        case txt of
            "global"       -> return SharingGlobal
            "conversation" -> return SharingConversation
            "turn"         -> return SharingTurn
            "tool-call"    -> return SharingToolCall
            other -> fail $ "Invalid SqliteSharing: " ++ Text.unpack other ++ ". Expected 'global', 'conversation', 'turn', or 'tool-call'."

{- | Version handle for restoration (serializable to JSON).

A version handle uniquely identifies a specific database state that
can be restored in subsequent tool calls. This enables LLMs to:
* Branch from specific database states
* Rollback to previous states
* Reference immutable snapshots
-}
data SqliteVersionHandle = SqliteVersionHandle
    { vhSessionId :: !Text
    -- ^ Session UUID as text
    , vhTurnId :: !Text
    -- ^ Turn UUID as text
    , vhToolCallIndex :: !Int
    -- ^ Index within turn (0, 1, 2...)
    , vhToolboxName :: !Text
    -- ^ Toolbox identifier
    , vhTimestamp :: !Text
    -- ^ ISO8601 timestamp for debugging
    }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON SqliteVersionHandle where
    toJSON h = Aeson.object
        [ "session_id"       .= vhSessionId h
        , "turn_id"          .= vhTurnId h
        , "tool_call_index"  .= vhToolCallIndex h
        , "toolbox_name"     .= vhToolboxName h
        , "timestamp"        .= vhTimestamp h
        ]

instance FromJSON SqliteVersionHandle where
    parseJSON = Aeson.withObject "SqliteVersionHandle" $ \v ->
        SqliteVersionHandle
            <$> v .:  "session_id"
            <*> v .:  "turn_id"
            <*> v .:  "tool_call_index"
            <*> v .:  "toolbox_name"
            <*> (v .:? "timestamp" Aeson..!= "")

{- | Enhanced SQLite versioning configuration.

Replaces the old 'SqliteAccessMode' with hierarchical versioning support.

* 'SqliteReadOnly': Direct read-only access, no versioning - only SELECT queries allowed
* 'SqliteReadWrite': Direct read-write access, no versioning - all queries allowed
* 'SqliteVersioned': Versioned with hierarchical storage and restoration capabilities
-}
data SqliteVersioningConfig
    = -- | Direct read-only access - only SELECT queries allowed
      SqliteReadOnly !FilePath
    | -- | Direct read-write access - all queries allowed
      SqliteReadWrite !FilePath
    | SqliteVersioned
        { vSharing :: !SqliteSharing
        -- ^ Sharing scope — determines when the write chain resets to basePath
        , vStorageRoot :: !(Maybe FilePath)
        -- ^ Optional custom storage root. Default: ~/.agents/sqlite-versions (or XDG equivalent)
        , vBasePath :: !FilePath
        -- ^ Template/source database path - the original database to copy from
        }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON SqliteVersioningConfig where
    toJSON (SqliteReadOnly path) = Aeson.object ["tag" .= ("SqliteReadOnly" :: Text), "path" .= path]
    toJSON (SqliteReadWrite path) = Aeson.object ["tag" .= ("SqliteReadWrite" :: Text), "path" .= path]
    toJSON (SqliteVersioned sharing mRoot base) =
        Aeson.object
            [ "tag" .= ("SqliteVersioned" :: Text)
            , "shared" .= sharing
            , "storageRoot" .= mRoot
            , "basePath" .= base
            ]

instance FromJSON SqliteVersioningConfig where
    parseJSON = Aeson.withObject "SqliteVersioningConfig" $ \v -> do
        tag <- v .: "tag"
        case (tag :: Text) of
            "SqliteReadOnly"     -> SqliteReadOnly  <$> v .: "path"
            "SqliteReadWrite"    -> SqliteReadWrite <$> v .: "path"
            "SqliteNoVersioning" -> SqliteReadWrite <$> v .: "path"
            "SqliteVersioned"    ->
                SqliteVersioned
                    <$> v .: "shared"
                    <*> v .:? "storageRoot"
                    <*> v .: "basePath"
            other -> fail $ "Unknown SqliteVersioningConfig tag: " ++ Text.unpack other

{- | Configuration for a SQLite builtin toolbox.

This describes a SQLite database to load as a builtin toolbox.
The toolbox provides tools for querying and optionally modifying
the SQLite database with optional hierarchical versioning.

Example configuration (versioned with conversation lifetime):

@
{
  "tag": "SqliteToolbox",
  "contents": {
    "name": "memory",
    "description": "a set of memories",
    "versioning": {
      "tag": "SqliteVersioned",
      "lifetime": "conversation",
      "storageRoot": null,
      "basePath": "/path/to/memories.sqlite"
    },
    "activation": "always"
  }
}
@

Example configuration (read-only direct access):

@
{
  "tag": "SqliteToolbox",
  "contents": {
    "name": "analytics",
    "description": "Analytics database",
    "versioning": {
      "tag": "SqliteReadOnly",
      "path": "/path/to/analytics.sqlite"
    },
    "activation": "always"
  }
}
@

Migration from old 'access' field:
- "access": "read-only" -> SqliteReadOnly with path
- "access": "read-write" -> SqliteReadWrite with path
- "access": "snapshot" -> SqliteVersioned with LifetimeConversation

The 'activation' field controls progressive disclosure (default: 'AlwaysActivated').
-}
data SqliteToolboxDescription = SqliteToolboxDescription
    { sqliteToolboxName :: !Text
    -- ^ Unique name for this toolbox instance (used as tool prefix)
    , sqliteToolboxDescription :: !Text
    -- ^ Human-readable description of the database contents/purpose
    , sqliteToolboxVersioning :: !SqliteVersioningConfig
    -- ^ Versioning configuration - replaces the old 'sqliteToolboxAccess' field
    , sqliteToolboxActivation :: !(Maybe Activation)
    -- ^ Optional activation mode (default: AlwaysActivated)
    }
    deriving (Show, Eq, Ord, Generic)

-- | Custom JSON options for SqliteToolboxDescription to use camelCase field names
sqliteToolboxOptions :: Aeson.Options
sqliteToolboxOptions =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = dropPrefix "sqliteToolbox"
        , Aeson.omitNothingFields = True
        }
  where
    dropPrefix prefix str
        | take (length prefix) str == prefix = drop (length prefix) str
        | otherwise = str

instance ToJSON SqliteToolboxDescription where
    toJSON = Aeson.genericToJSON sqliteToolboxOptions
    toEncoding = Aeson.genericToEncoding sqliteToolboxOptions

instance FromJSON SqliteToolboxDescription where
    parseJSON = Aeson.genericParseJSON sqliteToolboxOptions

-------------------------------------------------------------------------------
-- System Toolbox Configuration
-------------------------------------------------------------------------------

{- | Enumeration of available system information capabilities.

Each capability represents a category of system information that
can be exposed to agents through the system toolbox.
-}
data SystemToolCapability
    = -- | Current date/time information
      SystemToolDate
    | -- | OS name and version
      SystemToolOperatingSystem
    | -- | Environment variables (with optional filtering)
      SystemToolEnvVars
    | -- | Current user information
      SystemToolRunningUser
    | -- | Machine hostname
      SystemToolHostname
    | -- | Current working directory
      SystemToolWorkingDirectory
    | -- | Process ID and other process information
      SystemToolProcessInfo
    | -- | System uptime
      SystemToolUptime
    | -- | Attach a file to the conversation
      SystemToolAttachFile
    | -- | List accessible sessions
      SystemToolListSessions
    | -- | Full-text search across sessions
      SystemToolSearchSessions
    | -- | Read session content
      SystemToolReadSession
    | -- | Get session statistics
      SystemToolGetSessionStats
    deriving (Show, Ord, Eq, Generic)

-- | Serialize SystemToolCapability as kebab-case strings.
instance ToJSON SystemToolCapability where
    toJSON SystemToolDate = Aeson.String "date"
    toJSON SystemToolOperatingSystem = Aeson.String "operating-system"
    toJSON SystemToolEnvVars = Aeson.String "env-vars"
    toJSON SystemToolRunningUser = Aeson.String "running-user"
    toJSON SystemToolHostname = Aeson.String "hostname"
    toJSON SystemToolWorkingDirectory = Aeson.String "working-directory"
    toJSON SystemToolProcessInfo = Aeson.String "process-info"
    toJSON SystemToolUptime = Aeson.String "uptime"
    toJSON SystemToolAttachFile = Aeson.String "attach-file"
    toJSON SystemToolListSessions = Aeson.String "list-sessions"
    toJSON SystemToolSearchSessions = Aeson.String "search-sessions"
    toJSON SystemToolReadSession = Aeson.String "read-session"
    toJSON SystemToolGetSessionStats = Aeson.String "get-session-stats"

-- | Parse SystemToolCapability from kebab-case strings.
instance FromJSON SystemToolCapability where
    parseJSON = Aeson.withText "SystemToolCapability" $ \txt ->
        case txt of
            "date" -> return SystemToolDate
            "operating-system" -> return SystemToolOperatingSystem
            "env-vars" -> return SystemToolEnvVars
            "running-user" -> return SystemToolRunningUser
            "hostname" -> return SystemToolHostname
            "working-directory" -> return SystemToolWorkingDirectory
            "process-info" -> return SystemToolProcessInfo
            "uptime" -> return SystemToolUptime
            "attach-file" -> return SystemToolAttachFile
            "list-sessions" -> return SystemToolListSessions
            "search-sessions" -> return SystemToolSearchSessions
            "read-session" -> return SystemToolReadSession
            "get-session-stats" -> return SystemToolGetSessionStats
            other -> fail $ "Invalid SystemToolCapability: " ++ Text.unpack other ++ ". Expected one of: date, operating-system, env-vars, running-user, hostname, working-directory, process-info, uptime, attach-file, list-sessions, search-sessions, read-session, get-session-stats."

{- | Scope of accessible sessions for session introspection capabilities.

Controls which sessions can be accessed when using session introspection
capabilities like 'list-sessions', 'search-sessions', 'read-session', etc.
-}
data SessionIntrospectionScope
    = -- | Can only see parent sessions (ancestors via forkedFromSessionId)
      ScopeParentsOnly
    | -- | Can only see child sessions (descendants)
      ScopeChildrenOnly
    | -- | Parents + current + children (default scope)
      ScopeSubtree
    | -- | All sessions (requires explicit opt-in)
      ScopeAll
    deriving (Show, Ord, Eq, Generic)

-- | Serialize SessionIntrospectionScope as kebab-case strings.
instance ToJSON SessionIntrospectionScope where
    toJSON ScopeParentsOnly = Aeson.String "parents-only"
    toJSON ScopeChildrenOnly = Aeson.String "children-only"
    toJSON ScopeSubtree = Aeson.String "subtree"
    toJSON ScopeAll = Aeson.String "all"

-- | Parse SessionIntrospectionScope from kebab-case strings.
instance FromJSON SessionIntrospectionScope where
    parseJSON = Aeson.withText "SessionIntrospectionScope" $ \txt ->
        case txt of
            "parents-only" -> return ScopeParentsOnly
            "children-only" -> return ScopeChildrenOnly
            "subtree" -> return ScopeSubtree
            "all" -> return ScopeAll
            other -> fail $ "Invalid SessionIntrospectionScope: " ++ Text.unpack other ++ ". Expected one of: parents-only, children-only, subtree, all."

{- | Configuration for the system toolbox.

This describes which system information capabilities should be
made available to an agent. The system toolbox provides contextual
information about the running environment.

Example configuration:

@
{
  "tag": "SystemToolbox",
  "contents": {
    "name": "system",
    "description": "System information and context",
    "capabilities": ["date", "operating-system", "running-user", "hostname"],
    "envVarFilter": null,
    "lifetime": "conversation",
    "activation": "always"
  }
}
@

The 'envVarFilter' field is an optional regex/pattern to filter
environment variables when the 'env-vars' capability is enabled.
If not specified, all environment variables are exposed.

The 'activation' field controls progressive disclosure (default: 'AlwaysActivated').

Session introspection configuration:
The optional session introspection fields control access to session history:
- 'sessionIntrospectionScope': Which sessions are accessible (default: ScopeSubtree)
- 'sessionIntrospectionMaxResults': Max sessions to return (default: 50)
- 'sessionIntrospectionIncludeToolOutputs': Include tool outputs in read operations (default: True)
-}
data SystemToolboxDescription
    = SystemToolboxDescription
    { systemToolboxName :: Text
    -- ^ Unique name for this toolbox instance (used as tool prefix)
    , systemToolboxDescription :: Text
    -- ^ Human-readable description of the toolbox purpose
    , systemToolboxCapabilities :: [SystemToolCapability]
    -- ^ List of system information capabilities to expose
    , systemToolboxEnvVarFilter :: Maybe Text
    -- ^ Optional regex/pattern to filter environment variables
    , systemToolboxActivation :: Maybe Activation
    -- ^ Optional activation mode (default: AlwaysActivated)
    , systemToolboxSessionIntrospectionScope :: Maybe SessionIntrospectionScope
    -- ^ Scope of accessible sessions (default: ScopeSubtree)
    , systemToolboxSessionIntrospectionMaxResults :: Maybe Int
    -- ^ Max sessions to return in list operations (default: 50)
    , systemToolboxSessionIntrospectionIncludeToolOutputs :: Maybe Bool
    -- ^ Whether to include tool outputs in read operations (default: True)
    , systemToolboxFileSandbox :: Maybe FileSandboxConfig
    -- ^ File sandbox for attach-file capability (default: deny all)
    }
    deriving (Show, Ord, Eq, Generic)

-- | Custom JSON options for SystemToolboxDescription to use camelCase field names
systemToolboxOptions :: Aeson.Options
systemToolboxOptions =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = dropPrefix "systemToolbox"
        , Aeson.omitNothingFields = True
        }
  where
    dropPrefix prefix str
        | take (length prefix) str == prefix = drop (length prefix) str
        | otherwise = str

instance ToJSON SystemToolboxDescription where
    toJSON = Aeson.genericToJSON systemToolboxOptions
    toEncoding = Aeson.genericToEncoding systemToolboxOptions

instance FromJSON SystemToolboxDescription where
    parseJSON = Aeson.genericParseJSON systemToolboxOptions

-- | Default system file sandbox (denies all access).
defaultSystemFileSandbox :: FileSandboxConfig
defaultSystemFileSandbox =
    FileSandboxConfig
        { fsbPredicate = AlwaysDeny
        , fsbMaxFileSize = Just (50 * 1024 * 1024) -- 50MB default
        , fsbName = Just "system-attach-sandbox"
        }

-------------------------------------------------------------------------------
-- Lua Toolbox Configuration
-------------------------------------------------------------------------------

{- | Configuration for a Lua builtin toolbox.

This describes a sandboxed Lua interpreter that can orchestrate other tools
through the tool portal mechanism. Each tool call creates a fresh, isolated
Lua state that is destroyed immediately after execution.

== Calling Convention

The tool accepts a JSON object with:

* @script@ (string, required): Lua source code to execute
* @timeout@ (integer, optional): Override timeout in seconds

The script's return value(s) are automatically converted to JSON:

> -- Single return value
> return 42  -- Returns: {"values": [42], "executionTime": 0.001}
>
> -- Multiple return values
> return "status", {count = 5}  -- Returns: {"values": ["status", {"count": 5}], ...}
>
> -- Table/object return
> return {name = "Alice", tags = {"a", "b"}}

== Available Modules

Six standard library modules are pre-registered and available via @require()@:

* __json__: JSON encoding/decoding
    * @json.encode(value)@ - Encode to JSON string
    * @json.encode_pretty(value)@ - Pretty-print JSON
    * @json.decode(str)@ - Decode JSON to Lua value

* __text__: UTF-8 string utilities
    * @text.split(str, delim)@, @text.find(str, pattern)@, @text.gsub(str, pat, repl)@
    * @text.trim(str)@, @text.startswith(str, prefix)@, @text.endswith(str, suffix)@
    * @text.lower(str)@, @text.upper(str)@, @text.len(str)@, @text.sub(str, start, end)@

* __time__: Time functions
    * @time.now()@ - Current timestamp (seconds since epoch)
    * @time.sleep(seconds)@ - Sleep for specified duration
    * @time.format(timestamp, format)@ - Format timestamp (e.g., "%Y-%m-%d %H:%M:%S")
    * @time.diff(t1, t2)@ - Calculate difference in seconds

* __fs__: Sandboxed filesystem (path-restricted by fileSandbox config)
    * @fs.read(path)@, @fs.write(path, content)@, @fs.exists(path)@
    * @fs.list(path)@, @fs.mkdir(path)@, @fs.isdir(path)@, @fs.isfile(path)@
    * @fs.patch(path, search, replace)@ - Search and replace in file

* __http__: HTTP requests (host-whitelisted by allowedHosts config)
    * @http.get(url, [options])@, @http.post(url, body, [options])@
    * @http.request({method, url, body, headers})@ - Generic request
    * Returns: @{status, headers, body}@ table

* __tools__: Tool portal for calling other tools (tool-whitelisted by allowedTools config)
    * @tools.list()@ - Returns array of available tool names
    * @tools.call(tool_name, args_table)@ - Call a tool, returns result table

== Example Script

> local json = require("json")
> local tools = require("tools")
>
> local result = tools.call("bash_read_file", {filepath = "/path/to/file"})
> if result.status == "ok" then
>     local data = json.decode(result.result_txt)
>     return {content = data.content, timestamp = os.time()}
> else
>     return {error = result.result_txt}
> end

Example configuration:

@
{
  "tag": "LuaToolbox",
  "contents": {
    "name": "lua",
    "description": "Sandboxed Lua interpreter for tool orchestration",
    "maxMemoryMB": 256,
    "maxExecutionTimeSeconds": 300,
    "allowedTools": ["bash", "sqlite", "io"],
    "allowedHosts": ["localhost", "127.0.0.1"],
    "fileSandbox": {
      "predicate": {"tag": "PathPrefix", "contents": "./repro"},
      "maxFileSize": 10485760,
      "name": "lua-sandbox"
    },
    "lifetime": "conversation",
    "activation": "always"
  }
}
@

The 'activation' field controls progressive disclosure (default: 'AlwaysActivated').
-}
data LuaToolboxDescription = LuaToolboxDescription
    { luaToolboxName :: Text
    -- ^ Unique name for this toolbox instance (used as tool prefix)
    , luaToolboxDescription :: Text
    -- ^ Human-readable description of the toolbox purpose
    , luaToolboxMaxMemoryMB :: Int
    -- ^ Maximum Lua heap memory in megabytes
    , luaToolboxMaxExecutionTimeSeconds :: Int
    -- ^ Maximum script execution time in seconds
    , luaToolboxAllowedTools :: [Text]
    -- ^ Whitelist of tool names that Lua scripts can call via the portal
    , luaToolboxAllowedHosts :: [Text]
    -- ^ Whitelist of network hosts accessible to Lua HTTP module
    , luaToolboxActivation :: Maybe Activation
    -- ^ Optional activation mode (default: AlwaysActivated)
    , luaToolboxFileSandbox :: Maybe FileSandboxConfig
    -- ^ File sandbox configuration for Lua file system operations
    }
    deriving (Show, Ord, Eq, Generic)

-- | Custom JSON options for LuaToolboxDescription to use camelCase field names
luaToolboxOptions :: Aeson.Options
luaToolboxOptions =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = dropPrefix "luaToolbox"
        , Aeson.omitNothingFields = True
        }
  where
    dropPrefix prefix str
        | take (length prefix) str == prefix = drop (length prefix) str
        | otherwise = str

instance ToJSON LuaToolboxDescription where
    toJSON = Aeson.genericToJSON luaToolboxOptions
    toEncoding = Aeson.genericToEncoding luaToolboxOptions

instance FromJSON LuaToolboxDescription where
    parseJSON = Aeson.genericParseJSON luaToolboxOptions

-------------------------------------------------------------------------------
-- Developer Toolbox Configuration
-------------------------------------------------------------------------------

{- | Enumeration of available developer tools capabilities.

Each capability represents a tool that can help developers write
and validate agents and tools.
-}
data DeveloperToolCapability
    = -- | Validate a tool script
      DevToolValidateTool
    | -- | Generate agent scaffolding
      DevToolScaffoldAgent
    | -- | Generate tool scaffolding
      DevToolScaffoldTool
    | -- | Show bash-tools specification
      DevToolShowSpec
    | -- | Validate an agent JSON file
      DevToolValidateAgent
    | -- | Create a new agent configuration
      DevToolCreateAgent
    | -- | Create a new tool script
      DevToolCreateTool
    | -- | Read specific line ranges from a file
      DevToolReadFileRange
    | -- | Replace line ranges in a file with new content
      DevToolWriteFileRange
    | -- | Apply a unified diff patch to a file
      DevToolPatchFile
    | -- | Get detailed help for developer tools
      DevToolHelp
    | -- | Enable snapshot functionality for file edits
      DevToolSnapshot
    | -- | Restore a file from a snapshot
      DevToolRestoreFile
    | -- | List directory contents with metadata
      DevToolListDirectory
    | -- | Traverse directory tree recursively
      DevToolTraverseDirectory
    deriving (Show, Ord, Eq, Generic)
-- | Serialize DeveloperToolCapability as kebab-case strings.
instance ToJSON DeveloperToolCapability where
    toJSON DevToolValidateTool = Aeson.String "validate-tool"
    toJSON DevToolScaffoldAgent = Aeson.String "scaffold-agent"
    toJSON DevToolScaffoldTool = Aeson.String "scaffold-tool"
    toJSON DevToolShowSpec = Aeson.String "show-spec"
    toJSON DevToolValidateAgent = Aeson.String "validate-agent"
    toJSON DevToolCreateAgent = Aeson.String "create-agent"
    toJSON DevToolCreateTool = Aeson.String "create-tool"
    toJSON DevToolReadFileRange = Aeson.String "read-file-range"
    toJSON DevToolWriteFileRange = Aeson.String "write-file-range"
    toJSON DevToolPatchFile = Aeson.String "patch-file"
    toJSON DevToolHelp = Aeson.String "help"
    toJSON DevToolSnapshot = Aeson.String "snapshot"
    toJSON DevToolRestoreFile = Aeson.String "restore-file"
    toJSON DevToolListDirectory = Aeson.String "list-directory"
    toJSON DevToolTraverseDirectory = Aeson.String "traverse-directory"

-- | Parse DeveloperToolCapability from kebab-case strings.
instance FromJSON DeveloperToolCapability where
    parseJSON = Aeson.withText "DeveloperToolCapability" $ \txt ->
        case txt of
            "validate-tool" -> return DevToolValidateTool
            "scaffold-agent" -> return DevToolScaffoldAgent
            "scaffold-tool" -> return DevToolScaffoldTool
            "show-spec" -> return DevToolShowSpec
            "validate-agent" -> return DevToolValidateAgent
            "create-agent" -> return DevToolCreateAgent
            "create-tool" -> return DevToolCreateTool
            "read-file-range" -> return DevToolReadFileRange
            "write-file-range" -> return DevToolWriteFileRange
            "patch-file" -> return DevToolPatchFile
            "help" -> return DevToolHelp
            "snapshot" -> return DevToolSnapshot
            "restore-file" -> return DevToolRestoreFile
            "list-directory" -> return DevToolListDirectory
            "traverse-directory" -> return DevToolTraverseDirectory
            other -> fail $ "Invalid DeveloperToolCapability: " ++ Text.unpack other ++ ". Expected one of: validate-tool, scaffold-agent, scaffold-tool, show-spec, validate-agent, create-agent, create-tool, read-file-range, write-file-range, patch-file, help, snapshot, restore-file, list-directory, traverse-directory."
{- | Configuration for the developer toolbox.

This describes which developer tools should be made available to an agent.
These tools help with writing and validating agents and tools.

Example configuration:

@
{
  "tag": "DeveloperToolbox",
  "contents": {
    "name": "developer",
    "description": "Tools for developing agents and tools",
    "capabilities": ["validate-tool", "scaffold-agent", "scaffold-tool", "read-file-range", "write-file-range", "patch-file", "help", "snapshot", "restore-file"],
    "lifetime": "conversation",
    "activation": "always"
  }
}
@

The 'activation' field controls progressive disclosure (default: 'AlwaysActivated').
-}
data DeveloperToolboxDescription
    = DeveloperToolboxDescription
    { developerToolboxName :: Text
    -- ^ Unique name for this toolbox instance (used as tool prefix)
    , developerToolboxDescription :: Text
    -- ^ Human-readable description of the toolbox purpose
    , developerToolboxCapabilities :: [DeveloperToolCapability]
    -- ^ List of developer tool capabilities to expose
    , developerToolboxActivation :: Maybe Activation
    -- ^ Optional activation mode (default: AlwaysActivated)
    , developerToolboxFileSandbox :: Maybe FileSandboxConfig
    {- ^ File sandbox for read-file-range, write-file-range, patch-file
    Default: deny all (secure by default)
    -}
    }
    deriving (Show, Ord, Eq, Generic)

-- | Custom JSON options for DeveloperToolboxDescription to use camelCase field names
developerToolboxOptions :: Aeson.Options
developerToolboxOptions =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = dropPrefix "developerToolbox"
        , Aeson.omitNothingFields = True
        }
  where
    dropPrefix prefix str
        | take (length prefix) str == prefix = drop (length prefix) str
        | otherwise = str

instance ToJSON DeveloperToolboxDescription where
    toJSON = Aeson.genericToJSON developerToolboxOptions
    toEncoding = Aeson.genericToEncoding developerToolboxOptions

instance FromJSON DeveloperToolboxDescription where
    parseJSON = Aeson.genericParseJSON developerToolboxOptions

-- | Default developer file sandbox (denies all access).
defaultDeveloperFileSandbox :: FileSandboxConfig
defaultDeveloperFileSandbox =
    FileSandboxConfig
        { fsbPredicate = AlwaysDeny
        , fsbMaxFileSize = Nothing
        , fsbName = Just "developer-sandbox"
        }

{- | Wrapper type for builtin toolbox descriptions with tag-based JSON serialization.

This is a tagged union type that allows extensible builtin toolbox types.
New builtin toolbox types can be added in the future without breaking
existing configurations.

Example configuration:

@
{
  "builtinToolboxes": [
    {"tag": "SqliteToolbox", "contents": {"name": "memory", "description": "a set of memories", "path": "/path/to/memories.sqlite", "access": "read-write"}},
    {"tag": "SqliteToolbox", "contents": {"name": "guidelines", "description": "a set of guidelines", "path": "/path/to/guidelines.sqlite", "access": "read-only"}},
    {"tag": "SystemToolbox", "contents": {"name": "system", "description": "System context", "capabilities": ["date", "hostname"], "envVarFilter": null}},
    {"tag": "DeveloperToolbox", "contents": {"name": "developer", "description": "Development tools", "capabilities": ["validate-tool", "scaffold-agent", "read-file-range", "write-file-range", "patch-file", "help", "snapshot", "restore-file"]}},
    {"tag": "LuaToolbox", "contents": {"name": "lua", "description": "Lua orchestration", "maxMemoryMB": 256, "maxExecutionTimeSeconds": 300, "allowedTools": ["bash"], "fileSandbox": {"predicate": {"tag": "AlwaysAllow"}, "maxFileSize": 10485760, "name": "lua-sandbox"}}}
  ]
}
@
-}
data BuiltinToolboxDescription
    = SqliteToolbox SqliteToolboxDescription
    | SystemToolbox SystemToolboxDescription
    | DeveloperToolbox DeveloperToolboxDescription
    | LuaToolbox LuaToolboxDescription
    deriving (Show, Ord, Eq, Generic)

instance ToJSON BuiltinToolboxDescription where
    toJSON (SqliteToolbox val) =
        Aeson.object
            [ "tag" .= ("SqliteToolbox" :: Text)
            , "contents" .= val
            ]
    toJSON (SystemToolbox val) =
        Aeson.object
            [ "tag" .= ("SystemToolbox" :: Text)
            , "contents" .= val
            ]
    toJSON (DeveloperToolbox val) =
        Aeson.object
            [ "tag" .= ("DeveloperToolbox" :: Text)
            , "contents" .= val
            ]
    toJSON (LuaToolbox val) =
        Aeson.object
            [ "tag" .= ("LuaToolbox" :: Text)
            , "contents" .= val
            ]

instance FromJSON BuiltinToolboxDescription where
    parseJSON = Aeson.withObject "BuiltinToolboxDescription" $ \v -> do
        tag <- v .: "tag"
        case (tag :: Text) of
            "SqliteToolbox" ->
                SqliteToolbox <$> v .: "contents"
            "SystemToolbox" ->
                SystemToolbox <$> v .: "contents"
            "DeveloperToolbox" ->
                DeveloperToolbox <$> v .: "contents"
            "LuaToolbox" ->
                LuaToolbox <$> v .: "contents"
            _ -> fail "expecting 'SqliteToolbox', 'SystemToolbox', 'LuaToolbox', or 'DeveloperToolbox' tag"

-------------------------------------------------------------------------------
-- Skills Toolbox Configuration
-------------------------------------------------------------------------------

{- | Configuration for skills toolboxes per agentskills.io specification.

Skills provide procedural knowledge and executable capabilities via
progressive disclosure. The agent configuration specifies:
- skillSources: Where to load skills from (local directories, git repos)
- autoEnableSkills: Which skills to enable automatically at session start

Example configuration:

@
{
  "skillSources": [
    {"tag": "SkillDirectory", "contents": "./skills"},
    {"tag": "SkillGitRepo", "contents": {"url": "https://github.com/example/shared-skills.git"}}
  ],
  "autoEnableSkills": ["pdf-processing", "code-review"]
}
@

Progressive disclosure:
- Initially, only metadata tools are visible (skill_describe_{name}, skill_enable_{name})
- After skill_enable_{name} is called, script tools become available
- Tool availability is computed as a pure function of Session history

Skills now use the generic activation infrastructure internally.
Each skill maps to a toolgroup with the same name as the skill.
-}

-------------------------------------------------------------------------------
-- MCP Server Configuration
-------------------------------------------------------------------------------

{- | Configuration for an MCP server.

MCP servers provide tools via the Model Context Protocol.
They run as external processes and communicate via JSON-RPC.

Example configuration:

@
{
  "tag": "McpSimpleBinary",
  "contents": {
    "name": "filesystem",
    "executable": "/usr/bin/mcp-filesystem",
    "args": ["--readonly"],
    "lifetime": "conversation",
    "activation": {"tag": "first-n-steps", "steps": 5, "sticky": "sticky-if-used"}
  }
}
@
-}
data McpSimpleBinaryConfiguration
    = McpSimpleBinaryConfiguration
    { name :: Text
    -- ^ Name for this MCP server instance
    , executable :: FilePath
    -- ^ Path to the MCP server executable
    , args :: [Text]
    -- ^ Command-line arguments for the executable
    , mcpActivation :: Maybe Activation
    -- ^ Optional activation mode (default: AlwaysActivated)
    }
    deriving (Show, Ord, Eq, Generic)

-- | Custom JSON options for McpSimpleBinaryConfiguration
mcpSimpleBinaryOptions :: Aeson.Options
mcpSimpleBinaryOptions =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = dropPrefix "mcp"
        , Aeson.omitNothingFields = True
        }
  where
    dropPrefix prefix str
        | take (length prefix) str == prefix = drop (length prefix) str
        | otherwise = str

instance ToJSON McpSimpleBinaryConfiguration where
    toJSON = Aeson.genericToJSON mcpSimpleBinaryOptions
    toEncoding = Aeson.genericToEncoding mcpSimpleBinaryOptions

instance FromJSON McpSimpleBinaryConfiguration where
    parseJSON = Aeson.genericParseJSON mcpSimpleBinaryOptions

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
-- Agent Definition
-------------------------------------------------------------------------------

{- | Agent definition.

An agent can load tools from multiple sources:

1. The legacy @tools@ field (optional) - a single directory of bash tools.
2. The new @bashToolboxes@ field (optional) - a list of bash tool sources
   including directories with optional filters and single executable tools.
3. MCP servers from @mcpServers@.
4. OpenAPI toolboxes from @openApiToolboxes@.
5. PostgREST toolboxes from @postgrestToolboxes@.
6. Builtin toolboxes (SQLite, System, Developer, Lua) from @builtinToolboxes@.

If both @tools@ and @bashToolboxes@ are specified, both will be used.
This maintains backward compatibility while allowing gradual migration.

Example configuration:

@
{
  "slug": "my-agent",
  "apiKeyId": "openai",
  "flavor": "openai",
  "modelUrl": "https://api.openai.com/v1",
  "modelName": "gpt-4",
  "announce": "A helpful assistant",
  "systemPrompt": ["You are a helpful assistant."],
  "tools": "tools",
  "bashToolboxes": [
    {"tag": "FileSystemDirectory", "contents": {"path": "./extra-tools"}},
    {"tag": "SingleTool", "contents": {"path": "/path/to/special-tool.sh"}}
  ],
  "mcpServers": [...],
  "openApiToolboxes": [...],
  "builtinToolboxes": [...]
}
@

Note: Relative paths in bashToolboxes are resolved relative to the execution's
current working directory.
-}
data Agent
    = Agent
    { slug :: Text
    , apiKeyId :: Text
    , flavor :: Text
    , modelUrl :: Text
    , modelName :: Text
    , announce :: Text
    , systemPrompt :: [Text]
    , toolDirectory :: Maybe FilePath
    -- ^ Legacy single tool directory (optional for backward compatibility)
    , bashToolboxes :: Maybe [BashToolboxDescription]
    -- ^ New multiple bash tool sources
    , mcpServers :: Maybe [McpServerDescription]
    , openApiToolboxes :: Maybe [OpenAPIToolboxDescription]
    , postgrestToolboxes :: Maybe [PostgRESTToolboxDescription]
    , builtinToolboxes :: Maybe [BuiltinToolboxDescription]
    , extraAgents :: Maybe [ExtraAgentRef]
    , skillSources :: Maybe [SkillSource]
    -- ^ Sources to load skills from (directories, git repos)
    , autoEnableSkills :: Maybe [SkillName]
    -- ^ Skills to enable automatically at session start
    }
    deriving (Show, Eq, Generic)

-- | Custom JSON options for Agent - uses camelCase to match existing format
agentOptions :: Aeson.Options
agentOptions =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = id
        , Aeson.omitNothingFields = True
        }

instance ToJSON Agent where
    toJSON = Aeson.genericToJSON agentOptions
    toEncoding = Aeson.genericToEncoding agentOptions

instance FromJSON Agent where
    parseJSON = Aeson.genericParseJSON agentOptions

instance Ord Agent where
    compare a1 a2 = compare (slug a1) (slug a2)

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
