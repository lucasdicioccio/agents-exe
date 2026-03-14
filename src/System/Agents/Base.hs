{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module System.Agents.Base where

import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)

import System.Agents.Tools.EndpointPredicate (EndpointPredicate)
import System.Agents.Tools.PostgREST.Types (HttpMethod (..))

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
    "filter": {"tag": "PathPrefix", "contents": "/api/v1"}
  }
}
@

The optional 'filter' field allows restricting which endpoints are
exposed as tools. See 'EndpointPredicate' for the available filter
predicates. If no filter is specified, all endpoints are included.
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
    -- ^ Optional Bearer token for authentication
    , openApiFilter :: Maybe EndpointPredicate
    -- ^ Optional filter to restrict which endpoints are exposed as tools
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
  "filter": {"tag": "PathPrefix", "contents": "/api/v1"}
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
    "filter": {"tag": "PathPrefix", "contents": "/public"}
  }
}
@

The optional 'filter' field allows restricting which tables are
exposed as tools. See 'EndpointPredicate' for the available filter
predicates. If no filter is specified, all tables are included.
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
    -- ^ Optional Bearer token for JWT authentication
    , postgrestAllowedMethods :: Maybe [HttpMethod]
    {- ^ Optional list of HTTP methods to expose as tools.
    Default: read-only methods [GET, HEAD, OPTIONS] for safety.
    To enable full CRUD: ["GET", "HEAD", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"]
    -}
    , postgrestFilter :: Maybe EndpointPredicate
    -- ^ Optional filter to restrict which tables/endpoints are exposed as tools
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
  "filter": {"tag": "Not", "contents": {"tag": "PathContains", "contents": "_internal"}}
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

{- | Access mode for SQLite databases.

Controls whether the database is opened in read-only or read-write mode.
This affects both file permissions and SQLite's internal locking behavior.
-}
data SqliteAccessMode
    = -- | Open database in read-only mode. No modifications allowed.
      SqliteReadOnly
    | -- | Open database in read-write mode. Both reads and writes allowed.
      SqliteReadWrite
    deriving (Show, Ord, Eq, Generic)

-- | Serialize SqliteAccessMode as kebab-case strings.
instance ToJSON SqliteAccessMode where
    toJSON SqliteReadOnly = Aeson.String "read-only"
    toJSON SqliteReadWrite = Aeson.String "read-write"

-- | Parse SqliteAccessMode from kebab-case strings.
instance FromJSON SqliteAccessMode where
    parseJSON = Aeson.withText "SqliteAccessMode" $ \txt ->
        case txt of
            "read-only" -> return SqliteReadOnly
            "read-write" -> return SqliteReadWrite
            other -> fail $ "Invalid SqliteAccessMode: " ++ Text.unpack other ++ ". Expected 'read-only' or 'read-write'."

{- | Configuration for a SQLite builtin toolbox.

This describes a SQLite database to load as a builtin toolbox.
The toolbox provides tools for querying and optionally modifying
the SQLite database.

Example configuration:

@
{
  "name": "memory",
  "description": "a set of memories",
  "path": "/path/to/memories.sqlite",
  "access": "read-write"
}
@

The 'access' field controls whether the database is opened in
read-only or read-write mode. Use 'read-only' for safety when
the agent should only query data, and 'read-write' when the agent
needs to modify the database.
-}
data SqliteToolboxDescription
    = SqliteToolboxDescription
    { sqliteToolboxName :: Text
    -- ^ Unique name for this toolbox instance (used as tool prefix)
    , sqliteToolboxDescription :: Text
    -- ^ Human-readable description of the database contents/purpose
    , sqliteToolboxPath :: FilePath
    -- ^ Path to the SQLite database file
    , sqliteToolboxAccess :: SqliteAccessMode
    -- ^ Access mode: read-only or read-write
    }
    deriving (Show, Ord, Eq, Generic)

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
            other -> fail $ "Invalid SystemToolCapability: " ++ Text.unpack other ++ ". Expected one of: date, operating-system, env-vars, running-user, hostname, working-directory, process-info, uptime."

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
    "envVarFilter": null
  }
}
@

The 'envVarFilter' field is an optional regex/pattern to filter
environment variables when the 'env-vars' capability is enabled.
If not specified, all environment variables are exposed.
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
    deriving (Show, Ord, Eq, Generic)

-- | Serialize DeveloperToolCapability as kebab-case strings.
instance ToJSON DeveloperToolCapability where
    toJSON DevToolValidateTool = Aeson.String "validate-tool"
    toJSON DevToolScaffoldAgent = Aeson.String "scaffold-agent"
    toJSON DevToolScaffoldTool = Aeson.String "scaffold-tool"
    toJSON DevToolShowSpec = Aeson.String "show-spec"

-- | Parse DeveloperToolCapability from kebab-case strings.
instance FromJSON DeveloperToolCapability where
    parseJSON = Aeson.withText "DeveloperToolCapability" $ \txt ->
        case txt of
            "validate-tool" -> return DevToolValidateTool
            "scaffold-agent" -> return DevToolScaffoldAgent
            "scaffold-tool" -> return DevToolScaffoldTool
            "show-spec" -> return DevToolShowSpec
            other -> fail $ "Invalid DeveloperToolCapability: " ++ Text.unpack other ++ ". Expected one of: validate-tool, scaffold-agent, scaffold-tool, show-spec."

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
    "capabilities": ["validate-tool", "scaffold-agent", "scaffold-tool"]
  }
}
@
-}
data DeveloperToolboxDescription
    = DeveloperToolboxDescription
    { developerToolboxName :: Text
    -- ^ Unique name for this toolbox instance (used as tool prefix)
    , developerToolboxDescription :: Text
    -- ^ Human-readable description of the toolbox purpose
    , developerToolboxCapabilities :: [DeveloperToolCapability]
    -- ^ List of developer tool capabilities to expose
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
    {"tag": "DeveloperToolbox", "contents": {"name": "developer", "description": "Development tools", "capabilities": ["validate-tool", "scaffold-agent"]}}
  ]
}
@
-}
data BuiltinToolboxDescription
    = SqliteToolbox SqliteToolboxDescription
    | SystemToolbox SystemToolboxDescription
    | DeveloperToolbox DeveloperToolboxDescription
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
            _ -> fail "expecting 'SqliteToolbox', 'SystemToolbox', or 'DeveloperToolbox' tag"

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
    , openApiToolboxes :: Maybe [OpenAPIToolboxDescription]
    , postgrestToolboxes :: Maybe [PostgRESTToolboxDescription]
    , builtinToolboxes :: Maybe [BuiltinToolboxDescription]
    , extraAgents :: Maybe [ExtraAgentRef]
    }
    deriving (Show, Ord, Eq, Generic)

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

