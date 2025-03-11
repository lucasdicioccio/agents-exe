{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module System.Agents.MCP.Base where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)

type Name = Text
type MIME = Text
type Base64Encoded = Text
type ByteSize = Int
type URI = Text

newtype Cursor = Cursor Text
    deriving (Show, Ord, Eq, FromJSON, ToJSON)

-------------------------------------------------------

type Priority = Maybe Float

data Role = UserRole | AssistantRole

instance ToJSON Role where
    toJSON v = case v of
        UserRole -> Aeson.String "user"
        AssistantRole -> Aeson.String "assistant"

data Annotation = Annotation
    { audience :: [Role]
    , priority :: Priority
    }
instance ToJSON Annotation where
    toJSON a = object ["audience" .= a.audience, "priority" .= a.priority]

data TextContentImpl = TextContentImpl
    { text :: Text
    , annotations :: [Annotation]
    }

instance ToJSON TextContentImpl where
    toJSON t =
        object ["text" .= t.text, "annotations" .= t.annotations, "type" .= ("text" :: Text)]

data ImageContentImpl = ImageContentImpl
    { data_ :: Base64Encoded
    , mimeType :: MIME
    , annotations :: [Annotation]
    }

instance ToJSON ImageContentImpl where
    toJSON i =
        object ["data" .= i.data_, "mimeType" .= i.mimeType, "annotations" .= i.annotations, "type" .= ("image" :: Text)]

data EmbeddedResource = EmbeddedResource
    -- todo:ensure we have a hardcoded type= resource
    { resource :: ResourceContents
    , annotations :: [Annotation]
    }

data Content
    = TextContent TextContentImpl
    | ImageContent ImageContentImpl
    | EmbeddedResourceContent ResourceContents
instance ToJSON Content where
    toJSON (TextContent o) = toJSON o
    toJSON (ImageContent o) = toJSON o
    toJSON (EmbeddedResourceContent o) = toJSON o

---------------

data Resource = Resource
    { uri :: URI
    , name :: Name
    , description :: Maybe Text
    , mimeType :: Maybe MIME
    , size :: Maybe ByteSize
    }
    deriving (Show)

instance ToJSON Resource where
    toJSON r =
        object
            [ "uri" .= r.uri
            , "name" .= r.name
            , "description" .=? r.description
            , "mimeType" .=? r.mimeType
            , "size" .=? r.size
            ]

data ResourceContents
    = TextResourceContents !TextResourceContentsImpl
    | BlobResourceContents !BlobResourceContentsImpl

instance ToJSON ResourceContents where
    toJSON (TextResourceContents i) = toJSON i
    toJSON (BlobResourceContents i) = toJSON i

data TextResourceContentsImpl = TextResourceContentsImpl
    { uri :: URI
    , mimeType :: Maybe MIME
    , text :: Text
    }

instance ToJSON TextResourceContentsImpl where
    toJSON t = object ["uri" .= t.uri, "mimeType" .=? t.mimeType, "text" .= t.text]

data BlobResourceContentsImpl = BlobResourceContentsImpl
    { uri :: URI
    , mimeType :: Maybe MIME
    , blob :: Base64Encoded
    }

instance ToJSON BlobResourceContentsImpl where
    toJSON t = object ["uri" .= t.uri, "mimeType" .=? t.mimeType, "blob" .= t.blob]

---------------
data Prompt = Prompt
    { name :: Text
    , description :: Maybe Text
    , arguments :: [PromptArgument]
    }
instance ToJSON Prompt where
    toJSON p =
        object
            [ "name" .= p.name
            , "description" .=? p.description
            , "arguments" .= p.arguments
            ]

data PromptArgument = PromptArgument
    { name :: Text
    , description :: Maybe Text
    , required :: Maybe Bool
    }
instance ToJSON PromptArgument where
    toJSON pa =
        object
            [ "name" .= pa.name
            , "description" .=? pa.description
            , "required" .=? pa.required
            ]

data PromptMessage = PromptMessage
    { role :: Role
    , content :: PromptMessageContent
    }

type PromptMessageContent = Content

-- note: only aluded to but not defined in the .ts
-- https://github.com/modelcontextprotocol/rust-sdk/blob/main/crates/mcp-core/src/prompt.rs#L152
data PromptTemplate = PromptTemplate
    { id :: Text
    , template :: Text
    , arguments :: [PromptArgumentTemplate]
    }

data PromptArgumentTemplate = PromptArgumentTemplate
    { name :: Name
    , description :: Maybe Text
    , required :: Maybe Bool
    }

---------------

data Tool = Tool
    { name :: Name
    , description :: Maybe Text
    , inputSchema :: InputSchema
    }

instance ToJSON Tool where
    toJSON t =
        object
            [ "name" .= t.name
            , "description" .=? t.description
            , "inputSchema" .= t.inputSchema
            ]

data InputSchema = InputSchema
    { required :: Maybe [Text]
    , properties :: Maybe Aeson.Object
    }

instance ToJSON InputSchema where
    toJSON is =
        object
            [ "type" .= ("object" :: Text)
            , "required" .=? is.required
            , "properties" .=? is.properties
            ]

---------------

type FlagsSet a = [a]

data ClientFlag
    = RootsListChanged
    deriving (Show, Eq)

data ServerFlag
    = PromptsListChanged
    | ResourcesSubscribe
    | ResourcesListChanged
    | ToolsListChanged
    deriving (Show, Eq)

data ClientCapabilities
    = ClientCapabilities
    { experimental :: Maybe Aeson.Object
    , sampling :: Maybe Aeson.Value
    , flags :: FlagsSet ClientFlag
    }
    deriving (Show)

instance FromJSON ClientCapabilities where
    parseJSON = withObject "ClientCapabilities" $ \p -> do
        ClientCapabilities
            <$> p .:? "experimental"
            <*> p .:? "sampling"
            <*> pure (clientFlags p)
      where
        clientFlags :: Aeson.Object -> FlagsSet ClientFlag
        clientFlags o = Maybe.mapMaybe (\(k, f) -> flg k f o) [(RootsListChanged, hasRootsListChanged)]

        flg :: ClientFlag -> (Aeson.Object -> Maybe Bool) -> Aeson.Object -> Maybe ClientFlag
        flg k predParse o =
            if Maybe.fromMaybe False (predParse o)
                then Just k
                else Nothing

        hasRootsListChanged :: Aeson.Object -> Maybe Bool
        hasRootsListChanged o = do
            (Aeson.Object r) <- Aeson.lookup "roots" o
            (Aeson.Bool lc) <- Aeson.lookup "listChanged" r
            pure lc

data ServerCapabilities
    = ServerCapabilities
    { experimental :: Maybe Aeson.Object
    , logging :: Maybe Aeson.Value
    , flags :: FlagsSet ServerFlag
    }
    deriving (Show)

instance ToJSON ServerCapabilities where
    toJSON c =
        object
            [ "experimental" .=? c.experimental
            , "logging" .=? c.logging
            , "prompts"
                .= object
                    [ "listChanged" .= List.elem PromptsListChanged c.flags
                    ]
            , "resources"
                .= object
                    [ "listChanged" .= List.elem ResourcesListChanged c.flags
                    , "subscribe" .= List.elem ResourcesSubscribe c.flags
                    ]
            , "tools"
                .= object
                    [ "listChanged" .= List.elem ToolsListChanged c.flags
                    ]
            ]

data PingRequest = PingRequest
    deriving (Show)

data InitializeRequest = InitializeRequest
    { protocolVersion :: Text
    , capabilities :: ClientCapabilities
    , clientInfo :: Implementation
    }
    deriving (Show)

instance FromJSON InitializeRequest where
    parseJSON = withObject "InitializeRequest.params" $ \p ->
        InitializeRequest
            <$> p .: "protocolVersion"
            <*> p .: "capabilities"
            <*> p .: "clientInfo"

data InitializeResult = InitializeResult
    { protocolVersion :: Text
    , capabilities :: ServerCapabilities
    , serverInfo :: Implementation
    , instructions :: Maybe Text
    }
    deriving (Show)

instance ToJSON InitializeResult where
    toJSON ir =
        object
            [ "protocolVersion" .= ir.protocolVersion
            , "capabilities" .= ir.capabilities
            , "serverInfo" .= ir.serverInfo
            , "instructions" .=? ir.instructions
            ]

data Implementation = Implementation
    { name :: Text
    , version :: Text
    }
    deriving (Show)

instance FromJSON Implementation where
    parseJSON = withObject "Implementation" $ \p ->
        Implementation
            <$> p .: "name"
            <*> p .: "version"

instance ToJSON Implementation where
    toJSON i = object ["name" .= i.name, "version" .= i.version]

data ListResourcesRequest = ListResourcesRequest
    { cursor :: Maybe Cursor
    }
    deriving (Show)

instance FromJSON ListResourcesRequest where
    parseJSON = withObject "ListResourcesRequest" $ \l ->
        ListResourcesRequest
            <$> l .:? "cursor"

data ListResourcesResult = ListResourcesResult
    { resources :: [Resource]
    , nextCursor :: Maybe Cursor
    }
    deriving (Show)

instance ToJSON ListResourcesResult where
    toJSON lr =
        object
            [ "resources" .= lr.resources
            , "nextCursor" .=? lr.nextCursor
            ]

data ReadResourceRequest = ReadResourceRequest
    { uri :: URI
    }

data ReadResourceResult = ReadResourceResult
    { contents :: [ResourceContents]
    }

data SubscribeRequest = SubscribeRequest
    { uri :: URI
    }

data UnsubscribeRequest = UnsubscribeRequest
    { uri :: URI
    }

data ListToolsRequest = ListToolsRequest
    { cursor :: Maybe Cursor
    }
    deriving (Show)

instance FromJSON ListToolsRequest where
    parseJSON = withObject "ListToolsRequest" $ \l ->
        ListToolsRequest <$> l .:? "cursor"

data ListToolsResult = ListToolsResult
    { tools :: [Tool]
    , nextCursor :: Maybe Cursor
    }
instance ToJSON ListToolsResult where
    toJSON lt =
        object
            [ "tools" .= lt.tools
            , "nextCursor" .=? lt.nextCursor
            ]

data CallToolRequest = CallToolRequest
    { name :: Name
    , arguments :: Maybe Aeson.Object
    }
    deriving (Show)
instance FromJSON CallToolRequest where
    parseJSON = withObject "CallToolRequest" $ \o ->
        CallToolRequest
            <$> o .: "name"
            <*> o .: "arguments"

data CallToolResult = CallToolResult
    { content :: [Content]
    , isError :: Maybe Bool
    }
instance ToJSON CallToolResult where
    toJSON ct =
        object
            [ "content" .= ct.content
            , "isError" .=? ct.isError
            ]

data ListPromptsRequest = ListPromptsRequest
    { cursor :: Maybe Cursor
    }
    deriving (Show)

instance FromJSON ListPromptsRequest where
    parseJSON = withObject "ListPromptsRequest" $ \o ->
        ListPromptsRequest <$> o .:? "cursor"

data ListPromptsResult = ListPromptsResult
    { prompts :: [Prompt]
    , nextCursor :: Maybe Cursor
    }

instance ToJSON ListPromptsResult where
    toJSON lp =
        object
            [ "prompts" .= lp.prompts
            , "nextCursor" .=? lp.nextCursor
            ]

data GetPromptRequest = GetPromptRequest
    { name :: Name
    , arguments :: Maybe Aeson.Object
    }

data GetPromptResult = GetPromptResult
    { description :: Maybe Text
    , messages :: [PromptMessage]
    }

data EmptyResult = EmptyResult

data RequestId
    = IntRequestId !Int
    | TextRequestId !Text
    deriving (Show, Ord, Eq)
instance FromJSON RequestId where
    parseJSON v = (IntRequestId <$> Aeson.parseJSON v) <|> (TextRequestId <$> Aeson.parseJSON v)

data CancelledNotification = CancelledNotification
    { requestId :: RequestId
    , reason :: Maybe Text
    }
    deriving (Show)
instance FromJSON CancelledNotification where
    parseJSON = withObject "CancelledNotification" $ \o ->
        CancelledNotification <$> o .: "requestId" <*> o .: "reason"

data SetLevelRequest = SetLevelRequest
    { level :: LoggingLevel
    }

data InitializedNotification = InitializedNotification
    deriving (Show, Ord, Eq)

data ProgressToken = TextProgressToken Text | NumberProgresstoken Float
    deriving (Show, Ord, Eq)

data ProgressNotification
    = ProgressNotification
    { progressToken :: ProgressToken
    , progress :: Int
    , total :: Maybe Int
    }

data ResourceListChangedNotification = ResourceListChangedNotification

data ResourceUpdatedNotification = ResourceUpdatedNotification
    { uri :: URI
    }

data PromptListChangedNotification = PromptListChangedNotification
data ToolListChangedNotification = ToolListChangedNotification

data LoggingMessageNotification = LoggingMessageNotification
    { level :: LoggingLevel
    , logger :: Maybe Text
    , data_ :: Aeson.Value
    }

data RootsListChangedNotification = RootsListChangedNotification

data Root = Root
    { uri :: URI
    , name :: Maybe Name
    }

data ListRootsRequest = ListRootsRequest

data ListRootsResult = ListRootsResult
    { roots :: [Root]
    }

data ResourceTemplate = ResourceTemplate
    { uriTemplate :: URI
    , name :: Text
    , description :: Maybe Text
    , mimeType :: Maybe MIME
    }

data ListResourcesTemplateRequest = ListResourcesTemplateRequest
    { cursor :: Maybe Cursor
    }

data ListResourcesTemplateResult = ListResourceTemplatesResult
    { resourceTemplates :: [ResourceTemplate]
    , nextCursor :: Maybe Cursor
    }

data SamplingMessage = SamplingMessage
    { role :: Role
    , content :: SamplingMessageContents
    }

data SamplingMessageContents
    = TextSamplingMessageContents TextContentImpl
    | ImageSamplingMessageContents ImageContentImpl

type Temperature = Float

data IncludeContext
    = None
    | ThisServer
    | AllServers

instance ToJSON IncludeContext where
    toJSON v = case v of
        None -> Aeson.String "none"
        ThisServer -> Aeson.String "thisServer"
        AllServers -> Aeson.String "allServer"

data ModelHint = ModelHintName Text

instance ToJSON ModelHint where
    toJSON (ModelHintName n) =
        object ["name" .= n]

data ModelPreferences = ModelPreferences
    { hints :: [ModelHint]
    , costPriority :: Priority
    , speedPriority :: Priority
    , intelligencePriority :: Priority
    }

data CreateMessageRequest
    = CreateMessageRequest
    { messages :: [SamplingMessage]
    , modelPreferences :: Maybe ModelPreferences
    , systemPrompt :: Maybe Text
    , includeContext :: Maybe IncludeContext
    , temperature :: Maybe Temperature
    , maxTokens :: Int
    , stopSequences :: Maybe [Text]
    , metadata :: Maybe Aeson.Object
    }

data CreateMessageResult
    = CreateMessageResult
    { model :: Text
    , stopReason :: Maybe StopReason
    , role :: Role
    , content :: SamplingMessageContents
    }

data StopReason
    = StopReason_EndTurn
    | StopReason_StopSequence
    | StopReason_MaxTokens
    | StopReason_Other !Text

data LoggingLevel
    = LoggingLevel_Debug
    | LoggingLevel_Info
    | LoggingLevel_Notice
    | LoggingLevel_Warning
    | LoggingLevel_Error
    | LoggingLevel_Critical
    | LoggingLevel_Alert
    | LoggingLevel_Emergency

data CompleteRequest = CompleteRequest
    { ref :: CompleteRequestRef
    , argument :: CompleteRequestArgument
    }

data CompleteRequestArgument = CompleteRequestArgument
    { name :: Name
    , value :: Text
    }

data CompleteRequestRef
    = PromptCompleteRequestRef PromptRefImpl
    | ResourceCompleteRequestRef ResourceRefImpl

data PromptRefImpl = PromptRefImpl
    { name :: Text
    }

data ResourceRefImpl = ResourceRefImpl
    { uri :: URI
    }

data CompleteResult = CompleteResult
    { values :: [Text]
    , total :: Maybe Int
    , hasMore :: Maybe Bool
    }

pairz :: [Maybe Aeson.Pair] -> Aeson.Object
pairz = Aeson.fromList . Maybe.catMaybes

object :: [Maybe Aeson.Pair] -> Aeson.Value
object = Aeson.object . Maybe.catMaybes

(.=?) :: (ToJSON v) => Aeson.Key -> Maybe v -> Maybe Aeson.Pair
(.=?) k v = fmap ((Aeson..=) k) v

infixr 8 .=
(.=) :: (ToJSON v) => Aeson.Key -> v -> Maybe Aeson.Pair
(.=) k v = Just ((Aeson..=) k v)
