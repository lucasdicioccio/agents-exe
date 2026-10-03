{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{- | Agent configurations kept in a database rather than in files.

A stored agent is the @contents@ of an agent file (the 'Base.Agent'
record), keyed by slug, together with the files of its bash tools
('saFiles'). Its @extraAgents@ name other stored agents by slug
('resolveHelpers'). The other fields that refer to files cannot be stored:
see 'fileBasedFields'.
-}
module System.Agents.AgentStore (
    StoredAgent (..),
    ToolFiles,
    AgentStore (..),
    mkSqliteAgentStore,
    fileBasedFields,
    storedPathErrors,
    HelperError (..),
    resolveHelpers,
    referencedSlugs,
    materializeToolFiles,
) where

import Control.Monad (foldM, forM_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection, Only (..), execute, execute_, query, query_)
import Database.SQLite.Simple.QQ (sql)
import System.Directory (createDirectoryIfMissing, getPermissions, setOwnerExecutable, setPermissions)
import System.FilePath (isAbsolute, splitDirectories, takeDirectory, (</>))

import qualified System.Agents.Base as Base
import System.Agents.SessionStore (Migration (..), runMigrations)

{- | The files of a stored agent's bash tools: contents by path, relative to
the directory the agent's @toolDirectory@ and @bashToolboxes@ paths are
relative to.
-}
type ToolFiles = Map FilePath Text

data StoredAgent = StoredAgent
    { saConfig :: Base.Agent
    , saFiles :: ToolFiles
    , saUpdatedAt :: UTCTime
    , saUpdatedBy :: Maybe Text
    -- ^ The owner who stored it, when the host authenticates callers.
    }
    deriving (Show)

data AgentStore = AgentStore
    { asList :: IO [StoredAgent]
    -- ^ Every stored agent whose configuration still parses.
    , asPut :: Maybe Text -> Base.Agent -> ToolFiles -> IO StoredAgent
    -- ^ Insert or replace an agent and its files, by slug.
    , asDelete :: Text -> IO Bool
    -- ^ Remove an agent; 'False' when there was none.
    }

{- | Fields of a configuration that refer to files or directories on the
host, and that a stored agent therefore cannot use, with their names.
@toolDirectory@ and @bashToolboxes@ are not among them: their files are
stored with the agent ('saFiles'). Nor is @extraAgents@, which names other
stored agents.
-}
fileBasedFields :: Base.Agent -> [Text]
fileBasedFields agent =
    [ name
    | (name, used) <-
        [ ("openApiToolboxes", nonEmpty (Base.openApiToolboxes agent))
        , ("postgrestToolboxes", nonEmpty (Base.postgrestToolboxes agent))
        , ("skillSources", nonEmpty (Base.skillSources agent))
        , ("autoEnableSkills", nonEmpty (Base.autoEnableSkills agent))
        ]
    , used
    ]
  where
    nonEmpty = maybe False (not . null)

{- | What is wrong with the paths of a stored agent, if anything. Its tool
paths and the paths of its files must stay inside the agent's own directory
(relative, without @..@), and a helper is named by slug alone.
-}
storedPathErrors :: Base.Agent -> ToolFiles -> [Text]
storedPathErrors agent files =
    [ what <> " " <> quoted path <> " must be a relative path without '..'"
    | (what, path) <- toolPaths ++ [("file", f) | f <- Map.keys files]
    , not (contained path)
    ]
        ++ [ "bashToolboxes root " <> quoted root <> " cannot be used by a stored agent"
           | Base.FileSystemDirectory desc <- toolboxes
           , Just root <- [desc.fsDirRoot]
           ]
        ++ [ "extraAgents entry '" <> ref.extraAgentSlug <> "' names a stored agent by slug and takes no path"
           | ref <- fromMaybe [] (Base.extraAgents agent)
           , not (null ref.extraAgentPath)
           ]
  where
    quoted path = "'" <> Text.pack path <> "'"
    toolboxes = fromMaybe [] (Base.bashToolboxes agent)
    toolPaths =
        [("toolDirectory", dir) | Just dir <- [Base.toolDirectory agent]]
            ++ [("bashToolboxes path", desc.fsDirPath) | Base.FileSystemDirectory desc <- toolboxes]
            ++ [("bashToolboxes path", desc.singleToolPath) | Base.SingleTool desc <- toolboxes]
    contained path =
        not (null path) && not (isAbsolute path) && ".." `notElem` splitDirectories path

-- | Why the helpers of a stored agent cannot be resolved.
data HelperError
    = -- | An agent (first) names a helper (second) that is not stored.
      UnknownHelper Text Text
    | -- | The slugs of a reference cycle, ending where it starts.
      HelperCycle [Text]
    deriving (Show, Eq)

-- | The slugs an agent's @extraAgents@ name.
referencedSlugs :: Base.Agent -> [Text]
referencedSlugs = map (.extraAgentSlug) . fromMaybe [] . Base.extraAgents

{- | Every stored agent an agent reaches through @extraAgents@, itself
excluded, each once. The references of stored agents must form a DAG:
unlike an agent from a file, a stored agent can refer neither to itself nor
back to an agent that reaches it.
-}
resolveHelpers :: Map Text StoredAgent -> Base.Agent -> Either HelperError [StoredAgent]
resolveHelpers available root = Map.elems <$> go [Base.slug root] Map.empty root
  where
    go path found agent = foldM (step path (Base.slug agent)) found (referencedSlugs agent)
    step path from found slug
        | slug `elem` path = Left $ HelperCycle (slug : reverse (slug : takeWhile (/= slug) path))
        | Map.member slug found = Right found
        | otherwise = case Map.lookup slug available of
            Nothing -> Left $ UnknownHelper from slug
            Just sa -> go (slug : path) (Map.insert slug sa found) sa.saConfig

{- | Write a stored agent's files under a directory, and answer the
configuration that loads its bash tools from there. Every file is made
executable, and the directories the configuration names exist afterwards,
so that a tool directory without files is an empty toolbox.
-}
materializeToolFiles :: FilePath -> StoredAgent -> IO Base.Agent
materializeToolFiles dir sa = do
    createDirectoryIfMissing True dir
    forM_ (Map.toList sa.saFiles) $ \(path, contents) -> do
        let target = dir </> path
        createDirectoryIfMissing True (takeDirectory target)
        ByteString.writeFile target (TextEnc.encodeUtf8 contents)
        perms <- getPermissions target
        setPermissions target (setOwnerExecutable True perms)
    forM_ toolDirs $ createDirectoryIfMissing True . (dir </>)
    pure
        agent
            { Base.toolDirectory = (dir </>) <$> Base.toolDirectory agent
            , Base.bashToolboxes = map inDir <$> Base.bashToolboxes agent
            }
  where
    agent = sa.saConfig
    toolDirs =
        [d | Just d <- [Base.toolDirectory agent]]
            ++ [desc.fsDirPath | Base.FileSystemDirectory desc <- fromMaybe [] (Base.bashToolboxes agent)]
    inDir (Base.FileSystemDirectory desc) = Base.FileSystemDirectory desc{Base.fsDirPath = dir </> desc.fsDirPath}
    inDir (Base.SingleTool desc) = Base.SingleTool desc{Base.singleToolPath = dir </> desc.singleToolPath}

-- | An agent store in the host's SQLite database (table @agents@).
mkSqliteAgentStore :: Connection -> IO AgentStore
mkSqliteAgentStore conn = do
    runMigrations conn "agents" agentMigrations
    pure
        AgentStore
            { asList = do
                rows <- query_ conn [sql| SELECT json, files, updated_at, updated_by FROM agents ORDER BY slug |] :: IO [(Text, Text, UTCTime, Maybe Text)]
                pure $ mapMaybe fromRow rows
            , asPut = \by agent files -> do
                now <- getCurrentTime
                execute
                    conn
                    [sql| INSERT INTO agents (slug, json, files, updated_at, updated_by) VALUES (?, ?, ?, ?, ?)
                          ON CONFLICT(slug) DO UPDATE SET
                            json = excluded.json, files = excluded.files,
                            updated_at = excluded.updated_at, updated_by = excluded.updated_by |]
                    (Base.slug agent, encode agent, encode files, now, by)
                pure $ StoredAgent agent files now by
            , asDelete = \slug -> do
                rows <- query conn [sql| DELETE FROM agents WHERE slug = ? RETURNING slug |] (Only slug) :: IO [Only Text]
                pure $ not (null rows)
            }
  where
    fromRow (json, files, updated, by) = do
        agent <- Aeson.decodeStrict' (TextEnc.encodeUtf8 json)
        storedFiles <- Aeson.decodeStrict' (TextEnc.encodeUtf8 files)
        pure $ StoredAgent agent storedFiles updated by
    encode :: (Aeson.ToJSON a) => a -> Text
    encode = TextEnc.decodeUtf8 . LByteString.toStrict . Aeson.encode

agentMigrations :: [Migration]
agentMigrations =
    [ Migration 1 $ \conn ->
        execute_
            conn
            [sql| CREATE TABLE IF NOT EXISTS agents (
                slug TEXT PRIMARY KEY,
                json TEXT NOT NULL,
                updated_at TIMESTAMP NOT NULL,
                updated_by TEXT
            ) |]
    , -- The files of the agent's bash tools: a JSON object, contents by path.
      Migration 2 $ \conn ->
        execute_ conn [sql| ALTER TABLE agents ADD COLUMN files TEXT NOT NULL DEFAULT '{}' |]
    ]
