# File Loader

The file loader utilities provide JSON file loading, section-based file parsing, and directory scanning for agent configurations and tools.

## Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                      File Loader System                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐          │
│  │ FileLoader   │  │ FileLoader.  │  │ FileLoader.  │          │
│  │ (main API)   │  │ JSON         │  │ Section      │          │
│  │              │  │ (parsing)    │  │ (extract)    │          │
│  └──────────────┘  └──────────────┘  └──────────────┘          │
│                                                                  │
│  Functions:                                                      │
│  - listJsonDirectory  - readJsonDescriptionFile                  │
│  - loadJsonFile       - parseAgentJson                           │
│  - findSection        - extractSection                           │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## Core Types

### FileLoader (`System.Agents.FileLoader`)

```haskell
module System.Agents.FileLoader
    ( module System.Agents.Base
    , module System.Agents.FileLoader.JSON
    , Trace (..)
    , Agents (..)
    , listJsonDirectory
    , loadJsonFile
    , InvalidAgentError (..)
    ) where

data Trace
    = LoadJsonFile !FilePath
    | LoadJsonFileFailure !FilePath String
    deriving (Show)

data Agents = Agents
    { dir :: FilePath
    , agents :: [AgentDescription]
    }

data InvalidAgentError
    = LoadFailure FilePath String
    deriving (Show)
```

### FileLoader.JSON (`System.Agents.FileLoader.JSON`)

```haskell
module System.Agents.FileLoader.JSON where

readJsonDescriptionFile :: 
    FilePath -> 
    IO (Either String AgentDescription)

parseAgentJson :: 
    ByteString -> 
    Either String AgentDescription
```

### FileLoader.Section (`System.Agents.FileLoader.Section`)

```haskell
module System.Agents.FileLoader.Section where

-- Extract a delimited section from file content
extractSection :: 
    Text ->           -- Start delimiter (e.g., "----BEGIN----")
    Text ->           -- End delimiter (e.g., "----END----")
    Text ->           -- File content
    Maybe Text        -- Extracted section

-- Find section with default delimiters
findSection :: 
    Text ->           -- Section name
    Text ->           -- File content  
    Maybe Text        -- Extracted content
```

## Directory Scanning

### listJsonDirectory

Recursively finds all `.json` files in a directory:

```haskell
listJsonDirectory :: FilePath -> IO [FilePath]
listJsonDirectory path = do
    entries <- listDirectory path
    files <- forM entries $ \entry -> do
        let fullPath = path </> entry
        isDir <- doesDirectoryExist fullPath
        if isDir
            then listJsonDirectory fullPath
            else return [fullPath | isJson fullPath]
    return $ concat files
  where
    isJson p = takeExtension p == ".json"
```

**Example:**

```haskell
-- Directory structure:
-- agents/
--   ├── main.json
--   ├── helpers/
--   │   ├── helper1.json
--   │   └── helper2.json
--   └── utils.json

listJsonDirectory "agents"
-- Returns: ["agents/main.json", "agents/helpers/helper1.json", 
--           "agents/helpers/helper2.json", "agents/utils.json"]
```

### With Tracing

```haskell
loadJsonFile :: 
    Tracer IO Trace -> 
    FilePath -> 
    IO (Either InvalidAgentError AgentDescription)
loadJsonFile tracer path = do
    runTracer tracer (LoadJsonFile path)
    result <- readJsonDescriptionFile path
    case result of
        Right desc -> return $ Right desc
        Left err -> do
            runTracer tracer (LoadJsonFileFailure path err)
            return $ Left $ LoadFailure path err
```

## JSON Parsing

### Agent Description Parsing

```haskell
readJsonDescriptionFile :: FilePath -> IO (Either String AgentDescription)
readJsonDescriptionFile path = do
    contents <- ByteString.readFile path
    return $ parseAgentJson contents

parseAgentJson :: ByteString -> Either String AgentDescription
parseAgentJson = eitherDecodeStrict
```

### JSON Schema

```json
{
  "tag": "OpenAIAgentDescription",
  "contents": {
    "slug": "my-agent",
    "apiKeyId": "openai",
    "flavor": "openai",
    "modelUrl": "https://api.openai.com/v1",
    "modelName": "gpt-4",
    "announce": "A helpful assistant",
    "systemPrompt": ["You are helpful."],
    "toolDirectory": "tools",
    "mcpServers": [
      {
        "tag": "McpSimpleBinary",
        "contents": {
          "name": "filesystem",
          "executable": "/usr/bin/mcp-fs",
          "args": ["--root", "/home/user"]
        }
      }
    ],
    "openApiToolboxes": [
      {
        "tag": "OpenAPIServer",
        "contents": {
          "specUrl": "https://api.example.com/openapi.json",
          "baseUrl": "https://api.example.com",
          "headers": null,
          "token": "${API_TOKEN}"
        }
      }
    ],
    "extraAgents": [
      {"slug": "helper", "path": "./helper.json"}
    ]
  }
}
```

## Section Extraction

### Use Case: Multi-part Prompt Files

Section extraction allows embedding multiple content blocks in a single file:

```markdown
# My Prompt File

Some instructions here.

----BEGIN SYSTEM----
You are a code reviewer. Focus on:
- Security issues
- Performance
- Maintainability
----END SYSTEM----

----BEGIN EXAMPLE----
Example review output format...
----END EXAMPLE----

Rest of file...
```

### Extracting Sections

```haskell
import System.Agents.FileLoader.Section

content <- Text.readFile "prompt.md"

let systemPrompt = extractSection 
        "----BEGIN SYSTEM----" 
        "----END SYSTEM----" 
        content
        
let exampleOutput = extractSection
        "----BEGIN EXAMPLE----"
        "----END EXAMPLE----"
        content
```

### Named Sections

```haskell
-- Using findSection with default delimiters
findSection :: Text -> Text -> Maybe Text
findSection name content = 
    extractSection start end content
  where
    start = "----BEGIN " <> name <> "----"
    end   = "----END " <> name <> "----"

-- Usage
findSection "PROMPT" content
findSection "CONTEXT" content
findSection "EXAMPLES" content
```

## Configuration Loading

### agents-exe.cfg.json

Project configuration discovery:

```haskell
locateAgentsExeConfig :: IO (Maybe FilePath)
locateAgentsExeConfig = do
    go =<< getCurrentDirectory
  where
    go "" = return Nothing
    go "/" = return Nothing
    go path = do
        let tentative = path </> "agents-exe.cfg.json"
        exists <- doesFileExist tentative
        if exists
            then return $ Just tentative
            else go (takeDirectory path)
```

**Search order:**
1. Start at current directory
2. Check for `agents-exe.cfg.json`
3. If not found, move to parent
4. Repeat until root

### Configuration Schema

```haskell
data AgentsExeConfig = AgentsExeConfig
    { agentsConfigDir :: Maybe FilePath
    , agentsDirectories :: [FilePath]
    , agentsFiles :: [FilePath]
    , agentsLogs :: Maybe AgentsExeLogConfig
    } deriving (Show, Generic)

data AgentsExeLogConfig = AgentsExeLogConfig
    { logJsonHttpEndpoint :: Maybe String
    , logJsonPath :: Maybe FilePath
    , logRawPath :: Maybe FilePath
    , logSessionsJsonPrefix :: Maybe FilePath
    } deriving (Show, Generic)

instance FromJSON AgentsExeConfig
instance FromJSON AgentsExeLogConfig
```

## Error Handling

### JSON Parse Errors

```haskell
data InvalidAgentError
    = LoadFailure FilePath String
    deriving (Show)

-- Example error messages:
-- "LoadFailure ./agent.json: Error in $.modelName: expected Text, got Number"
-- "LoadFailure ./agent.json: Missing required field 'systemPrompt'"
```

### File Not Found

```haskell
handleFileError :: FilePath -> IOException -> IO (Either InvalidAgentError a)
handleFileError path e = return $ Left $ LoadFailure path (show e)

loadJsonFileSafe :: FilePath -> IO (Either InvalidAgentError AgentDescription)
loadJsonFileSafe path = 
    (loadJsonFile silentTracer path) `catch` handleFileError path
```

### Validation

```haskell
validateAgent :: Agent -> Either ValidationError ()
validateAgent agent = do
    when (T.null $ slug agent) $ 
        Left $ MissingField "slug"
    when (T.null $ apiKeyId agent) $ 
        Left $ MissingField "apiKeyId"
    when (T.null $ modelUrl agent) $ 
        Left $ MissingField "modelUrl"
    -- ... more validation
```

## File Watching

### Hot Reload Support

The bash toolbox uses file watching for hot reloading:

```haskell
-- From BashToolbox module
initializeBackroundToolbox :: 
    Tracer IO Trace -> 
    FilePath -> 
    IO (Either ToolboxError Toolbox)
initializeBackroundToolbox tracer dir = do
    -- Initial tool discovery
    tools <- discoverTools dir
    
    -- Create background thread for watching
    backgroundVal <- Background.backgroundVal 
        tracer
        (BashToolsLoadingTrace . map toolName)
        (loadTools dir)
        
    return $ Right Toolbox
        { tools = backgroundVal
        , triggerReload = Background.triggerReload backgroundVal
        }
```

### File Notification

```haskell
-- From FileNotification module
watchDirectory :: 
    FilePath -> 
    (FilePath -> IO ()) ->  -- Callback on change
    IO WatchHandle
```

## Best Practices

### Configuration Files

1. **Use .json extension**: Consistent with loader expectations
2. **Validate on load**: Check required fields
3. **Use UTF-8 encoding**: Standard for JSON
4. **Pretty print**: Human-readable config files
5. **Comments in separate file**: JSON doesn't support comments

### Section Extraction

1. **Clear delimiters**: Use distinctive markers
2. **Nesting awareness**: Don't overlap sections
3. **Graceful degradation**: Handle missing sections
4. **Document format**: Explain section usage

### Directory Structure

```
project/
├── agents-exe.cfg.json      # Project config
├── agents/
│   ├── main.json            # Main agent
│   ├── helpers/
│   │   ├── code-helper.json
│   │   └── doc-helper.json
│   └── shared-tools/        # Shared tool library
├── sessions/                # Session storage
│   └── session-*.json
└── prompts/                 # Reusable prompts
    ├── code-review.md
    └── pr-description.md
```

## Examples

### Loading Multiple Agents

```haskell
import System.Agents.FileLoader
import Prod.Tracer (silentTracer)

loadAllAgents :: FilePath -> IO [AgentDescription]
loadAllAgents dir = do
    jsonFiles <- listJsonDirectory dir
    results <- mapM (loadJsonFile silentTracer) jsonFiles
    return [agent | Right agent <- results]

-- With error reporting
loadAllAgentsWithErrors :: FilePath -> IO ([AgentDescription], [InvalidAgentError])
loadAllAgentsWithErrors dir = do
    jsonFiles <- listJsonDirectory dir
    results <- mapM (loadJsonFile silentTracer) jsonFiles
    let (errors, agents) = partitionEithers results
    return (agents, errors)
```

### Extracting Code from Markdown

```haskell
import System.Agents.FileLoader.Section

extractHaskellCode :: Text -> Maybe Text
extractHaskellCode content = 
    extractSection "```haskell" "```" content
    <|> extractSection "```hs" "```" content

extractAllSections :: Text -> [Text]
extractAllSections content =
    catMaybes $ map (`findSection` content) sectionNames
  where
    sectionNames = ["PROMPT", "CONTEXT", "EXAMPLES", "INSTRUCTIONS"]
```

### Custom Config Loading

```haskell
loadProjectConfig :: IO (Maybe AgentsExeConfig)
loadProjectConfig = do
    mPath <- locateAgentsExeConfig
    case mPath of
        Nothing -> return Nothing
        Just path -> do
            result <- eitherDecodeFileStrict' path
            case result of
                Left err -> do
                    putStrLn $ "Config error: " ++ err
                    return Nothing
                Right config -> return $ Just config
```

## Integration with Other Modules

### AgentTree Integration

```haskell
-- AgentTree uses FileLoader to discover agents
loadAgentTree :: FilePath -> IO (Either AgentTreeError AgentTree)
loadAgentTree rootFile = do
    -- Load root agent
    rootResult <- readJsonDescriptionFile rootFile
    case rootResult of
        Left err -> return $ Left $ LoadError err
        Right rootDesc -> do
            -- Discover child agents
            let toolDir = toolDirectory (agent rootDesc)
            childFiles <- listJsonDirectory toolDir
            -- ... build tree
```

### CLI Integration

```haskell
-- Main.hs uses FileLoader for agent discovery
initArgParserArgs :: IO ArgParserArgs
initArgParserArgs = do
    agentsExecConfig <- locateAgentsExeConfig
    case agentsExecConfig of
        Just path -> loadFromConfig path
        Nothing -> loadDefaults
  where
    loadFromConfig path = do
        config <- eitherDecodeFileStrict' path
        jsonPaths <- traverse listJsonDirectory (agentsDirectories config)
        return $ ArgParserArgs
            { defaultAgentFiles = agentsFiles config <> concat jsonPaths
            , ...
            }
```

