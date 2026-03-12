# Export/Import - Tool Sharing

The export/import system enables sharing agents, tools, and configurations between users, teams, and environments via archives or git repositories.

## Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                      Export/Import System                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Export                          Import                         │
│   ──────                          ──────                         │
│                                                                  │
│   ┌─────────┐                    ┌─────────┐                    │
│   │ Agent   │──┐              ┌─>│ Extract │                    │
│   │ Config  │  │              │  │ Archive │                    │
│   └─────────┘  │    ┌─────┐   │  └────┬────┘                    │
│                ├───>│Archive├──┤       │                         │
│   ┌─────────┐  │    │.tar.gz│   │  ┌────▼────┐                  │
│   │ Tools   │──┘    └─────┘   └─>│ Validate │                  │
│   │ (scripts)│                    │  Schema  │                  │
│   └─────────┘                    └────┬────┘                  │
│                                       │                          │
│   ┌─────────┐                    ┌────▼────┐                  │
│   │ MCP     │                    │ Install │                  │
│   │ Servers │                    │ Files   │                  │
│   └─────────┘                    └─────────┘                  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## Export System

### Export Package Structure

```haskell
data ExportPackage = ExportPackage
    { packageMetadata :: PackageMetadata
    , packageAgents :: [AgentExport]
    , packageTools :: [StandaloneToolExport]
    , packageMcpServers :: [McpServerExport]
    }

data PackageMetadata = PackageMetadata
    { packageVersion :: Text           -- Schema version
    , packageCreatedAt :: UTCTime
    , packageDescription :: Maybe Text
    , packageSource :: Maybe Text
    }
```

### Agent Export

```haskell
data AgentExport = AgentExport
    { agentConfig :: Agent
    , agentNamespace :: Maybe Text
    , agentTools :: [ToolExport]
    }

data ToolExport = ToolExport
    { toolName :: Text
    , toolContent :: ByteString
    , toolPermissions :: FileMode
    , toolMetadata :: Maybe ScriptInfo
    , toolNamespace :: Maybe Text
    }
```

### Standalone Tool Export

```haskell
data StandaloneToolExport = StandaloneToolExport
    { standaloneToolInfo :: ScriptInfo
    , standaloneToolScript :: ByteString
    , standaloneToolPermissions :: FileMode
    , standaloneToolAuxFiles :: [(FilePath, ByteString)]
    }
```

### Export Formats

```haskell
data ArchiveFormat
    = TarFormat
    | TarGzFormat    -- Default
    | ZipFormat

exportToArchive :: 
    ExportPackage -> 
    ArchiveFormat -> 
    FilePath -> 
    IO (Either ArchiveError ())
```

### Archive Structure

```
export.tar.gz
├── metadata.json          # Package metadata
├── agents/
│   └── my-agent/
│       ├── agent.json     # Agent configuration
│       └── tools/
│           ├── list_files
│           └── read_file
├── tools/                 # Standalone tools
│   └── useful-script
└── mcp-servers/
    └── filesystem.json
```

### Git Export

```haskell
data GitUrl = GitUrl
    { gitRemote :: Text
    , gitBranch :: Maybe Text
    , gitPath :: Maybe Namespace
    }

data GitExportOptions = GitExportOptions
    { gitCommitMessage :: Text
    , gitTag :: Maybe Text
    , gitPush :: Bool
    }

exportToGit :: 
    ExportPackage -> 
    GitUrl -> 
    GitExportOptions -> 
    IO (Either GitError ())
```

**Git export flow:**
1. Clone repository
2. Write files to specified path
3. Commit changes
4. Create tag (optional)
5. Push to remote (optional)

## Import System

### Import Sources

```haskell
data ImportSource
    = ImportFromFile FilePath
    | ImportFromGit GitImportSource

data GitImportSource = GitImportSource
    { gitImportUrl :: Text
    , gitImportRef :: Maybe Text    -- branch, tag, or commit
    , gitImportNamespace :: Maybe Text
    }
```

### Import Destinations

```haskell
data ImportDestination
    = ImportToCurrentDir
    | ImportToPath FilePath
    | ImportToConfigDir
    | ImportToAgent FilePath      -- Install to agent's tool dir
    | ImportToToolDir FilePath    -- Install to specific tool dir
```

### Import Modes

```haskell
data ImportMode
    = ImportFailOnConflict   -- Default
    | ImportOverwrite
    | ImportMerge
```

### Import Process

```haskell
importFromArchive :: 
    ImportDestination ->
    InstallOptions ->
    FilePath ->                    -- Archive path
    IO (Either ImportError ())

importFromGit ::
    ImportDestination ->
    InstallOptions ->
    GitUrl ->
    GitImportOptions ->
    IO (Either ImportError ())
```

### Import Validation

1. **Schema validation**: Verify package structure
2. **Namespace collision**: Check for conflicts
3. **Tool validation**: Run `describe` on imported tools
4. **Agent validation**: Verify agent configuration

## Namespace System

Namespaces organize tools and agents hierarchically:

```haskell
newtype Namespace = Namespace [Text]  -- Dot-separated components

-- Examples:
-- "company.team"      -> Namespace ["company", "team"]
-- "org.project.lib"   -> Namespace ["org", "project", "lib"]

parseNamespace :: Text -> Either ParseError Namespace
renderNamespace :: Namespace -> Text
```

### Namespace Usage

```bash
# Export with namespace
agents-exe export --namespace team-a.tools --output ./tools.tar.gz

# Import specific namespace
agents-exe import \
    --git-url https://github.com/company/agents \
    --namespace team-a.tools \
    --to ./my-tools/
```

### Namespaced Archive Structure

```
export.tar.gz
└── team-a/
    └── tools/
        ├── metadata.json
        └── tools/
            ├── list_files
            └── read_file
```

## Tool Installation

### Install Options

```haskell
data InstallOptions = InstallOptions
    { installForce :: Bool      -- Overwrite existing
    , installLink :: Bool       -- Create symlinks instead of copy
    , installPrefix :: Maybe Text
    }

importToolPackage ::
    ToolPackage ->
    ImportDestination ->
    InstallOptions ->
    IO (Either InstallError ())
```

### Installation Process

1. Extract archive to temp directory
2. Validate tool signatures (optional)
3. Copy/link files to destination
4. Set permissions (executable bits)
5. Run tool validation (`describe`)
6. Update agent configuration (if ImportToAgent)

### Tool Discovery

```haskell
discoverTools :: FilePath -> IO [FilePath]
discoverTools dir = do
    files <- listDirectory dir
    filterM isExecutable files
  where
    isExecutable f = do
        perms <- getPermissions (dir </> f)
        return $ executable perms
```

## Git-based Distribution

### Listing Remote Namespaces

```haskell
listGitNamespaces :: GitUrl -> IO (Either GitError [Namespace])
```

```bash
agents-exe import \
    --git-url https://github.com/company/agents \
    --list-namespaces
```

Output:
```
Available namespaces:
  team-a.tools
  team-a.agents
  team-b.utilities
  shared.common
```

### Listing Remote Tools

```haskell
listGitTools :: 
    GitUrl -> 
    IO (Either GitError [(Namespace, ScriptInfo)])
```

```bash
agents-exe import \
    --git-url https://github.com/company/agents \
    --namespace team-a.tools \
    --list-tools
```

Output:
```
Available tools:
  team-a.tools:
    list_files - List files in directory
    read_file - Read file contents
    search_code - Search code with grep
```

### Sparse Checkout

Import only specific tools from a large repository:

```haskell
data GitImportOptions = GitImportOptions
    { gitRef :: Maybe Text
    , gitSparsePaths :: [FilePath]
    }
```

## Archive Formats

### TAR.GZ (Default)

```bash
# Export
agents-exe export --output ./agent.tar.gz

# Import
agents-exe import --from-file ./agent.tar.gz
```

### ZIP

```bash
# Export with explicit format
agents-exe export --format zip --output ./agent.zip

# Auto-detected from extension
agents-exe export --output ./agent.zip  # Detects ZIP format
```

### Plain TAR

```bash
agents-exe export --format tar --output ./agent.tar
```

## Version Compatibility

### Export Schema Version

```haskell
exportSchemaVersion :: Text
exportSchemaVersion = "1.0.0"
```

### Compatibility Check

```haskell
checkVersionCompatibility :: Text -> Either VersionError ()
checkVersionCompatibility ver
    | ver == exportSchemaVersion = Right ()
    | ver < "1.0.0" = Left VersionTooOld
    | ver > exportSchemaVersion = Left VersionTooNew
```

## Security Considerations

### Tool Validation

All imported tools are validated before installation:

```haskell
validateTool :: FilePath -> IO ValidationResult
validateTool path = do
    -- Run describe command
    (exitCode, out, err) <- readProcessWithExitCode path ["describe"] ""
    
    case exitCode of
        ExitSuccess -> do
            case decode (fromString out) of
                Just info -> return $ Valid info
                Nothing -> return $ InvalidParse out
        ExitFailure code -> 
            return $ InvalidExitCode code err
```

### Permission Handling

```haskell
getFileMode :: FilePath -> IO FileMode
setFileMode :: FilePath -> FileMode -> IO ()

-- Preserve executable permissions
installTool :: FilePath -> FilePath -> FileMode -> IO ()
installTool src dst mode = do
    copyFile src dst
    setFileMode dst mode
```

## Best Practices

### Exporting

1. **Use namespaces**: Organize exports with clear namespaces
2. **Include metadata**: Add description and source info
3. **Version control**: Tag exports with version numbers
4. **Test before export**: Run `check` to validate
5. **Document dependencies**: Note required MCP servers

### Importing

1. **Review before install**: Use `--list-tools` to preview
2. **Use namespaces**: Avoid collisions with existing tools
3. **Backup first**: Save existing configuration
4. **Validate after**: Run `check` after import
5. **Test isolated**: Test in separate directory first

### Distribution

1. **Git for teams**: Use git for shared team tools
2. **Archives for releases**: Use archives for versioned releases
3. **Semantic versioning**: Follow semver for exports
4. **Changelogs**: Include change descriptions
5. **Signed exports**: Consider signing archives

## Examples

### Complete Export/Import Workflow

```bash
# 1. Create and test agent
agents-exe check --agent-file ./my-agent.json

# 2. Export agent with tools
agents-exe export \
    --agent-file ./my-agent.json \
    --namespace team.tools \
    --output ./my-agent-1.0.0.tar.gz

# 3. List contents
agents-exe import --from-file ./my-agent-1.0.0.tar.gz --list-tools

# 4. Import on another machine
agents-exe import \
    --from-file ./my-agent-1.0.0.tar.gz \
    --to ~/agents/

# 5. Verify import
agents-exe check --agent-file ~/agents/my-agent.json
```

### Git-based Team Sharing

```bash
# Export to shared repo
agents-exe export \
    --agent-file ./utils.json \
    --namespace team.utilities \
    --git-url https://github.com/company/agent-tools \
    --git-branch main \
    --git-message "Add utility tools v1.2.0" \
    --git-tag "utilities-v1.2.0" \
    --git-push

# Team member imports
agents-exe import \
    --git-url https://github.com/company/agent-tools \
    --namespace team.utilities \
    --install-to-agent ./my-agent.json \
    --merge
```

### Tool Library Pattern

```bash
# Create focused tool library
cd ~/tools/networking/
agents-exe init --agent-file ./network-tools.json
# ... add networking tools ...

# Export library
agents-exe export \
    --agent-file ./network-tools.json \
    --tools-only \
    --namespace lib.networking \
    --output ./networking-tools.tar.gz

# Use library in other agents
agents-exe import \
    --from-file ./networking-tools.tar.gz \
    --install-to-agent ./web-scraper.json
```

## Troubleshooting

### Common Issues

| Issue | Cause | Solution |
|-------|-------|----------|
| "Invalid archive format" | Corrupted or wrong format | Verify with `file` command |
| "Tool validation failed" | Missing dependencies | Check tool requirements |
| "Namespace conflict" | Existing namespace | Use `--overwrite` or change namespace |
| "Permission denied" | No execute bit | Check tool permissions |
| "Git clone failed" | No access or wrong URL | Verify credentials and URL |

