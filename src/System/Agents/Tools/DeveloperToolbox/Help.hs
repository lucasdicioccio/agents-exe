{-# LANGUAGE OverloadedStrings #-}

{- |
Help capability for the DeveloperToolbox.

This module provides comprehensive documentation for all developer
toolbox capabilities, returned as a formatted text response.
-}
module System.Agents.Tools.DeveloperToolbox.Help (
    executeHelp,
    buildHelpText,
) where

import Data.Text (Text)
import qualified Data.Text as Text

import System.Agents.Base (DeveloperToolCapability (..))
import System.Agents.Tools.DeveloperToolbox.Types (
    DeveloperToolError (..),
    Toolbox (..),
 )

{- | Execute the help capability.

Returns comprehensive documentation for all developer toolbox capabilities,
with detailed explanations and usage examples.
-}
executeHelp ::
    Toolbox ->
    IO (Either DeveloperToolError Text)
executeHelp toolbox = do
    if DevToolHelp `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "help"
        else do
            let enabledCapabilities = toolboxCapabilities toolbox
            pure $ Right $ buildHelpTextForCapabilities enabledCapabilities

{- | Build comprehensive help text for the given capabilities.

This function generates formatted documentation covering all enabled
capabilities with detailed parameter descriptions and examples.
-}
buildHelpTextForCapabilities :: [DeveloperToolCapability] -> Text
buildHelpTextForCapabilities enabledCapabilities =
    let header =
            Text.unlines
                [ "Developer Toolbox Help"
                , "======================"
                , ""
                , "This toolbox provides utilities for writing, validating, and scaffolding"
                , "agents and tools. Below are detailed descriptions of all available capabilities."
                , ""
                , "Enabled capabilities in this toolbox:"
                , "  " <> Text.intercalate ", " (map capabilityToShortName enabledCapabilities)
                , ""
                , "=============================================="
                , ""
                ]

        capabilitySections = map buildCapabilityHelp enabledCapabilities

        footer =
            Text.unlines
                [ ""
                , "=============================================="
                , ""
                , "TIPS AND BEST PRACTICES"
                , "======================="
                , ""
                , "1. File Range Operations"
                , "   - Use 'head' to create NEW files (e.g., ranges=\"head\" for write-file-range)"
                , "   - Use 'tail' to append to existing files"
                , "   - Use 'whole' to completely overwrite a file"
                , "   - Line numbers are 1-based (first line is line 1)"
                , "   - When editing multiple ranges, they are processed top-to-bottom"
                , "   - Line numbers automatically adjust after each edit"
                , ""
                , "2. Content Blocks"
                , "   - For write-file-range, provide one content block per range"
                , "   - Use empty strings (\"\") to delete lines"
                , "   - Multi-line content is supported within each block"
                , ""
                , "3. File Sandbox"
                , "   - read-file-range, write-file-range, and patch-file require a file sandbox"
                , "   - The sandbox restricts which files can be accessed"
                , "   - Configure the sandbox in your agent's DeveloperToolbox settings"
                , ""
                , "4. Patch vs Write-File-Range"
                , "   - Use patch-file when you need context validation (safer)"
                , "   - Use write-file-range for simple line replacements (faster)"
                , "   - patch-file uses unified diff format"
                , ""
                , "5. Validation First"
                , "   - Use validate-tool before deploying new tools"
                , "   - Use validate-agent to check agent configurations"
                , ""
                , "6. Scaffolding"
                , "   - scaffold-agent creates agent JSON from templates (openai, mistral, ollama)"
                , "   - scaffold-tool creates tool scripts (bash, python, haskell)"
                , "   - Use these as starting points, then customize"
                , ""
                , "7. Optimistic Locking with Snapshots"
                , "   - Enable 'snapshot' capability to get snapshot references"
                , "   - Use expectedSnapshotRef parameter to prevent concurrent edit conflicts"
                , "   - If SnapshotMismatchError occurs, re-read and retry your edit"
                , "   - Chain operations using afterSnapshotRef from previous operation"
                , ""
                ]
     in header <> Text.unlines capabilitySections <> footer

-- | Build comprehensive help text for a single capability.
buildCapabilityHelp :: DeveloperToolCapability -> Text
buildCapabilityHelp cap = case cap of
    DevToolValidateTool ->
        Text.unlines
            [ "validate-tool"
            , "---------------"
            , "Validates a bash tool script by loading its description."
            , ""
            , "Parameters:"
            , "  - tool_path (string, required): Path to the tool script to validate"
            , ""
            , "Returns: ValidationResult with:"
            , "  - path: The file path validated"
            , "  - valid: Boolean indicating if validation passed"
            , "  - slug: The tool's slug (if valid)"
            , "  - error: Error message (if invalid)"
            , ""
            , "Example:"
            , "  {\"capability\": \"validate-tool\", \"tool_path\": \"./tools/my-tool.sh\"}"
            , ""
            ]
    DevToolScaffoldAgent ->
        Text.unlines
            [ "scaffold-agent"
            , "--------------"
            , "Generates agent scaffolding from a template."
            , ""
            , "Parameters:"
            , "  - template (string, optional): Template to use - 'openai' (default), 'mistral', or 'ollama'"
            , "  - slug (string, optional): Name/slug for the agent (default: 'new-agent')"
            , "  - file_path (string, optional): Output file path (default: 'new-agent.json')"
            , "  - force (boolean, optional): Overwrite existing file (default: false)"
            , ""
            , "Returns: ScaffoldResult with:"
            , "  - success: Boolean indicating if scaffolding succeeded"
            , "  - path: Path where the agent file was created"
            , "  - error: Error message (if failed)"
            , ""
            , "Example:"
            , "  {\"capability\": \"scaffold-agent\", \"template\": \"openai\", \"slug\": \"my-agent\", \"file_path\": \"./my-agent.json\"}"
            , ""
            ]
    DevToolScaffoldTool ->
        Text.unlines
            [ "scaffold-tool"
            , "-------------"
            , "Generates tool scaffolding in the specified language."
            , ""
            , "Parameters:"
            , "  - language (string, optional): Language - 'bash' (default), 'python', or 'haskell'"
            , "  - slug (string, optional): Name/slug for the tool (default: 'new-tool')"
            , "  - file_path (string, optional): Output file path (default: 'new-tool.sh')"
            , "  - force (boolean, optional): Overwrite existing file (default: false)"
            , ""
            , "Returns: ScaffoldResult with:"
            , "  - success: Boolean indicating if scaffolding succeeded"
            , "  - path: Path where the tool file was created"
            , "  - error: Error message (if failed)"
            , ""
            , "Example:"
            , "  {\"capability\": \"scaffold-tool\", \"language\": \"bash\", \"slug\": \"my-tool\", \"file_path\": \"./tools/my-tool.sh\"}"
            , ""
            ]
    DevToolShowSpec ->
        Text.unlines
            [ "show-spec"
            , "---------"
            , "Displays specification documentation for tool development."
            , ""
            , "Parameters:"
            , "  - spec_name (string, required): Name of the spec to show - 'bash-tools'"
            , ""
            , "Returns: Text containing the specification documentation"
            , ""
            , "Example:"
            , "  {\"capability\": \"show-spec\", \"spec_name\": \"bash-tools\"}"
            , ""
            ]
    DevToolValidateAgent ->
        Text.unlines
            [ "validate-agent"
            , "--------------"
            , "Validates an agent JSON configuration file."
            , ""
            , "Parameters:"
            , "  - agent_path (string, required): Path to the agent JSON file to validate"
            , ""
            , "Returns: AgentValidationResult with:"
            , "  - path: The file path validated"
            , "  - valid: Boolean indicating if validation passed"
            , "  - slug: The agent's slug (if valid)"
            , "  - errors: List of validation errors"
            , "  - warnings: List of validation warnings"
            , ""
            ]
    DevToolCreateAgent ->
        Text.unlines
            [ "create-agent"
            , "------------"
            , "Creates a new agent configuration from scratch or from a reference."
            , ""
            , "Parameters:"
            , "  - file_path (string, required): Path where to create the agent JSON file"
            , "  - reference_path (string, optional): Path to a reference agent to use as template"
            , "  - Various override parameters for agent fields"
            , ""
            , "Returns: CreateResult with:"
            , "  - success: Boolean indicating if creation succeeded"
            , "  - path: Path where the agent file was created"
            , "  - slug: The agent's slug"
            , "  - error: Error message (if failed)"
            , ""
            ]
    DevToolCreateTool ->
        Text.unlines
            [ "create-tool"
            , "-----------"
            , "Creates a new tool script from scratch or from a reference."
            , ""
            , "Parameters:"
            , "  - file_path (string, required): Path where to create the tool script"
            , "  - reference_path (string, optional): Path to a reference tool to use as template"
            , "  - Various configuration parameters for the tool"
            , ""
            , "Returns: CreateResult with:"
            , "  - success: Boolean indicating if creation succeeded"
            , "  - path: Path where the tool was created"
            , "  - slug: The tool's slug"
            , "  - error: Error message (if failed)"
            , ""
            ]
    DevToolReadFileRange ->
        Text.unlines
            [ "read-file-range"
            , "---------------"
            , "Reads specific line ranges from a file."
            , ""
            , "Parameters:"
            , "  - path (string, required): Path to the file to read"
            , "  - ranges (string, optional): Line ranges to read. Omit to read entire file."
            , ""
            , "Range Formats:"
            , "  - Omit or empty: Reads entire file (default, recommended for full file read)"
            , "  - \"whole\" - Reads entire file with line numbers"
            , "  - \"N\" - Reads single line N (1-based)"
            , "  - \"N-M\" - Reads inclusive range from line N to line M"
            , "  - \"1-50\" - Reads first 50 lines"
            , "  - \"head\" - Returns empty for read (use \"whole\" or omit ranges for full file)"
            , "  - \"tail\" - Returns empty for read (use \"whole\" or omit ranges for full file)"
            , "  - \"N+\" - Not applicable for read operations (write-only)"
            , "  - Multiple ranges: \"1-10,20-30\" - Reads lines 1-10 and 20-30"
            , ""
            , "Returns: ReadFileRangeResult with:"
            , "  - path: The file path"
            , "  - content: File content with line numbers (format: 'N\\tline content')"
            , "  - linesRead: Number of lines actually returned"
            , "  - totalFileSize: Total file size in bytes"
            , "  - totalLineCount: Total number of lines in the file"
            , "  - rangesParsed: Normalized range specifications that were applied"
            , ""
            , "Examples:"
            , "  -- Read entire file (recommended - omit ranges parameter):"
            , "  {\"capability\": \"read-file-range\", \"path\": \"./file.txt\"}"
            , ""
            , "  -- Read entire file (explicit):"
            , "  {\"capability\": \"read-file-range\", \"path\": \"./file.txt\", \"ranges\": \"whole\"}"
            , ""
            , "  -- Read specific lines:"
            , "  {\"capability\": \"read-file-range\", \"path\": \"./file.txt\", \"ranges\": \"10-20\"}"
            , ""
            , "  -- Read first 50 lines:"
            , "  {\"capability\": \"read-file-range\", \"path\": \"./file.txt\", \"ranges\": \"1-50\"}"
            , ""
            ]
    DevToolWriteFileRange ->
        Text.unlines
            [ "write-file-range"
            , "----------------"
            , "Replaces specific lines in a file with new content."
            , ""
            , "⚠️ IMPORTANT: To CREATE a new file, use ranges='head' ⚠️"
            , ""
            , "Parameters:"
            , "  - path (string, required): Path to the file to modify"
            , "  - ranges (string, required): Line ranges to replace (see formats below)"
            , "  - contentBlocks (array of strings, required): One content block per range"
            , "  - expectedSnapshotRef (string, optional): For optimistic locking - MD5 hash"
            , "    of expected file content. Operation fails if content doesn't match."
            , "    Use value from previous write's beforeSnapshotRef or afterSnapshotRef."
            , ""
            , "Range Formats:"
            , "  - Single line: '5' - Replaces line 5"
            , "  - Line range: '1-10' - Replaces lines 1 through 10"
            , "  - Multiple ranges: '1-5,20-30' - Replaces multiple ranges (need 2 content blocks)"
            , "  - Head: 'head' - Prepends content BEFORE line 1 (use for NEW files)"
            , "  - Tail: 'tail' - Appends content AFTER last line"
            , "  - Whole: 'whole' - Overwrites entire file"
            , "  - Insert after: 'N+' - Inserts after line N (e.g., '5+')"
            , ""
            , "Content Blocks:"
            , "  - Provide one content block for each range specified"
            , "  - Use empty string (\"\") to DELETE lines"
            , "  - Multi-line strings are supported within each block"
            , ""
            , "Optimistic Locking:"
            , "  - Enable 'snapshot' capability to get snapshot references"
            , "  - Pass expectedSnapshotRef to prevent editing stale content"
            , "  - If SnapshotMismatchError occurs, re-read file and retry"
            , "  - Use afterSnapshotRef from one operation as expectedSnapshotRef for next"
            , ""
            , "Returns: WriteFileRangeResult with:"
            , "  - path: The file path"
            , "  - rangesModified: Number of ranges processed"
            , "  - linesWritten: Total lines written"
            , "  - finalLineCount: Total lines in file after edits"
            , "  - rangeResults: Detailed per-range edit information"
            , "  - beforeSnapshotRef: MD5 of content before edit (if snapshot enabled)"
            , "  - afterSnapshotRef: MD5 of content after edit (if snapshot enabled)"
            , ""
            , "Examples:"
            , "  -- Replace line 5:"
            , "  {\"capability\": \"write-file-range\", \"path\": \"file.txt\", \"ranges\": \"5\", \"contentBlocks\": [\"new line 5\"]}"
            , ""
            , "  -- Delete lines 3 and 7:"
            , "  {\"capability\": \"write-file-range\", \"path\": \"file.txt\", \"ranges\": \"3,7\", \"contentBlocks\": [\"\", \"\"]}"
            , ""
            , "  -- Create NEW file (⚠️ use ranges='head'):"
            , "  {\"capability\": \"write-file-range\", \"path\": \"newfile.txt\", \"ranges\": \"head\", \"contentBlocks\": [\"line 1\\nline 2\"]}"
            , ""
            , "  -- Replace ranges with multi-line content:"
            , "  {\"capability\": \"write-file-range\", \"path\": \"file.txt\", \"ranges\": \"1-2,5-6\", \"contentBlocks\": [\"new 1\\nnew 2\", \"new 5\\nnew 6\"]}"
            , ""
            , "  -- Safe edit with optimistic locking:"
            , "  {\"capability\": \"write-file-range\", \"path\": \"file.txt\", \"ranges\": \"5\", \"contentBlocks\": [\"new line\"], \"expectedSnapshotRef\": \"a1b2c3d4...\"}"
            , ""
            ]
    DevToolPatchFile ->
        Text.unlines
            [ "patch-file"
            , "----------"
            , "Applies a unified diff patch to a file atomically with context validation."
            , ""
            , "Parameters:"
            , "  - path (string, required): Path to the file to patch"
            , "  - patch (string, required): Unified diff patch content"
            , "  - expectedSnapshotRef (string, optional): For optimistic locking - MD5 hash"
            , "    of expected file content. Operation fails if content doesn't match."
            , "    Use value from previous write/patch beforeSnapshotRef or afterSnapshotRef."
            , ""
            , "Patch Format (Unified Diff):"
            , "  - File headers (--- and +++ lines) are ignored"
            , "  - Hunk headers start with @@ (e.g., @@ -10,5 +11,6 @@)"
            , "  - Context lines have no prefix"
            , "  - Removed lines start with -"
            , "  - Added lines start with +"
            , ""
            , "Features:"
            , "  - Atomic application: All hunks validated before any changes"
            , "  - Context validation: Each hunk's context must match exactly"
            , "  - Overlap detection: Overlapping hunks are rejected"
            , "  - Bottom-to-top: Applied in descending order to avoid line shifts"
            , ""
            , "Optimistic Locking:"
            , "  - Enable 'snapshot' capability to get snapshot references"
            , "  - Pass expectedSnapshotRef to prevent editing stale content"
            , "  - If SnapshotMismatchError occurs, re-read file and retry"
            , "  - Use afterSnapshotRef from one operation as expectedSnapshotRef for next"
            , ""
            , "Returns: PatchResult with:"
            , "  - path: The file path"
            , "  - hunksApplied: Number of hunks successfully applied"
            , "  - hunksRejected: Number of hunks rejected"
            , "  - linesChanged: Total lines changed"
            , "  - beforeSnapshotRef: MD5 of content before patch (if snapshot enabled)"
            , "  - afterSnapshotRef: MD5 of content after patch (if snapshot enabled)"
            , ""
            , "Example:"
            , "  {\"capability\": \"patch-file\", \"path\": \"file.hs\", \"patch\": \"--- a/file.hs\\n+++ b/file.hs\\n@@ -10,5 +10,6 @@ import Foo\\n+import Data.Text (Text)\"}"
            , ""
            ]
    DevToolHelp ->
        Text.unlines
            [ "help"
            , "----"
            , "Returns this comprehensive help documentation for all enabled capabilities."
            , ""
            , "Parameters: None"
            , ""
            , "Returns: Text containing detailed documentation for all activated capabilities."
            , ""
            ]
    DevToolSnapshot ->
        Text.unlines
            [ "snapshot"
            , "--------"
            , "Enables snapshot functionality for write-file-range and patch-file operations."
            , ""
            , "When enabled, file content is stored in memory (RAM) before edits."
            , "This allows rollback via restore-file using the returned snapshot reference."
            , ""
            , "Snapshot references are MD5 hashes of the original file content."
            , "Snapshots are stored per-conversation (not persisted)."
            , ""
            , "Optimistic Locking:"
            , "  - Snapshots enable optimistic locking via expectedSnapshotRef parameter"
            , "  - Before writing, current file MD5 is compared to expectedSnapshotRef"
            , "  - If they differ, SnapshotMismatchError is returned"
            , "  - This prevents lost updates when multiple agents edit the same file"
            , ""
            , "Returns: Text confirming snapshot capability is enabled"
            , ""
            , "Note: This capability doesn't take direct tool calls. Instead, it modifies"
            , "the behavior of write-file-range and patch-file to save snapshots and"
            , "enable optimistic locking via expectedSnapshotRef parameter."
            , ""
            ]
    DevToolRestoreFile ->
        Text.unlines
            [ "restore-file"
            , "------------"
            , "Restores a file to a previous version using a snapshot reference."
            , ""
            , "Parameters:"
            , "  - path (string, required): Path to the file to restore"
            , "  - snapshot_ref (string, required): Snapshot reference (MD5 hash from write/patch result)"
            , ""
            , "Returns: RestoreResult with:"
            , "  - path: The file path"
            , "  - snapshotRef: The snapshot reference used"
            , "  - success: Boolean indicating if restore succeeded"
            , "  - error: Error message (if failed)"
            , ""
            , "Example:"
            , "  {\"capability\": \"restore-file\", \"path\": \"file.txt\", \"snapshot_ref\": \"a1b2c3d4...\"}"
            , ""
            , "Note: Requires the 'snapshot' capability to be enabled to have stored snapshots."
            , ""
            ]
    DevToolListDirectory ->
        Text.unlines
            [ "list-directory"
            , "--------------"
            , "Lists directory contents with metadata."
            , ""
            , "Parameters:"
            , "  - path (string, required): Directory path to list"
            , "  - recursive (bool, optional): Recurse into subdirectories (default: false)"
            , "  - include_hidden (bool, optional): Include hidden files/dotfiles (default: false)"
            , "  - name_patterns (array of strings, optional): Glob patterns to filter by name"
            , ""
            , "Returns: DirectoryListingResult with:"
            , "  - path: The directory path listed"
            , "  - entries: Array of file entry metadata objects"
            , "  - entryCount: Number of entries returned"
            , "  - recursive: Whether recursive listing was used"
            , ""
            ]
    DevToolTraverseDirectory ->
        Text.unlines
            [ "traverse-directory"
            , "------------------"
            , "Recursively traverses a directory tree and returns all entries within scope."
            , ""
            , "Parameters:"
            , "  - path (string, required): Root directory path to traverse"
            , ""
            , "Returns: DirectoryListingResult with:"
            , "  - path: The root directory path"
            , "  - entries: Array of all file entry metadata objects"
            , "  - entryCount: Total number of entries found"
            , "  - recursive: Always true"
            , ""
            ]

-- | Get short name for a capability.
capabilityToShortName :: DeveloperToolCapability -> Text
capabilityToShortName DevToolValidateTool = "validate-tool"
capabilityToShortName DevToolScaffoldAgent = "scaffold-agent"
capabilityToShortName DevToolScaffoldTool = "scaffold-tool"
capabilityToShortName DevToolShowSpec = "show-spec"
capabilityToShortName DevToolValidateAgent = "validate-agent"
capabilityToShortName DevToolCreateAgent = "create-agent"
capabilityToShortName DevToolCreateTool = "create-tool"
capabilityToShortName DevToolReadFileRange = "read-file-range"
capabilityToShortName DevToolWriteFileRange = "write-file-range"
capabilityToShortName DevToolPatchFile = "patch-file"
capabilityToShortName DevToolHelp = "help"
capabilityToShortName DevToolSnapshot = "snapshot"
capabilityToShortName DevToolRestoreFile = "restore-file"
capabilityToShortName DevToolListDirectory = "list-directory"
capabilityToShortName DevToolTraverseDirectory = "traverse-directory"

{- | Build comprehensive help text for all capabilities.

This generates documentation covering ALL possible capabilities,
useful for reference documentation or when all capabilities are enabled.
-}
buildHelpText :: Text
buildHelpText =
    let allCapabilities =
            [ DevToolValidateTool
            , DevToolScaffoldAgent
            , DevToolScaffoldTool
            , DevToolShowSpec
            , DevToolValidateAgent
            , DevToolCreateAgent
            , DevToolCreateTool
            , DevToolReadFileRange
            , DevToolWriteFileRange
            , DevToolPatchFile
            , DevToolHelp
            , DevToolSnapshot
            , DevToolRestoreFile
            ]
     in buildHelpTextForCapabilities allCapabilities
