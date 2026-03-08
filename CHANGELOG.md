# Revision history for agents

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

## Unreleased

### Added

* **Tool Manipulation Commands** - New `toolbox` command group for direct tool manipulation without agent instantiation
  * `toolbox list` - List available tools from agents, directories, or MCP configs
    * Supports multiple output formats: table, json, names
    * Can list tools from agent files, tool directories, or MCP configurations
  * `toolbox nest` - Create nested bash-compatible tools from multiple tools
    * Wraps multiple tools into a single executable script
    * Supports include/exclude filtering
    * Maintains standard bash-tool interface (describe/run commands)
  * `toolbox mcp` - Run MCP server directly from tools without agent indirection
    * Supports multiple tool sources (agents, directories, MCP toolboxes, OpenAPI specs, PostgREST configs)
    * Configurable name mapping strategies (keep/strip prefixes, use original names)
    * Stdio transport support for MCP protocol

### Documentation

* Added README.md with quick start guide and tool manipulation examples
* Added comprehensive CLI reference documentation (docs/cli-reference.md)
* Added internal tool mapping documentation (docs/internal-tool-mapping.md)

### Modules Added

* `System.Agents.Tools.List` - Tool listing functionality
* `System.Agents.Tools.Nest` - Tool nesting/composition functionality  
* `System.Agents.Tools.McpServer` - Direct MCP server for toolboxes

Closes #145
Relates-to: #117, #135, #139, #144

