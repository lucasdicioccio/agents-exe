# Export/Import

**Status: Removed**

The export and import commands have been removed from agents-exe. They were wonky and hard to use, so we're taking another approach (see issue #339).

## Alternatives

For sharing agents and tools, consider these alternatives:

### 1. Direct File Copy

Tools and agent configurations are just files. You can copy them directly:

```bash
# Copy tools to another agent
cp ./source-agent/tools/* ./target-agent/tools/

# Copy entire agent configuration
cp -r ./my-agent ./shared-agents/
```

### 2. Git Submodules

For team sharing, use git submodules:

```bash
# Add shared tools as submodule
git submodule add https://github.com/company/shared-tools.git tools/shared

# Update when needed
git submodule update --remote
```

### 3. Package Managers

Use standard package managers for your platform:

- **Nix**: Package tools as nix derivations
- **Homebrew**: Tap for macOS/Linux
- **Apt/Yum**: System packages for tools

### 4. Container Distribution

Package agents with their tools in containers:

```dockerfile
FROM agents-exe:latest
COPY ./tools /app/tools/
COPY ./agent.json /app/
```

## Historical Context

The original export/import system supported:

- Exporting agents, tools, and MCP servers to archives (tar.gz, zip)
- Git-based distribution with namespacing
- Tool installation with conflict resolution
- Namespace-based organization

This functionality was removed because:

1. **Complexity**: The namespace and packaging system added significant complexity
2. **Overlap**: Functionality overlapped with standard tools (tar, git, cp)
3. **Usage patterns**: Users preferred direct file management
4. **Maintenance burden**: High effort to maintain relative to usage

## Migration

If you were using export/import, migrate to:

1. **Simple archives**: Use `tar` directly
   ```bash
   # Instead of: agents-exe export --output pkg.tar.gz
   tar czf pkg.tar.gz ./my-agent/

   # Instead of: agents-exe import --from-file pkg.tar.gz
   tar xzf pkg.tar.gz -C ./destination/
   ```

2. **Git workflows**: Use standard git workflows
   ```bash
   # Clone and copy what you need
   git clone https://github.com/company/agents /tmp/agents
   cp /tmp/agents/tools/* ./my-tools/
   ```

3. **Configuration management**: Use tools like Ansible, Chef, or simple shell scripts

