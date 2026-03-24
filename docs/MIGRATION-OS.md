# OS Migration

The migration to the OS-native ECS architecture is **COMPLETE**.

## Overview

The Agents framework has fully migrated to an Entity-Component-System (ECS) architecture for managing agents, toolboxes, and resources.

## What Changed

1. **Agent Management**: Moved from Runtime-per-agent to OS-native ECS
2. **Tool Registration**: Now uses STM TVars attached to OSAgentNode
3. **Agent Tree**: Uses OS-native structures (OSAgentTree, OSAgentNode)

## Documentation

- `docs/OS-API.md` - Complete OS API reference
- `docs/architecture.md` - Architecture overview

