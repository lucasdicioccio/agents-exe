# Migration Guide

The migration from Runtime-per-agent to OS-native ECS architecture is **COMPLETE**.

## Status

All legacy Runtime code has been removed. The system now uses OS-native structures exclusively.

## Migration Complete

- Phase 1 (Dual Mode): COMPLETE
- Phase 2+ (New-Only): COMPLETE  
- Final Phase (Cleanup): COMPLETE

## Current Architecture

The system now uses:
- `System.Agents.OS.Core` - ECS world and components
- `System.Agents.OS.Agents` - OS-native agent management
- `System.Agents.AgentTree` - OS-native agent tree loading

See `docs/OS-API.md` for the complete API reference.

