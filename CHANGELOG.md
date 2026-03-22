# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased] - Recursive Conversation Capture

### Added

#### Recursive Agent Calls (Sub-Agent Sessions)

This release introduces comprehensive support for recursive conversation capture when agents call other agents as tools. This enables proper tracking of LLM-to-LLM conversation hierarchies.

**Core Features:**

- **Parent-Child Session Tracking** (#348)
  - Sessions now include `parentSessionId`, `parentConversationId`, and `parentAgentSlug` fields
  - `mkChildSession` function creates child sessions linked to their parents
  - `isChildSession` function checks if a session is a child session
  - Backward compatible - old sessions without parent fields load correctly

- **Callback Mechanism for Sub-Agent Progress** (#349)
  - `SubAgentSessionConfig` type for configuring sub-agent callbacks
  - `defaultSubAgentConfig` for sensible defaults
  - `turnAgentRuntimeIntoIOToolWithCallbacks` for creating tools with callback support
  - `OnSessionProgress` callback type for receiving session lifecycle events
  - `SessionProgress` type with `SessionStarted`, `SessionUpdated`, `SessionCompleted`, `SessionFailed` events

- **TUI Types Updated** (#350)
  - `ConversationNode` type for tree structure representation
  - `buildConversationTree` function for organizing conversations hierarchically
  - `AppEvent_SubAgentSessionStarted`, `AppEvent_SubAgentSessionUpdated`, `AppEvent_SubAgentSessionCompleted` events
  - `conversationTreeExpanded` field for tracking expanded nodes
  - `selectedConversationPath` field for tree navigation
  - `makeSubAgentCallback` factory for creating TUI-compatible callbacks

- **TUI Event Loop & Rendering** (#351)
  - Tree navigation handlers (`navigateConversationUp`, `navigateConversationDown`, `toggleSelectedConversation`)
  - `handleSubAgentSessionStarted`, `handleSubAgentSessionUpdated`, `handleSubAgentSessionCompleted` event handlers
  - Auto-expansion of parent conversations when sub-agents start
  - Status messages for sub-agent lifecycle events

- **Tracing Enhancements** (#352)
  - `AgentTrace_SubAgentStarted` trace event
  - `AgentTrace_SubAgentCompleted` trace event
  - `AgentTrace_SubAgentFailed` trace event
  - `SubAgentCallTrace` and `SubAgentReturnTrace` conversation traces
  - `traceParentConversationId`, `traceSubAgentConversationId`, `isSubAgentTrace` helper functions

**API Changes:**

```haskell
-- New exports from System.Agents
import System.Agents (
    SubAgentSessionConfig(..),
    defaultSubAgentConfig,
    turnAgentRuntimeIntoIOToolWithCallbacks,
    mkChildSession,
    isChildSession,
)

-- Creating a child session
childSess <- mkChildSession parentSessId parentConvId "parent-agent"

-- Configuring sub-agent callbacks
let config = defaultSubAgentConfig
        { subAgentOnProgress = Just myCallback
        , subAgentStore = Just sessionStore
        }

-- Creating tools with callback support
let toolReg = turnAgentRuntimeIntoIOToolWithCallbacks
        config
        parentTracer
        subAgentRuntime
        parentSlug
        parentAgentId
```

**Documentation:**

- Updated `docs/sessions.md` with nested session documentation
- Updated `docs/tui.md` with conversation tree navigation
- Added migration guide in documentation

### Changed

- `System.Agents.CLI.TUI` now uses `turnAgentRuntimeIntoIOToolWithCallbacks` for proper sub-agent tracking
- `System.Agents` exports new recursive agent call APIs
- Session JSON format now includes parent tracking fields (backward compatible)

### Migration Guide

For users upgrading to this version:

1. **Existing sessions**: Will load without parent information (backward compatible)
2. **TUI users**: Will see conversation tree instead of flat list when agents call sub-agents
3. **Trace consumers**: May see new trace event types for sub-agent lifecycle

### Internal Changes

- `System.Agents.AgentTree.OneShotTool` now provides full callback-based sub-agent support
- `System.Agents.TUI.Event` handles all sub-agent lifecycle events
- `System.Agents.TUI.Types` includes complete tree navigation state
- `System.Agents.Runtime.Trace` includes sub-agent correlation traces

## [Previous Releases]

### Notes

- Previous release notes would be listed here
- This changelog starts tracking from the recursive conversation capture feature (#347)

---

## Contributing

When adding new features or making changes:

1. Add entries under the `[Unreleased]` section
2. Use the categories: Added, Changed, Deprecated, Removed, Fixed, Security
3. Reference issue numbers where applicable
4. Update migration guide if breaking changes are introduced

## Release Checklist

- [ ] Update version numbers
- [ ] Update CHANGELOG.md (move Unreleased to a version)
- [ ] Run full test suite: `cabal test`
- [ ] Verify documentation is current
- [ ] Create git tag
- [ ] Create GitHub release

