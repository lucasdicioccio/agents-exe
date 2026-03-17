# Advanced Configuration

This guide covers advanced configuration patterns for agents-exe, including recursive agent references, mutual recursion, and runtime safety controls.

## Table of Contents

- [Recursive Agent References](#recursive-agent-references)
  - [Self-References](#self-references)
  - [Mutual Recursion](#mutual-recursion)
  - [Configuration via extraAgents](#configuration-via-extraagents)
- [Recursion Safety](#recursion-safety)
  - [Depth Limits](#depth-limits)
  - [Call Stack Tracking](#call-stack-tracking)
- [Configuration Examples](#configuration-examples)
  - [Basic Recursive Agent](#basic-recursive-agent)
  - [Mutually Recursive Agents](#mutually-recursive-agents)
  - [Hierarchical Agent with Recursion](#hierarchical-agent-with-recursion)
- [Best Practices](#best-practices)
- [Troubleshooting](#troubleshooting)

---

## Recursive Agent References

By default, agents can only reference sub-agents located in their `toolDirectory`. This creates a strict tree hierarchy. However, some use cases require:

1. **Self-references**: An agent that can call itself for iterative refinement
2. **Mutual recursion**: Agent A calls Agent B, which calls Agent A
3. **Cross-tree references**: Agents in different branches need to collaborate

The `extraAgents` configuration field enables these patterns by allowing explicit declarations of agent references outside the normal directory hierarchy.

### How It Works

When an agent defines `extraAgents`, the system uses a **two-phase initialization**:

1. **Discovery Phase**: All agent configurations are loaded via BFS traversal
2. **Validation Phase**: References are validated (all slugs must exist)
3. **Runtime Shell Phase**: Runtime shells are created with empty tool lists
4. **Wiring Phase**: Tool references are resolved via the `RuntimeRegistry`
5. **Tree Building Phase**: The final `AgentTree` is constructed

This approach allows cycles in the agent reference graph while preventing runtime failures from missing references.

### Self-References

An agent can reference itself to implement iterative refinement patterns:

```
┌─────────────────┐
│  refine-code    │◄────────────────┐
│  (self-ref)     │                 │
└────────┬────────┘                 │
         │                          │
         │ tool call                │
         ▼                          │
┌─────────────────┐                 │
│  refine-code    │─────────────────┘
│  (same agent)   │
└─────────────────┘
```

Use cases:
- Iterative code improvement ("make it faster", "add tests")
- Progressive document refinement
- Recursive problem decomposition with depth limits

### Mutual Recursion

Two or more agents can reference each other:

```
┌─────────────┐      ┌─────────────┐
│  planner    │─────►│  executor   │
│             │◄─────│             │
└─────────────┘      └─────────────┘
```

Use cases:
- Planning and execution loops
- Code generation and review cycles
- Question-answering with fact-checking

### Configuration via extraAgents

The `extraAgents` field in an agent's JSON configuration declares references to agents outside the `toolDirectory` hierarchy.

**Field Structure:**

```haskell
data ExtraAgentRef = ExtraAgentRef
    { extraAgentSlug :: AgentSlug   -- ^ How to reference this agent
    , extraAgentPath :: FilePath    -- ^ Path to agent JSON file
    }
```

> **Important**: The `slug` field in `extraAgents` must match the target agent's 
> actual `slug` as defined in its JSON file. It is NOT an alias. The system uses 
> this slug to look up the agent in the registry after all configurations are loaded.
>
> For example, if you want to reference an agent defined in `helper.json` that has
> `"slug": "my-helper"`, your extraAgents entry must use `"slug": "my-helper"`,
> not any other name.

**JSON Format:**

```json
{
  "tag": "OpenAIAgentDescription",
  "contents": {
    "slug": "my-agent",
    "api-key-id": "my-key",
    "flavor": "OpenAI",
    "model-url": "https://api.openai.com",
    "model-name": "gpt-4",
    "tool-directory": "tools",
    "extraAgents": [
      {
        "slug": "helper-agent",
        "path": "../helpers/helper-agent.json"
      },
      {
        "slug": "my-agent",
        "path": "./agent.json"
      }
    ],
    "announce": "A recursive agent that can call itself.",
    "system-prompt": [
      "You are a helpful assistant.",
      "You can call yourself recursively for complex tasks."
    ]
  }
}
```

**Field Descriptions:**

| Field | Type | Description |
|-------|------|-------------|
| `slug` | string | Must match the target agent's actual `slug` as defined in its JSON file |
| `path` | string | Relative or absolute path to the agent's JSON file |

> **Warning**: A common mistake is to use an arbitrary name for the `slug` field
> in `extraAgents`. This will cause a `MissingAgentReference` error because the
> system cannot find an agent with that slug. Always ensure the slug in
> `extraAgents` matches the `slug` field in the target agent's configuration file.

---

## Recursion Safety

Unbounded recursion can lead to infinite loops and excessive API costs. The system provides multiple safety mechanisms.

### Depth Limits

The `ToolExecutionContext` tracks recursion depth via a call stack:

```haskell
data ToolExecutionContext = ToolExecutionContext
    { ctxCallStack :: [CallStackEntry]  -- ^ Call chain tracking
    , ctxMaxDepth  :: Maybe Int         -- ^ Optional depth limit
    -- ... other fields
    }

data CallStackEntry = CallStackEntry
    { callAgentSlug      :: Text
    , callConversationId :: ConversationId
    , callDepth          :: Int
    }
```

When an agent calls another agent, the system:

1. Creates a new `CallStackEntry` with incremented depth
2. Checks against `ctxMaxDepth` if set
3. Returns `Left (MaxRecursionDepthExceeded callStack)` if limit exceeded
4. Otherwise, continues with the new context

**Important**: Currently, depth limits are enforced at runtime via the context, not at configuration time. Future versions may support `maxRecursionDepth` in agent configuration.

### Call Stack Tracking

Every nested agent call appends to the call stack:

```
Depth 0: [root]                    -- Initial context
Depth 1: [agent-a, root]           -- After first call
Depth 2: [agent-b, agent-a, root]  -- After second call
```

The call stack enables:

- **Debugging**: Full call chain in error messages
- **Cycle detection**: Check if an agent is already in the stack
- **Context-aware behavior**: Different logic at different depths

**Helper Functions:**

```haskell
-- Check current recursion depth
currentRecursionDepth :: ToolExecutionContext -> Int

-- Check if at or beyond specific depth
isAtDepth :: Int -> ToolExecutionContext -> Bool

-- Get full call chain (root first)
callChain :: ToolExecutionContext -> [CallStackEntry]

-- Check for cycles (agent already in stack)
isAgentInCallStack :: Text -> ToolExecutionContext -> Bool
```

---

## Configuration Examples

### Basic Recursive Agent

An agent that can call itself for iterative code refinement:

**Directory Structure:**

```
refine-agent/
├── agent.json
└── tools/
    └── some-tool.sh
```

**agent.json:**

```json
{
  "tag": "OpenAIAgentDescription",
  "contents": {
    "slug": "refine-code",
    "api-key-id": "openai-key",
    "flavor": "OpenAI",
    "model-url": "https://api.openai.com",
    "model-name": "gpt-4o",
    "tool-directory": "tools",
    "extraAgents": [
      {
        "slug": "refine-code",
        "path": "./agent.json"
      }
    ],
    "announce": "Code refinement agent with self-recursion.",
    "system-prompt": [
      "You are a code refinement specialist.",
      "Analyze the provided code and suggest improvements.",
      "If the task is complex, break it into steps and call",
      "the 'refine-code' tool iteratively, up to 3 levels deep.",
      "Track your progress and stop when improvements diminish."
    ]
  }
}
```

Note that the `slug` in `extraAgents` (`"refine-code"`) matches the agent's own
`slug` field. This is required for self-references to work correctly.

**Usage:**

```bash
agents-exe run -p "Refactor this Python function for performance:" \
               -f myscript.py \
               --agent-file refine-agent/agent.json
```

### Mutually Recursive Agents

A planner and executor that collaborate in a loop:

**Directory Structure:**

```
plan-exec/
├── planner.json
├── executor.json
└── tools/
    └── validate.sh
```

**planner.json:**

```json
{
  "tag": "OpenAIAgentDescription",
  "contents": {
    "slug": "task-planner",
    "api-key-id": "openai-key",
    "flavor": "OpenAI",
    "model-url": "https://api.openai.com",
    "model-name": "gpt-4o",
    "tool-directory": "tools",
    "extraAgents": [
      {
        "slug": "task-executor",
        "path": "./executor.json"
      }
    ],
    "announce": "Task planner that delegates to executor.",
    "system-prompt": [
      "You are a strategic planner.",
      "Break down complex tasks into executable steps.",
      "Use the 'task-executor' tool to execute each step.",
      "Review results and iterate up to 5 times if needed."
    ]
  }
}
```

**executor.json:**

```json
{
  "tag": "OpenAIAgentDescription",
  "contents": {
    "slug": "task-executor",
    "api-key-id": "openai-key",
    "flavor": "OpenAI",
    "model-url": "https://api.openai.com",
    "model-name": "gpt-4o",
    "tool-directory": "tools",
    "extraAgents": [
      {
        "slug": "task-planner",
        "path": "./planner.json"
      }
    ],
    "announce": "Task executor that can request replanning.",
    "system-prompt": [
      "You are an execution specialist.",
      "Implement the planned steps precisely.",
      "If execution fails or needs adjustment,",
      "call 'task-planner' with context for revised planning.",
      "Stop after 5 iterations to prevent infinite loops."
    ]
  }
}
```

Note that in `planner.json`, the `extraAgents` slug is `"task-executor"` which
must match the `slug` field in `executor.json`. Similarly, in `executor.json`,
the `extraAgents` slug is `"task-planner"` which must match the `slug` field in
`planner.json`.

**Usage:**

```bash
agents-exe run -p "Set up a Django project with user auth:" \
               --agent-file plan-exec/planner.json
```

### Hierarchical Agent with Recursion

A boss agent with recursive worker agents at different levels:

**Directory Structure:**

```
hierarchy/
├── boss.json
├── senior-dev.json
├── junior-dev.json
└── tools/
    ├── git.sh
    └── test.sh
```

**boss.json:**

```json
{
  "tag": "OpenAIAgentDescription",
  "contents": {
    "slug": "boss",
    "api-key-id": "openai-key",
    "flavor": "OpenAI",
    "model-url": "https://api.openai.com",
    "model-name": "gpt-4o",
    "tool-directory": "tools",
    "extraAgents": [
      {
        "slug": "senior-dev",
        "path": "./senior-dev.json"
      }
    ],
    "announce": "Project manager coordinating development.",
    "system-prompt": [
      "You are a technical project manager.",
      "Delegate architecture decisions to senior-dev.",
      "Track overall progress and ensure quality."
    ]
  }
}
```

**senior-dev.json:**

```json
{
  "tag": "OpenAIAgentDescription",
  "contents": {
    "slug": "senior-dev",
    "api-key-id": "openai-key",
    "flavor": "OpenAI",
    "model-url": "https://api.openai.com",
    "model-name": "gpt-4o-mini",
    "tool-directory": "tools",
    "extraAgents": [
      {
        "slug": "junior-dev",
        "path": "./junior-dev.json"
      },
      {
        "slug": "senior-dev",
        "path": "./senior-dev.json"
      }
    ],
    "announce": "Senior developer for architecture and review.",
    "system-prompt": [
      "You are a senior software developer.",
      "Design architecture and review implementations.",
      "Delegate implementation tasks to junior-dev.",
      "Use self-recursion for complex code reviews (max 2 levels)."
    ]
  }
}
```

**junior-dev.json:**

```json
{
  "tag": "OpenAIAgentDescription",
  "contents": {
    "slug": "junior-dev",
    "api-key-id": "openai-key",
    "flavor": "OpenAI",
    "model-url": "https://api.openai.com",
    "model-name": "gpt-4o-mini",
    "tool-directory": "tools",
    "extraAgents": [],
    "announce": "Junior developer for implementation.",
    "system-prompt": [
      "You are a junior software developer.",
      "Implement features following senior-dev specifications.",
      "Write tests and documentation for all code."
    ]
  }
}
```

---

## Best Practices

### 1. Always Set Depth Limits

When implementing recursive patterns, explicitly track depth and set limits:

```haskell
-- In tool implementation
case pushAgentContext "my-agent" newConvId parentCtx of
    Left (MaxRecursionDepthExceeded stack) -> 
        return $ "Max depth reached. Current chain: " ++ show (map callAgentSlug stack)
    Right newCtx -> continueWithSubAgent newCtx
```

### 2. Document Recursion Patterns

Add clear documentation in system prompts:

```json
{
  "system-prompt": [
    "You can call yourself recursively using the 'my-agent' tool.",
    "IMPORTANT: Stop after 3 levels of recursion.",
    "Track recursion depth in your reasoning."
  ]
}
```

### 3. Use Appropriate Models

Different recursion depths benefit from different models:

| Depth | Recommendation |
|-------|----------------|
| 1-2 (root + 1 level) | Use strongest model (gpt-4o) |
| 3-5 | Can use lighter model (gpt-4o-mini) |
| 5+ | Review architecture; may indicate wrong approach |

### 4. Validate Configuration

Use the `check` command to validate recursive configurations:

```bash
agents-exe check --agent-file my-recursive-agent.json
```

This will:
- Verify all `extraAgents` paths resolve
- Check for duplicate slugs
- Warn about detected cycles (cycles are allowed but warned)

### 5. Monitor Costs

Recursive agents can incur unexpected API costs:

- Log recursion depth in your traces
- Set up budget alerts
- Consider using lighter models for deep recursion
- Test with depth limits before removing them

### 6. Prefer Explicit Over Implicit

Be explicit about which agents can recurse:

```json
{
  "extraAgents": [
    {"slug": "explicit-self", "path": "./agent.json"}
  ]
}
```

Rather than relying on implicit discovery.

### 7. Handle Cycle Warnings

When the system detects cycles, it logs warnings but continues:

```
CyclicReferencesWarning [["agent-a", "agent-b", "agent-a"]]
```

Review these warnings to ensure cycles are intentional.

---

## Troubleshooting

### "MissingAgentReference" Error

**Problem:** An agent references a slug that doesn't exist.

```
ReferenceError: Missing agent reference 'self-reference'
  Referrer: 'local:lrm' in file: localdev-agents/kimi-07.err1.json
  
  Hint: The slug 'self-reference' was not found in any loaded agent configuration.
  
  Did you mean to reference one of these agents?
    - 'local:lrm' (defined in localdev-agents/kimi-07.err1.json)
  
  Remember: The 'slug' in extraAgents must match the target agent's actual slug,
  not an arbitrary name. Check that your extraAgents configuration uses the 
  correct slug from the target file.
```

**Common Causes:**
1. The `slug` in `extraAgents` doesn't match the target agent's actual `slug`
2. The referenced agent file doesn't exist at the specified path
3. The referenced agent file has a different `slug` than expected

**Solutions:**
1. Check that the `slug` in `extraAgents` matches the target agent's `slug` field exactly
2. Verify the `path` in `extraAgents` points to the correct file
3. Open the target agent's JSON file and confirm its `slug` field
4. Remember: The `slug` in `extraAgents` is NOT an alias - it must match the target's actual slug

**Example Fix:**

If you have an agent file `helper.json`:
```json
{
  "contents": {
    "slug": "my-helper",
    ...
  }
}
```

Your `extraAgents` entry must use `"slug": "my-helper"`:
```json
{
  "extraAgents": [
    {
      "slug": "my-helper",
      "path": "./helper.json"
    }
  ]
}
```

Using `"slug": "helper"` or any other name will cause the error.

### "DuplicateAgentSlug" Error

**Problem:** The same slug is defined in multiple files.

```
ReferenceError (DuplicateAgentSlug "helper" ["./a/helper.json", "./b/helper.json"])
```

**Solutions:**
1. Rename one of the agents
2. Use different slugs for different purposes
3. Consolidate duplicate definitions

### Infinite Recursion

**Problem:** Agents call each other indefinitely.

**Symptoms:**
- High API costs
- Sessions don't terminate
- Repeated tool calls with similar parameters

**Solutions:**
1. Add explicit depth tracking to system prompts
2. Use `pushAgentContext` with `ctxMaxDepth` in tool implementations
3. Add termination conditions ("stop if no improvements")
4. Review and simplify agent collaboration patterns

### Performance Issues

**Problem:** Recursive agents are slow.

**Solutions:**
1. Use lighter models for deep recursion levels
2. Reduce context window by summarizing earlier results
3. Cache intermediate results
4. Consider flattening the recursion into iteration

---

## Summary

Recursive agent references enable powerful patterns like iterative refinement and collaborative problem-solving. The key points are:

1. Use `extraAgents` to declare references outside `toolDirectory`
2. **Important**: The `slug` in `extraAgents` must match the target agent's actual `slug`
3. The system validates all references before runtime
4. Implement depth limits via `ToolExecutionContext`
5. Monitor for cycles and unintended infinite recursion
6. Document recursion patterns in system prompts

For more information, see:
- `System.Agents.AgentTree` - Loading and initialization
- `System.Agents.Tools.Context` - Recursion tracking
- `System.Agents.Base` - `ExtraAgentRef` type

