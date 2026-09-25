# Internal Tool Mapping Documentation

## Overview

This document describes how tools from various sources (OpenAPI, MCP, Bash, IO) are mapped to LLM-compatible tool names. This mapping is critical because different systems have different naming constraints.

## The Problem

LLM APIs (like OpenAI) have strict requirements for function names:
- Must start with a letter (a-z, A-Z)
- Can only contain alphanumeric characters and underscores: `[a-zA-Z0-9_]`
- Some providers have additional length constraints

Note: Dashes (`-`) and colons (`:`) are **not** allowed, even though some documentation may suggest otherwise.

However, tool identifiers from external sources may contain invalid characters:
- OpenAPI operation IDs can contain dots (e.g., `pet.findByStatus`)
- OpenAPI operation IDs can contain slashes (e.g., `users/pets/get`)
- URLs may contain dots, colons, slashes, and other special characters (e.g., `localhost:8001`)
- MCP tool names may contain dashes, colons, or other special characters

## Current Naming Schemes

### 1. IO Tools

```haskell
io2LLMName :: IOScript a b -> OpenAI.ToolName
io2LLMName io = OpenAI.ToolName ("io_" <> io.description.ioSlug)
```

- Prefix: `io_`
- Source: User-defined slug
- Constraints: Assumed valid (user-controlled)

### 2. Bash Tools

```haskell
bash2LLMName :: ScriptDescription -> OpenAI.ToolName
bash2LLMName bash = OpenAI.ToolName ("bash_" <> bash.scriptInfo.scriptSlug)
```

- Prefix: `bash_`
- Source: User-defined slug
- Constraints: Assumed valid (user-controlled)

### 3. MCP Tools

```haskell
mcp2LLMName :: Toolbox -> ToolDescription -> OpenAI.ToolName
mcp2LLMName box mcp = OpenAI.ToolName ("mcp_" <> box.name <> "_" <> mcp.getToolDescription.name)
```

- Prefix: `mcp_<toolboxName>_`
- Source: MCP tool name from server
- **Issue**: MCP tool names may contain invalid characters (dashes, colons, etc.)

### 4. OpenAPI Tools

```haskell
openapi2LLMName :: Text -> Text -> OpenAI.ToolName
openapi2LLMName tboxName operationId =
    OpenAI.ToolName ("openapi_" <> tboxName <> "_" <> operationId)
```

- Prefix: `openapi_<toolboxName>_`
- Source: OpenAPI operationId
- **Issue**: operationId often contains dots (e.g., `pet.findByStatus`)
- **Issue**: Toolbox names derived from URLs may contain colons (e.g., `localhost:8001`)

## Error Manifestation

When an invalid tool name is sent to an LLM API, you may see errors like:

```
"Invalid request: function name is invalid, must start with a letter 
 and can contain only alphanumerics and underscores"
```

This commonly occurs with:
- OpenAPI specs that use dot-notation for operation IDs:
  - `pet.findByStatus` → Invalid (contains dot)
  - `users/pets/get` → Invalid (contains slash)
- Toolbox names derived from URLs:
  - `localhost:8001` → Invalid (contains colon)
  - `api.example.com` → Invalid (contains dots)
- MCP tool names with dashes or special characters:
  - `get-pet` → Invalid (contains dash)

## Proposed Solution: Normalization with Bi-directional Mapping

### Core Principles

1. **Normalization**: Convert all tool names to LLM-valid format (alphanumeric + underscore only)
2. **Bi-directional Mapping**: Maintain ability to map back to original names
3. **Consistency**: Same normalization logic for registration and lookup
4. **Collision Avoidance**: Ensure unique normalized names

### Normalization Function

```haskell
-- | Normalize a tool name to be LLM-compatible.
-- 
-- Rules:
-- 1. Must start with a letter (prefix with 't' if needed)
-- 2. Replace all non-alphanumeric characters with underscores
-- 3. Collapse multiple consecutive underscores
-- 4. Preserve case (some providers are case-sensitive)
--
-- Examples:
-- >>> normalizeToolName "pet.findByStatus"
-- "pet_findByStatus"
-- >>> normalizeToolName "users/pets/get"
-- "users_pets_get"
-- >>> normalizeToolName "2.0_getPet"  -- starts with number
-- "t2_0_getPet"
-- >>> normalizeToolName "localhost:8001"  -- contains colon
-- "localhost_8001"
-- >>> normalizeToolName "get-pet"  -- contains dash
-- "get_pet"
normalizeToolName :: Text -> Text
```

### Character Replacement Rules

| Character | Replacement | Example |
|-----------|-------------|---------|
| `.` (dot) | `_` (underscore) | `pet.find` → `pet_find` |
| `/` (slash) | `_` (underscore) | `users/pets` → `users_pets` |
| `:` (colon) | `_` (underscore) | `localhost:8001` → `localhost_8001` |
| `-` (dash) | `_` (underscore) | `get-pet` → `get_pet` |
| Other special chars | `_` (underscore) | `pet@home` → `pet_home` |
| Leading digit | Prefix with `t` | `2.0_api` → `t2_0_api` |

### Bi-directional Mapping Strategy

For OpenAPI tools, we need to maintain a mapping from normalized LLM names back to original operation IDs:

```haskell
data ToolNameMapping = ToolNameMapping
    { originalName :: Text        -- ^ Original operationId
    , normalizedName :: Text      -- ^ LLM-safe name
    , fullLLMName :: Text         -- ^ With prefix (e.g., "openapi_pets_pet_findByStatus")
    }

-- Build mapping during tool registration
buildNameMapping :: Text -> [OpenAPITool] -> Map Text ToolNameMapping
buildNameMapping toolboxName tools = ...

-- Lookup during tool execution
findToolByLLMName :: Map Text ToolNameMapping -> OpenAI.ToolName -> Maybe OpenAPITool
```

### Implementation in OpenAPI Toolbox

The OpenAPI toolbox should:

1. **During Initialization**:
   - Normalize the toolbox name itself (may contain colons from URLs)
   - Convert all operation IDs to normalized form
   - Store mapping from normalized → original
   - Generate LLM tool names with normalized IDs

2. **During Tool Execution**:
   - Receive LLM tool call with normalized name
   - Look up original operation ID from mapping
   - Execute using original ID

### Updated Naming Flow

```
OpenAPI Spec
    │
    ▼
Parse Operations
    │
    ▼
Extract operationId (e.g., "pet.findByStatus")
    │
    ▼
Normalize ──────────────────┐
    │                        │
    ▼                        │
"pet_findByStatus"          │
    │                        │
    ▼                        ▼
Add prefix:              Store mapping:
"openapi_pets_pet_findByStatus"  "pet_findByStatus" → "pet.findByStatus"
    │
    ▼
Register with LLM
    │
    ▼
LLM calls tool with name "openapi_pets_pet_findByStatus"
    │
    ▼
Lookup mapping → "pet.findByStatus"
    │
    ▼
Execute HTTP request
```

## Implementation Plan

### Phase 1: Normalization Function

Add to `System.Agents.Tools.OpenAPI.Converter`:

```haskell
-- | Normalize a tool name for LLM compatibility.
-- Only allows alphanumeric characters and underscores.
normalizeForLLM :: Text -> Text
normalizeForLLM = collapseUnderscores . ensureLetterStart . replaceInvalid
  where
    replaceInvalid = Text.map replaceChar
    replaceChar c
        | isValidChar c = c
        | otherwise = '_'
    -- Only alphanumeric and underscore allowed (NO dashes, NO colons)
    isValidChar c = isLetter c || isDigit c || c == '_'
    
    ensureLetterStart t
        | Text.null t = "tool"
        | isLetter (Text.head t) = t
        | otherwise = "t" <> t
    
    collapseUnderscores = Text.intercalate "_" . filter (not . Text.null) . Text.splitOn "_"
```

### Phase 2: Bi-directional Mapping

Add to `System.Agents.Tools.OpenAPI.Converter`:

```haskell
data NameMapping = NameMapping
    { nmOriginal :: Text
    , nmNormalized :: Text
    }

buildToolNameMapping :: Text -> [OpenAPITool] -> Map Text NameMapping
buildToolNameMapping toolboxName tools = 
    Map.fromList [(nmNormalized m, m) | m <- mappings]
  where
    mappings = [ NameMapping orig (normalizeForLLM orig)
               | tool <- tools
               , let orig = getOperationIdOrFallback tool
               ]
```

### Phase 3: Update OpenAPI Toolbox

Modify `Toolbox` to store name mapping:

```haskell
data Toolbox = Toolbox
    { toolboxName :: Text
    , toolboxBaseUrl :: Text
    , toolboxTools :: [OpenAPITool]
    , toolboxNameMapping :: Map Text NameMapping  -- ^ normalized → original
    , httpRuntime :: HttpClient.Runtime
    , headerFunc :: Maybe (IO (Map Text Text))
    , staticHeaders :: Map Text Text
    }
```

### Phase 4: Update Registration Functions

Update `openapi2LLMName` to use normalized names for both toolbox and operation:

```haskell
openapi2LLMName :: Text -> Text -> OpenAI.ToolName
openapi2LLMName tboxName operationId =
    let normalizedBox = normalizeForLLM tboxName
        normalizedOp = normalizeForLLM operationId
    in OpenAI.ToolName ("openapi_" <> normalizedBox <> "_" <> normalizedOp)
```

Note: Both `tboxName` and `operationId` must be normalized because toolbox names may come from URLs containing colons.

## Testing Strategy

1. **Unit Tests**:
   - Test normalization of various edge cases (dots, slashes, colons, dashes)
   - Test bi-directional mapping
   - Test collision handling

2. **Integration Tests**:
   - Test with real OpenAPI specs containing dots in operationIds
   - Test with toolbox names from URLs containing colons
   - Test end-to-end tool registration and execution

3. **Property Tests**:
   - Round-trip property: normalize → denormalize should preserve original
   - Validity property: normalized names always match `[a-zA-Z][a-zA-Z0-9_]*`

## Edge Cases

### 1. Name Collisions

When two different operation IDs normalize to the same name:

```haskell
-- Original names
"pet.find" and "pet_find" both normalize to "pet_find"

-- Solution: Append number suffix
"pet_find" and "pet_find_2"
```

### 2. Empty Operation IDs

When operationId is missing, we fall back to path/method:

```haskell
-- From path "/pets/{id}" and method "GET"
-- Generated name: "get_pets__id_"
```

### 3. Very Long Names

Some providers have length limits. Strategy:

```haskell
-- Truncate with hash suffix to maintain uniqueness
"openapi_very_long_toolbox_name_very_long_operation_id_..."
  → "openapi_very_long_toolbox_name_very_long_operat_a1b2c3"
```

### 4. Unicode Characters

Non-ASCII characters in operation IDs:

```haskell
-- Replace with underscores
"获取宠物" → "____"
-- Then ensure letter start: "t____"
```

### 5. Toolbox Names from URLs

When toolbox names are derived from URLs, they may contain colons and dots:

```haskell
-- Original URL-based name
"localhost:8001"
-- Normalized
"localhost_8001"

-- Original URL-based name
"api.example.com:8080"
-- Normalized
"api_example_com_8080"
```

## Related Files

- `src/System/Agents/Tools/OpenAPI/Converter.hs` - Tool conversion logic
- `src/System/Agents/Tools/OpenAPI/Types.hs` - Type definitions
- `src/System/Agents/Tools/OpenAPIToolbox.hs` - Toolbox implementation
- `src/System/Agents/ToolRegistration.hs` - LLM registration
- `src/System/Agents/LLMs/OpenAI.hs` - LLM ToolName type

## Future Considerations

1. **Caching**: Cache normalized names to avoid recomputation
2. **Configuration**: Allow users to provide custom normalization rules
3. **Validation**: Pre-validate specs during initialization
4. **Reporting**: Log warnings when names are normalized

