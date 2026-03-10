# Bug: PostgREST Body Parameter Schema Resolution Fails

## Summary

The PostgREST toolbox fails to correctly extract request body schemas from PostgREST OpenAPI specifications because PostgREST uses a parameter reference indirection pattern (`#/parameters/body.tablename`) that isn't being properly resolved.

## Problem Description

### PostgREST's Indirect Body Parameter Pattern

PostgREST (Swagger 2.0) encodes POST/PUT/PATCH body parameters using a two-level reference pattern:

#### 1. Operation references parameter definition
```json
{
  "paths": {
    "/trainers": {
      "post": {
        "parameters": [
          { "$ref": "#/parameters/body.trainers" },  // <-- Indirection
          { "$ref": "#/parameters/select" }
        ]
      }
    }
  }
}
```

#### 2. Parameter definition contains body location + schema ref
```json
{
  "parameters": {
    "body.trainers": {
      "name": "trainers",
      "description": "trainers",
      "required": false,
      "in": "body",                    // <-- "in": "body" is HERE
      "schema": {
        "$ref": "#/definitions/trainers"  // <-- Schema reference
      }
    }
  }
}
```

#### 3. Schema definition contains actual structure
```json
{
  "definitions": {
    "trainers": {
      "description": "Player accounts who collect and battle monsters",
      "required": ["id", "username", "coins", "wins", "losses", "started_at", "last_active_at", "is_active"],
      "properties": {
        "id": { "format": "uuid", "type": "string" },
        "username": { "format": "text", "type": "string" },
        "coins": { "default": 0, "description": "In-game currency for purchasing items", "format": "integer", "type": "integer" },
        ...
      },
      "type": "object"
    }
  }
}
```

### Why Current Code Fails

The current flow in `Types.hs` → `Converter.hs`:

1. **`parseOperationSwaggerV2`** correctly converts **inline** body parameters to `RequestBody`
2. But parameters with `$ref` to `#/parameters/body.xxx` are parsed by **`parseParameterSwaggerV2`** as regular parameters
3. The `$ref` is stored in `schemaRef` but **never resolved against the top-level `parameters` section**
4. `opRequestBody` ends up as `Nothing` for these operations
5. `prtRequestBodySchema` is `Nothing`, so tools lack request body schemas

### Root Cause

`OpenAPISpec` doesn't store the Swagger 2.0 `parameters` section:

```haskell
data OpenAPISpec = OpenAPISpec
    { specPaths :: Map Path (Map Method Operation)
    , specComponents :: Maybe Components  -- Only schemas, no parameters!
    }
```

The `parameters` section from Swagger 2.0 is parsed but discarded during conversion.

## Impact

- POST/PUT/PATCH tools for PostgREST APIs have no request body schema
- LLMs don't know what fields to include when creating records
- Required field validation is impossible
- Type information is lost

## Affected Operations in test/data/monstergame.json

All write operations are affected:
- `POST /trainers`, `PATCH /trainers`
- `POST /monsters`, `PATCH /monsters`
- `POST /battles`, `PATCH /battles`
- `POST /items`, `PATCH /items`
- etc.

## Proposed Solution

### Approach: Test-First, Then Patch

Given the complexity and the need to support both generic OpenAPI and PostgREST-specific patterns, we should:

1. **Add comprehensive tests** with toy specs for both patterns
2. **Extend `OpenAPISpec`** to store Swagger 2.0 parameters (or create PostgREST-specific types)
3. **Add resolution logic** for body parameter references
4. **Keep generic OpenAPI path intact** by duplicating/extracting PostgREST-specific logic

### Phase 1: Add Tests

Create test specs that isolate the patterns:

#### Test A: Standard OpenAPI 3.0 (works today)
```json
{
  "openapi": "3.0.0",
  "paths": {
    "/users": {
      "post": {
        "requestBody": {
          "content": {
            "application/json": {
              "schema": { "$ref": "#/components/schemas/User" }
            }
          }
        }
      }
    }
  },
  "components": {
    "schemas": {
      "User": {
        "type": "object",
        "properties": { "name": { "type": "string" } }
      }
    }
  }
}
```

#### Test B: Swagger 2.0 Inline Body (works today)
```json
{
  "swagger": "2.0",
  "paths": {
    "/users": {
      "post": {
        "parameters": [
          {
            "in": "body",
            "name": "user",
            "schema": { "$ref": "#/definitions/User" }
          }
        ]
      }
    }
  },
  "definitions": {
    "User": {
      "type": "object",
      "properties": { "name": { "type": "string" } }
    }
  }
}
```

#### Test C: PostgREST Pattern (BROKEN today)
```json
{
  "swagger": "2.0",
  "paths": {
    "/users": {
      "post": {
        "parameters": [
          { "$ref": "#/parameters/body.users" }
        ]
      }
    }
  },
  "parameters": {
    "body.users": {
      "name": "users",
      "in": "body",
      "schema": { "$ref": "#/definitions/User" }
    }
  },
  "definitions": {
    "User": {
      "type": "object",
      "properties": { "name": { "type": "string" } }
    }
  }
}
```

### Phase 2: Implementation Options

#### Option A: Extend `OpenAPISpec` with Parameters

Add a `specParameters` field for Swagger 2.0 compatibility:

```haskell
data OpenAPISpec = OpenAPISpec
    { specPaths :: Map Path (Map Method Operation)
    , specComponents :: Maybe Components
    , specParameters :: Maybe (Map Text Parameter)  -- NEW: Swagger 2.0 parameters
    }
```

Modify `parseSwaggerV2` to populate this field.

#### Option B: PostgREST-Specific Spec Type

Create parallel types in `PostgREST.Types`:

```haskell
data PostgRESTSpec = PostgRESTSpec
    { prsPaths :: Map Path (Map Method PostgRESTOperation)
    , prsDefinitions :: Map Text Schema
    , prsParameters :: Map Text PostgRESTParameter
    }

data PostgRESTParameter = PostgRESTParameter
    { prpName :: Text
    , prpIn :: ParamLocation
    , prpSchema :: Maybe Schema
    , ...
    }
```

#### Option C: Resolution During Conversion (Recommended)

Add a resolution step in `PostgREST.Converter` that:

1. Accepts the raw Aeson `Value` or stores unresolved refs
2. Resolves `#/parameters/body.xxx` references before conversion
3. Creates proper `RequestBody` values from resolved parameters

Example implementation sketch:

```haskell
-- In PostgREST.Converter
convertPostgRESToTools :: ... -> [PostgRESTool]
convertPostgRESToTools toolboxName allowedMethods spec rawSpecValue = do
    ...
    let requestBodySchema = extractPostgRESTBodySchema operation rawSpecValue
    ...

-- New function that handles PostgREST-specific resolution
extractPostgRESTBodySchema :: Operation -> Value -> Maybe Schema
extractPostgRESTBodySchema op rawSpec = do
    -- First try standard extraction
    case extractRequestBodySchema op of
        Just schema -> Just schema
        Nothing -> resolvePostgRESTBodyRef op rawSpec

resolvePostgRESTBodyRef :: Operation -> Value -> Maybe Schema
resolvePostgRESTBodyRef op rawSpec = do
    -- Find parameter with $ref to #/parameters/body.*
    param <- find isBodyParamRef (opParameters op)
    ref <- getRef param
    -- Extract table name from ref: #/parameters/body.tablename
    tableName <- extractTableName ref
    -- Look up in raw spec's parameters section
    paramDef <- lookupParameter rawSpec tableName
    -- Verify it's a body parameter
    guard (paramIn paramDef == ParamInBody)
    -- Extract schema from parameter definition
    paramSchema paramDef
```

### Phase 3: Validation

Ensure:
1. All three test specs (A, B, C) pass
2. `monstergame.json` POST tools have proper request body schemas
3. Generic OpenAPI 3.0 specs continue to work
4. Swagger 2.0 inline body parameters continue to work

## Acceptance Criteria

- [ ] Test specs for all three patterns pass
- [ ] `monstergame.json` POST/PATCH tools expose correct request body schemas
- [ ] No regression in generic OpenAPI toolbox
- [ ] Documentation updated with PostgREST-specific behavior

## Related Files

- `src/System/Agents/Tools/OpenAPI/Types.hs` - OpenAPI parsing
- `src/System/Agents/Tools/PostgREST/Converter.hs` - PostgREST conversion
- `src/System/Agents/Tools/PostgREST/Types.hs` - PostgREST types
- `test/data/monstergame.json` - Test data exhibiting the issue
- `test/PostgRESTParseTest.hs` - Existing tests

## Labels

`bug`, `postgrest`, `openapi`, `schema-resolution`

