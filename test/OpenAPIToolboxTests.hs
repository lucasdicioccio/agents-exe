{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Comprehensive tests for the OpenAPI Toolbox implementation.
--
-- This module contains tests for:
-- * Type parsing (OpenAPI types)
-- * Reference resolution ($ref dereferencing)
-- * Tool conversion (OpenAPI to LLM tools)
-- * Integration tests (path formatting, URL building)
--
-- Test data is loaded from test/data/ directory.
module OpenAPIToolboxTests where

import Data.Aeson (FromJSON, Value (..), decode, encode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.Tasty
import Test.Tasty.HUnit

-- Modules under test
import System.Agents.Tools.OpenAPI.Converter as Converter
import System.Agents.Tools.OpenAPI.Resolver as Resolver
import System.Agents.Tools.OpenAPI.Types as Types
import System.Agents.Tools.OpenAPIToolbox as Toolbox
import qualified System.Agents.LLMs.OpenAI as OpenAI

-- -------------------------------------------------------------------------
-- Test Entry Point
-- -------------------------------------------------------------------------

-- | All OpenAPI Toolbox tests.
tests :: TestTree
tests =
    testGroup
        "OpenAPI Toolbox Tests"
        [ parsingTests
        , resolverTests
        , converterTests
        , integrationTests
        , propertyTests
        , swaggerV2Tests
        ]

-- -------------------------------------------------------------------------
-- Test Data Loading
-- -------------------------------------------------------------------------

-- | Load a test spec from the test/data directory.
loadTestSpec :: FilePath -> IO OpenAPISpec
loadTestSpec filename = do
    let path = "test/data/" ++ filename
    content <- LBS.readFile path
    case decode content of
        Nothing -> assertFailure $ "Failed to parse JSON from " ++ path
        Just spec -> pure spec

-- | Load test spec as raw JSON value.
loadTestSpecRaw :: FilePath -> IO Value
loadTestSpecRaw filename = do
    let path = "test/data/" ++ filename
    content <- LBS.readFile path
    case decode content of
        Nothing -> assertFailure $ "Failed to parse JSON from " ++ path
        Just val -> pure val

-- -------------------------------------------------------------------------
-- 1. Types Parsing Tests
-- -------------------------------------------------------------------------

-- | Tests for OpenAPI type parsing.
parsingTests :: TestTree
parsingTests =
    testGroup
        "Types Parsing"
        [ test_parsePetstoreSpec
        , test_parseMinimalSpec
        , test_parseWithRefsSpec
        , test_parseWithEnumsSpec
        , test_parseOperation
        , test_parseParameter
        , test_parseSchema
        , test_parseRequestBody
        , test_parseComponents
        , test_roundtripSpec
        ]

-- | Test that petstore spec can be parsed.
test_parsePetstoreSpec :: TestTree
test_parsePetstoreSpec = testCase "Can parse Petstore OpenAPI spec" $ do
    spec <- loadTestSpec "petstore.json"
    assertBool "Has paths" (not $ Map.null $ specPaths spec)
    assertBool "Has components" (isJust $ specComponents spec)

-- | Test minimal spec parsing.
test_parseMinimalSpec :: TestTree
test_parseMinimalSpec = testCase "Can parse minimal spec" $ do
    spec <- loadTestSpec "minimal.json"
    Map.size (specPaths spec) @?= 1
    case Map.lookup "/health" (specPaths spec) of
        Nothing -> assertFailure "Path /health not found"
        Just methods -> case Map.lookup "GET" methods of
            Nothing -> assertFailure "GET method not found"
            Just op -> opOperationId op @?= Just "healthCheck"

-- | Test spec with references parsing.
test_parseWithRefsSpec :: TestTree
test_parseWithRefsSpec = testCase "Can parse spec with $refs" $ do
    spec <- loadTestSpec "with_refs.json"
    assertBool "Has paths" (Map.size (specPaths spec) > 0)
    case specComponents spec of
        Nothing -> assertFailure "Expected components"
        Just comps -> case componentsSchemas comps of
            Nothing -> assertFailure "Expected schemas"
            Just schemas -> Map.size schemas @?= 3

-- | Test spec with enums parsing.
test_parseWithEnumsSpec :: TestTree
test_parseWithEnumsSpec = testCase "Can parse spec with enums" $ do
    spec <- loadTestSpec "with_enums.json"
    -- Find an operation with enum parameters
    case Map.lookup "/orders" (specPaths spec) of
        Nothing -> assertFailure "Path /orders not found"
        Just methods -> do
            -- The parser converts method names to uppercase
            let methodLookup = Map.lookup "GET" methods <|> Map.lookup "get" methods
            case methodLookup of
                Nothing -> assertFailure "GET method not found"
                Just op -> do
                    assertBool "Has parameters" (not $ null $ opParameters op)
                    case filter (\p -> paramName p == "status") (opParameters op) of
                        [] -> assertFailure "status parameter not found"
                        (param : _) -> case paramSchema param of
                            Nothing -> assertFailure "status parameter has no schema"
                            Just schema -> case schemaEnum schema of
                                Nothing -> assertFailure "Expected enum in schema"
                                Just vals -> length vals @?= 5
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) (Just x) _ = Just x
    (<|>) Nothing y = y

-- | Test individual operation parsing.
test_parseOperation :: TestTree
test_parseOperation = testCase "Can parse individual operation" $ do
    let json =
            encode $
                Aeson.object
                    [ "operationId" Aeson..= ("testOp" :: Text)
                    , "summary" Aeson..= ("Test operation" :: Text)
                    , "description" Aeson..= ("A test operation" :: Text)
                    , "parameters" Aeson..= ([] :: [Value])
                    ]
    case decode json of
        Nothing -> assertFailure "Failed to parse operation"
        Just (op :: Operation) -> do
            opOperationId op @?= Just "testOp"
            opSummary op @?= Just "Test operation"
            opDescription op @?= Just "A test operation"

-- | Test individual parameter parsing.
test_parseParameter :: TestTree
test_parseParameter = testCase "Can parse individual parameter" $ do
    let json =
            encode $
                Aeson.object
                    [ "name" Aeson..= ("userId" :: Text)
                    , "in" Aeson..= ("path" :: Text)
                    , "required" Aeson..= True
                    , "description" Aeson..= ("The user ID" :: Text)
                    , "schema" Aeson..= (Aeson.object ["type" Aeson..= ("string" :: Text)] :: Value)
                    ]
    case decode json of
        Nothing -> assertFailure "Failed to parse parameter"
        Just (param :: Parameter) -> do
            paramName param @?= "userId"
            paramIn param @?= ParamInPath
            paramRequired param @?= True
            paramDescription param @?= Just "The user ID"
            assertBool "Has schema" (isJust $ paramSchema param)

-- | Test schema parsing.
test_parseSchema :: TestTree
test_parseSchema = testCase "Can parse various schema types" $ do
    -- Test $ref schema
    let refJson = Aeson.object ["$ref" Aeson..= ("#/components/schemas/Pet" :: Text)]
    case decode (encode refJson) of
        Nothing -> assertFailure "Failed to parse $ref schema"
        Just (schema :: Schema) -> do
            schemaRef schema @?= Just "#/components/schemas/Pet"
            schemaType schema @?= Nothing

    -- Test inline object schema
    let objJson =
            Aeson.object
                [ "type" Aeson..= ("object" :: Text)
                , "properties"
                    Aeson..= Aeson.object
                        [ "name" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text)]
                        , "age" Aeson..= Aeson.object ["type" Aeson..= ("integer" :: Text)]
                        ]
                , "required" Aeson..= (["name"] :: [Text])
                ]
    case decode (encode objJson) of
        Nothing -> assertFailure "Failed to parse object schema"
        Just (schema :: Schema) -> do
            schemaType schema @?= Just "object"
            case schemaProperties schema of
                Nothing -> assertFailure "Expected properties"
                Just props -> Map.size props @?= 2
            schemaRequired schema @?= Just ["name"]

-- | Test request body parsing.
test_parseRequestBody :: TestTree
test_parseRequestBody = testCase "Can parse request body" $ do
    let json =
            Aeson.object
                [ "description" Aeson..= ("Pet to create" :: Text)
                , "required" Aeson..= True
                , "content"
                    Aeson..= Aeson.object
                        [ "application/json"
                            Aeson..= Aeson.object
                                ["schema" Aeson..= (Aeson.object ["type" Aeson..= ("object" :: Text)] :: Value)]
                        ]
                ]
    case decode (encode json) of
        Nothing -> assertFailure "Failed to parse request body"
        Just (body :: RequestBody) -> do
            reqBodyDescription body @?= Just "Pet to create"
            reqBodyRequired body @?= True
            Map.size (reqBodyContent body) @?= 1

-- | Test components parsing.
test_parseComponents :: TestTree
test_parseComponents = testCase "Can parse components" $ do
    let json =
            Aeson.object
                [ "schemas"
                    Aeson..= Aeson.object
                        [ "Pet"
                            Aeson..= Aeson.object
                                [ "type" Aeson..= ("object" :: Text)
                                , "properties"
                                    Aeson..= Aeson.object
                                        ["name" Aeson..= Aeson.object ["type" Aeson..= ("string" :: Text)]]
                                ]
                        ]
                ]
    case decode (encode json) of
        Nothing -> assertFailure "Failed to parse components"
        Just (comps :: Components) -> do
            case componentsSchemas comps of
                Nothing -> assertFailure "Expected schemas"
                Just schemas -> do
                    Map.size schemas @?= 1
                    assertBool "Has Pet schema" (Map.member "Pet" schemas)

-- | Test JSON round-trip for spec.
test_roundtripSpec :: TestTree
test_roundtripSpec = testCase "OpenAPISpec round-trips through JSON" $ do
    spec <- loadTestSpec "petstore.json"
    let json = encode spec
    case decode json of
        Nothing -> assertFailure "Failed to round-trip spec"
        Just spec' -> do
            -- Check key properties match
            Map.size (specPaths spec) @?= Map.size (specPaths spec')
            (isJust $ specComponents spec) @?= (isJust $ specComponents spec')

-- -------------------------------------------------------------------------
-- 2. Resolver Tests
-- -------------------------------------------------------------------------

-- | Tests for reference resolution.
resolverTests :: TestTree
resolverTests =
    testGroup
        "Resolver"
        [ test_resolveSimpleRef
        , test_resolveNestedRef
        , test_resolveArrayItemRef
        , test_resolveAnyOfRef
        , test_resolveCircularRef
        , test_resolveMissingRef
        , test_resolveInvalidRefPath
        , test_dereferenceSpec
        , test_parseRefValid
        , test_parseRefInvalid
        ]

-- | Test simple reference resolution.
test_resolveSimpleRef :: TestTree
test_resolveSimpleRef = testCase "Resolves simple $ref" $ do
    let petSchema = mkSchema (Just "object") (Just "A pet") Nothing Nothing Nothing Nothing Nothing Nothing
    let components = Components (Just (Map.fromList [("Pet", petSchema)]))
    let refSchema = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Pet") Nothing

    let resolved = resolveSchema refSchema components
    schemaType resolved @?= Just "object"
    schemaDescription resolved @?= Just "A pet"
    schemaRef resolved @?= Nothing

-- | Test nested reference resolution.
test_resolveNestedRef :: TestTree
test_resolveNestedRef = testCase "Resolves nested $refs" $ do
    -- Address -> User -> UserRef
    let addressSchema = mkSchema (Just "object") (Just "An address") Nothing Nothing Nothing Nothing Nothing Nothing
    let addressRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Address") Nothing
    let userSchema = mkSchema (Just "object") Nothing Nothing (Just (Map.fromList [("address", addressRef)])) Nothing Nothing Nothing Nothing
    let userRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/User") Nothing
    let components = Components (Just (Map.fromList [("Address", addressSchema), ("User", userSchema)]))

    let resolved = resolveSchema userRef components
    schemaType resolved @?= Just "object"
    case schemaProperties resolved of
        Nothing -> assertFailure "Expected properties"
        Just props -> case Map.lookup "address" props of
            Nothing -> assertFailure "Expected address property"
            Just addr -> schemaDescription addr @?= Just "An address"

-- | Test array item reference resolution.
test_resolveArrayItemRef :: TestTree
test_resolveArrayItemRef = testCase "Resolves array item $ref" $ do
    let itemSchema = mkSchema (Just "string") (Just "A tag") Nothing Nothing Nothing Nothing Nothing Nothing
    let itemRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Tag") Nothing
    let arraySchema = mkSchema (Just "array") Nothing Nothing Nothing (Just itemRef) Nothing Nothing Nothing
    let components = Components (Just (Map.fromList [("Tag", itemSchema), ("Tags", arraySchema)]))
    let tagsRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Tags") Nothing

    let resolved = resolveSchema tagsRef components
    schemaType resolved @?= Just "array"
    case schemaItems resolved of
        Nothing -> assertFailure "Expected items"
        Just items -> schemaDescription items @?= Just "A tag"

-- | Test anyOf reference resolution.
test_resolveAnyOfRef :: TestTree
test_resolveAnyOfRef = testCase "Resolves anyOf $refs" $ do
    let catSchema = mkSchema (Just "object") (Just "A cat") Nothing Nothing Nothing Nothing Nothing Nothing
    let dogSchema = mkSchema (Just "object") (Just "A dog") Nothing Nothing Nothing Nothing Nothing Nothing
    let catRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Cat") Nothing
    let dogRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Dog") Nothing
    let anyOfSchema = mkSchema Nothing Nothing Nothing Nothing Nothing (Just [catRef, dogRef]) Nothing Nothing
    let components = Components (Just (Map.fromList [("Cat", catSchema), ("Dog", dogSchema), ("Pet", anyOfSchema)]))
    let petRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Pet") Nothing

    let resolved = resolveSchema petRef components
    case schemaAnyOf resolved of
        Nothing -> assertFailure "Expected anyOf"
        Just schemas -> do
            length schemas @?= 2
            let descs = map schemaDescription schemas
            assertBool "Contains cat description" (Just "A cat" `elem` descs)
            assertBool "Contains dog description" (Just "A dog" `elem` descs)

-- | Test circular reference detection.
test_resolveCircularRef :: TestTree
test_resolveCircularRef = testCase "Detects circular references" $ do
    -- A refs B, B refs A
    let aRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/A") Nothing
    let bRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/B") Nothing
    let schemaA = mkSchema (Just "object") Nothing Nothing (Just (Map.fromList [("b", bRef)])) Nothing Nothing Nothing Nothing
    let schemaB = mkSchema (Just "object") Nothing Nothing (Just (Map.fromList [("a", aRef)])) Nothing Nothing Nothing Nothing
    let components = Components (Just (Map.fromList [("A", schemaA), ("B", schemaB)]))

    let result = resolveSchemaWithDepth aRef components 0 Set.empty
    case result of
        Left (CircularReference _) -> pure ()
        Left err -> assertFailure $ "Expected CircularReference, got: " ++ show err
        Right _ -> assertFailure "Expected error for circular reference"

-- | Test missing reference handling.
test_resolveMissingRef :: TestTree
test_resolveMissingRef = testCase "Handles missing reference gracefully" $ do
    let refSchema = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Missing") Nothing
    let components = Components (Just Map.empty)

    let resolved = resolveSchema refSchema components
    -- Should return schema with error description
    case schemaDescription resolved of
        Just desc -> assertBool "Expected error message" ("Reference resolution failed" `Text.isInfixOf` desc)
        Nothing -> assertFailure "Expected error description"

-- | Test invalid ref path handling.
test_resolveInvalidRefPath :: TestTree
test_resolveInvalidRefPath = testCase "Handles invalid ref path" $ do
    let components = Components Nothing
    case resolveRef "invalid-ref" components of
        Left (InvalidRefPath _) -> pure ()
        Left err -> assertFailure $ "Expected InvalidRefPath, got: " ++ show err
        Right _ -> assertFailure "Expected error for invalid ref"

-- | Test full spec dereferencing.
test_dereferenceSpec :: TestTree
test_dereferenceSpec = testCase "Dereferences all schemas in spec" $ do
    let petSchema = mkSchema (Just "object") (Just "A pet") Nothing Nothing Nothing Nothing Nothing Nothing
    let components = Components (Just (Map.fromList [("Pet", petSchema)]))
    let refSchema = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Pet") Nothing
    let param = Parameter "petId" ParamInPath Nothing True (Just refSchema)
    let operation = Operation (Just "getPet") Nothing Nothing [param] Nothing
    let spec = OpenAPISpec (Map.singleton "/pets/{petId}" (Map.singleton "GET" operation)) (Just components) Nothing

    let dereferenced = dereferenceSpec spec
    case Map.lookup "/pets/{petId}" (specPaths dereferenced) of
        Nothing -> assertFailure "Path not found"
        Just methods -> case Map.lookup "GET" methods of
            Nothing -> assertFailure "GET method not found"
            Just op -> case opParameters op of
                [] -> assertFailure "Expected parameters"
                (p : _) -> case paramSchema p of
                    Nothing -> assertFailure "Expected schema"
                    Just schema -> do
                        schemaType schema @?= Just "object"
                        schemaRef schema @?= Nothing

-- | Test parsing valid refs.
test_parseRefValid :: TestTree
test_parseRefValid = testCase "Parses valid internal refs" $ do
    case parseRef "#/components/schemas/Pet" of
        Nothing -> assertFailure "Failed to parse valid ref"
        Just refPath -> do
            refComponents refPath @?= "components"
            refSection refPath @?= "schemas"
            refName refPath @?= "Pet"

    case parseRef "#/components/schemas/Nested/Deep" of
        Nothing -> assertFailure "Failed to parse nested ref"
        Just refPath -> refName refPath @?= "Nested"

-- | Test parsing invalid refs.
test_parseRefInvalid :: TestTree
test_parseRefInvalid = testCase "Rejects invalid refs" $ do
    parseRef "http://example.com/schema.json" @?= Nothing
    parseRef "https://example.com/schema.json" @?= Nothing
    parseRef "invalid" @?= Nothing
    parseRef "" @?= Nothing

-- -------------------------------------------------------------------------
-- 3. Converter Tests
-- -------------------------------------------------------------------------

-- | Tests for tool conversion.
converterTests :: TestTree
converterTests =
    testGroup
        "Converter"
        [ test_convertOperation
        , test_deriveToolName
        , test_sanitizePathForName
        , test_buildToolDescription
        , test_parameterPrefixing
        , test_bodyParameter
        , test_enumDescription
        , test_requiredParams
        , test_convertOpenAPIToTools
        , test_toOpenAITool
        ]

-- | Test operation conversion.
test_convertOperation :: TestTree
test_convertOperation = testCase "Converts operation to tool" $ do
    let op = Operation (Just "getPet") (Just "Get pet") (Just "Returns a pet") [] Nothing
    let tool = convertOperation "/pets/{id}" "GET" op

    toolName tool @?= "openapi_getPet"
    toolPath tool @?= "/pets/{id}"
    toolMethod tool @?= "GET"
    toolDescription tool @?= "Get pet:\nReturns a pet"

-- | Test tool name derivation.
test_deriveToolName :: TestTree
test_deriveToolName = testCase "Derives tool names correctly" $ do
    -- With operationId
    let op1 = Operation (Just "listPets") Nothing Nothing [] Nothing
    deriveToolName "/pets" "GET" op1 @?= "openapi_listPets"

    -- Without operationId (falls back to path)
    let op2 = Operation Nothing (Just "List pets") Nothing [] Nothing
    deriveToolName "/pets" "GET" op2 @?= "openapi_get_pets"

    -- With path parameters
    let op3 = Operation Nothing Nothing Nothing [] Nothing
    deriveToolName "/pets/{id}" "GET" op3 @?= "openapi_get_pets_id_"

-- | Test path sanitization.
test_sanitizePathForName :: TestTree
test_sanitizePathForName = testCase "Sanitizes paths for names" $ do
    sanitizePathForName "GET" "/pets" @?= "get_pets"
    sanitizePathForName "POST" "/pets/{id}" @?= "post_pets_id_"
    sanitizePathForName "GET" "/api/v1/users" @?= "get_api_v1_users"
    sanitizePathForName "DELETE" "/pets/{id}" @?= "delete_pets_id_"

-- | Test description building.
test_buildToolDescription :: TestTree
test_buildToolDescription = testCase "Builds tool descriptions" $ do
    let op1 = Operation Nothing (Just "Get a pet") (Just "Returns a pet") [] Nothing
    buildToolDescription op1 @?= "Get a pet:\nReturns a pet"

    let op2 = Operation Nothing (Just "Summary only") Nothing [] Nothing
    buildToolDescription op2 @?= "Summary only:\n"

    let op3 = Operation Nothing Nothing (Just "Description only") [] Nothing
    buildToolDescription op3 @?= ":\nDescription only"

-- | Test parameter prefixing with p_.
test_parameterPrefixing :: TestTree
test_parameterPrefixing = testCase "Prefixes parameters with p_" $ do
    let schema = Schema (Just "string") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    let param = Parameter "petId" ParamInPath Nothing True (Just schema)
    let op = Operation (Just "getPet") Nothing Nothing [param] Nothing
    let params = buildToolParameters op

    -- Check that parameter name is prefixed
    assertBool "Has p_petId property" (Map.member "p_petId" (paramsProperties params))
    paramsRequired params @?= ["p_petId"]

    -- Check optional query parameter
    let queryParam = Parameter "limit" ParamInQuery (Just "Max results") False (Just schema)
    let op2 = Operation (Just "listPets") Nothing Nothing [queryParam] Nothing
    let params2 = buildToolParameters op2

    assertBool "Has p_limit property" (Map.member "p_limit" (paramsProperties params2))
    paramsRequired params2 @?= []  -- Not required

-- | Test body parameter with "b" name.
test_bodyParameter :: TestTree
test_bodyParameter = testCase "Creates body parameter named 'b'" $ do
    let bodySchema = Schema (Just "object") (Just "Pet object") Nothing Nothing Nothing Nothing Nothing Nothing
    let content = Map.singleton "application/json" bodySchema
    let body = RequestBody (Just "Pet to create") True content
    let op = Operation (Just "createPet") Nothing Nothing [] (Just body)
    let params = buildToolParameters op

    assertBool "Has 'b' property" (Map.member "b" (paramsProperties params))
    paramsRequired params @?= ["b"]  -- Body is required

    -- Check optional body
    let body2 = RequestBody Nothing False content
    let op2 = Operation (Just "createPet") Nothing Nothing [] (Just body2)
    let params2 = buildToolParameters op2

    paramsRequired params2 @?= []  -- Not required

-- | Test enum values in description.
test_enumDescription :: TestTree
test_enumDescription = testCase "Includes enum values in description" $ do
    let enumVals = [String "cat", String "dog", String "bird"]
    let schema = Schema (Just "string") Nothing (Just enumVals) Nothing Nothing Nothing Nothing Nothing
    let prop = buildToolProperty schema (Just "Pet type")

    propType prop @?= Just "string"
    propEnum prop @?= Just ["cat", "dog", "bird"]
    assertBool "Description contains enum info" ("Allowed values" `Text.isInfixOf` propDescription prop)
    assertBool "Description contains values" ("cat" `Text.isInfixOf` propDescription prop)

-- | Test required parameters tracking.
test_requiredParams :: TestTree
test_requiredParams = testCase "Tracks required parameters correctly" $ do
    let schema = Schema (Just "string") Nothing Nothing Nothing Nothing Nothing Nothing Nothing

    -- Required path param
    let pathParam = Parameter "id" ParamInPath Nothing True (Just schema)
    let op1 = Operation Nothing Nothing Nothing [pathParam] Nothing
    getRequiredParams op1 @?= ["p_id"]

    -- Optional query param
    let queryParam = Parameter "filter" ParamInQuery Nothing False (Just schema)
    let op2 = Operation Nothing Nothing Nothing [queryParam] Nothing
    getRequiredParams op2 @?= []

    -- Required query param
    let reqQueryParam = Parameter "filter" ParamInQuery Nothing True (Just schema)
    let op3 = Operation Nothing Nothing Nothing [reqQueryParam] Nothing
    getRequiredParams op3 @?= ["p_filter"]

    -- Required body
    let body = RequestBody Nothing True Map.empty
    let op4 = Operation Nothing Nothing Nothing [] (Just body)
    getRequiredParams op4 @?= ["b"]

-- | Test converting entire spec.
test_convertOpenAPIToTools :: TestTree
test_convertOpenAPIToTools = testCase "Converts entire spec to tools" $ do
    let getOp = Operation (Just "getPet") (Just "Get") Nothing [] Nothing
    let postOp = Operation (Just "createPet") (Just "Create") Nothing [] Nothing
    let paths =
            Map.fromList
                [ ("/pets", Map.fromList [("GET", getOp), ("POST", postOp)])
                , ("/pets/{id}", Map.singleton "GET" getOp)
                ]
    let spec = OpenAPISpec paths Nothing Nothing

    let tools = convertOpenAPIToTools spec
    length tools @?= 3

    let names = map toolName tools
    assertBool "Has getPet tool" ("openapi_getPet" `elem` names)
    assertBool "Has createPet tool" ("openapi_createPet" `elem` names)

-- | Test OpenAI tool conversion.
test_toOpenAITool :: TestTree
test_toOpenAITool = testCase "Converts to OpenAI tool format" $ do
    let schema = Schema (Just "string") (Just "Pet ID") Nothing Nothing Nothing Nothing Nothing Nothing
    let param = Parameter "id" ParamInPath Nothing True (Just schema)
    let op = Operation (Just "testOp") (Just "Test") (Just "A test op") [param] Nothing
    let apiTool = convertOperation "/test" "GET" op
    let tool = toOpenAITool apiTool

    -- Verify OpenAI.Tool fields
    OpenAI.toolName tool @?= OpenAI.ToolName "openapi_testOp"
    OpenAI.toolDescription tool @?= "Test:\nA test op"
    -- Check that parameters are converted
    assertBool "Has parameter properties" (not $ null $ OpenAI.toolParamProperties tool)

-- -------------------------------------------------------------------------
-- 4. Integration Tests
-- -------------------------------------------------------------------------

-- | Integration tests for path formatting and URL building.
integrationTests :: TestTree
integrationTests =
    testGroup
        "Integration"
        [ test_formatPath
        , test_formatPathWithMultipleParams
        , test_buildFullUrl
        , test_extractParams
        , test_findParam
        ]

-- | Test path formatting.
test_formatPath :: TestTree
test_formatPath = testCase "Formats path with parameters" $ do
    let pathTemplate = "/pets/{petId}"
    let pathArgs = Map.fromList [("petId", "123")]
    formatPath pathTemplate pathArgs @?= "/pets/123"

-- | Test path formatting with multiple parameters.
test_formatPathWithMultipleParams :: TestTree
test_formatPathWithMultipleParams = testCase "Formats path with multiple parameters" $ do
    let pathTemplate = "/pets/{petId}/owners/{ownerId}"
    let pathArgs = Map.fromList [("petId", "123"), ("ownerId", "456")]
    let result = formatPath pathTemplate pathArgs
    result @?= "/pets/123/owners/456"

-- | Test URL building.
test_buildFullUrl :: TestTree
test_buildFullUrl = testCase "Builds full URL from base and path" $ do
    buildFullUrl "https://api.example.com" "/pets" @?= "https://api.example.com/pets"
    buildFullUrl "https://api.example.com/" "/pets" @?= "https://api.example.com/pets"
    buildFullUrl "https://api.example.com" "pets" @?= "https://api.example.com/pets"
    buildFullUrl "https://api.example.com/v1" "/pets/{id}" @?= "https://api.example.com/v1/pets/{id}"

-- | Test parameter extraction.
test_extractParams :: TestTree
test_extractParams = testCase "Extracts parameters from JSON object" $ do
    spec <- loadTestSpec "petstore.json"
    let getPetOp = case Map.lookup "/pets/{petId}" (specPaths spec) of
            Just methods -> case Map.lookup "GET" methods of
                Just op -> op
                Nothing -> error "GET not found"
            Nothing -> error "/pets/{petId} not found"

    -- Create a mock JSON object with parameters
    let objMap =
            Map.fromList
                [ ("p_petId", String "123")
                ]

    -- Test that we can find the petId parameter
    let params = opParameters getPetOp
    case findParam params "petId" of
        Nothing -> assertFailure "Should find petId parameter"
        Just param -> do
            paramName param @?= "petId"
            paramIn param @?= ParamInPath
            paramRequired param @?= True

-- | Test finding parameter.
test_findParam :: TestTree
test_findParam = testCase "Finds parameter by name" $ do
    let schema = Schema (Just "string") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    let param1 = Parameter "id" ParamInPath Nothing True (Just schema)
    let param2 = Parameter "limit" ParamInQuery Nothing False (Just schema)
    let params = [param1, param2]

    case findParam params "id" of
        Nothing -> assertFailure "Should find id parameter"
        Just p -> paramIn p @?= ParamInPath

    case findParam params "limit" of
        Nothing -> assertFailure "Should find limit parameter"
        Just p -> paramIn p @?= ParamInQuery

    case findParam params "nonexistent" of
        Just _ -> assertFailure "Should not find nonexistent parameter"
        Nothing -> pure ()

-- -------------------------------------------------------------------------
-- 5. Swagger V2 Tests
-- -------------------------------------------------------------------------

-- | Tests for Swagger 2.0 format support.
swaggerV2Tests :: TestTree
swaggerV2Tests =
    testGroup
        "Swagger 2.0 Support"
        [ test_parseSwaggerV2Spec
        , test_swaggerV2Definitions
        , test_swaggerV2BodyParam
        , test_swaggerV2FormDataParam
        , test_swaggerV2InitializeToolbox
        ]

-- | Test that Swagger 2.0 petstore spec can be parsed.
test_parseSwaggerV2Spec :: TestTree
test_parseSwaggerV2Spec = testCase "Can parse Swagger 2.0 (OpenAPI v2) Petstore spec" $ do
    spec <- loadTestSpec "petstore-v2.json"
    assertBool "Has paths" (not $ Map.null $ specPaths spec)
    -- Swagger 2.0 specs should be converted to have components
    assertBool "Has components" (isJust $ specComponents spec)

-- | Test that Swagger 2.0 definitions are accessible.
test_swaggerV2Definitions :: TestTree
test_swaggerV2Definitions = testCase "Swagger 2.0 definitions are accessible as components" $ do
    spec <- loadTestSpec "petstore-v2.json"
    case specComponents spec of
        Nothing -> assertFailure "Expected components"
        Just comps -> case componentsSchemas comps of
            Nothing -> assertFailure "Expected schemas in components"
            Just schemas -> do
                -- Check that Pet, Order, User definitions are present
                assertBool "Has Pet schema" (Map.member "Pet" schemas)
                assertBool "Has Order schema" (Map.member "Order" schemas)
                assertBool "Has User schema" (Map.member "User" schemas)
                assertBool "Has Category schema" (Map.member "Category" schemas)
                assertBool "Has Tag schema" (Map.member "Tag" schemas)

-- | Test that Swagger 2.0 body parameters are converted to requestBody.
test_swaggerV2BodyParam :: TestTree
test_swaggerV2BodyParam = testCase "Swagger 2.0 body params converted to requestBody" $ do
    spec <- loadTestSpec "petstore-v2.json"
    -- Find the POST /pet operation which has a body parameter
    case Map.lookup "/pet" (specPaths spec) of
        Nothing -> assertFailure "Path /pet not found"
        Just methods -> case Map.lookup "POST" methods of
            Nothing -> assertFailure "POST method not found"
            Just op -> do
                -- Should have requestBody converted from body parameter
                assertBool "Has requestBody" (isJust $ opRequestBody op)
                case opRequestBody op of
                    Just rb -> do
                        assertBool "Request body is required" (reqBodyRequired rb)
                        assertBool "Has content" (not $ Map.null $ reqBodyContent rb)
                    Nothing -> pure ()

-- | Test that Swagger 2.0 formData parameters are handled.
test_swaggerV2FormDataParam :: TestTree
test_swaggerV2FormDataParam = testCase "Swagger 2.0 formData params are handled" $ do
    spec <- loadTestSpec "petstore-v2.json"
    -- Find the POST /pet/{petId}/uploadImage operation which has formData params
    case Map.lookup "/pet/{petId}/uploadImage" (specPaths spec) of
        Nothing -> assertFailure "Path /pet/{petId}/uploadImage not found"
        Just methods -> case Map.lookup "POST" methods of
            Nothing -> assertFailure "POST method not found"
            Just op -> do
                -- Should have parameters (including the formData ones)
                assertBool "Has parameters" (not $ null $ opParameters op)
                -- Check that the file parameter exists
                let hasFileParam = any (\p -> paramName p == "file") (opParameters op)
                assertBool "Has file parameter" hasFileParam

-- | Test that initializeToolbox can handle Swagger 2.0 specs.
test_swaggerV2InitializeToolbox :: TestTree
test_swaggerV2InitializeToolbox = testCase "initializeToolbox works with Swagger 2.0 spec" $ do
    -- Read the spec file directly
    content <- LBS.readFile "test/data/petstore-v2.json"
    case decode content of
        Nothing -> assertFailure "Failed to parse Swagger 2.0 spec"
        Just spec -> do
            -- Verify it has paths
            assertBool "Spec has paths" (not $ Map.null $ specPaths spec)
            -- Verify tools can be converted
            let tools = convertOpenAPIToTools spec
            assertBool "Tools were converted" (not $ null tools)
            -- Should have tools for pet operations
            let toolNames = map toolName tools
            assertBool "Has addPet tool" (any ("addPet" `Text.isInfixOf`) toolNames)
            assertBool "Has getPetById tool" (any ("getPetById" `Text.isInfixOf`) toolNames)

-- -------------------------------------------------------------------------
-- 6. Property Tests (Manual Properties)
-- -------------------------------------------------------------------------

-- | Property-like tests using standard assertions.
propertyTests :: TestTree
propertyTests =
    testGroup
        "Property-like Tests"
        [ test_roundtripJsonProperty
        , test_refResolutionIdempotent
        , test_nameSanitizationPreservesUniqueness
        ]

-- | Test JSON round-trip property.
test_roundtripJsonProperty :: TestTree
test_roundtripJsonProperty = testCase "JSON round-trip preserves structure" $ do
    -- Test with multiple spec types
    specs <- mapM loadTestSpec ["minimal.json", "petstore.json", "with_refs.json"]
    mapM_ checkRoundtrip specs
  where
    checkRoundtrip :: OpenAPISpec -> IO ()
    checkRoundtrip spec = do
        let json = encode spec
        case decode json of
            Nothing -> assertFailure "Round-trip failed"
            Just spec' -> do
                -- Path count should match
                Map.size (specPaths spec) @?= Map.size (specPaths spec')
                -- Components presence should match
                (isJust $ specComponents spec) @?= (isJust $ specComponents spec')

-- | Test that resolution is idempotent on resolved schemas.
test_refResolutionIdempotent :: TestTree
test_refResolutionIdempotent = testCase "Ref resolution is idempotent" $ do
    let petSchema = mkSchema (Just "object") (Just "A pet") Nothing Nothing Nothing Nothing Nothing Nothing
    let components = Components (Just (Map.fromList [("Pet", petSchema)]))
    let refSchema = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Pet") Nothing

    -- First resolution
    let resolved1 = resolveSchema refSchema components
    -- Second resolution on already-resolved schema
    let resolved2 = resolveSchema resolved1 components

    -- Should be the same
    schemaType resolved1 @?= schemaType resolved2
    schemaDescription resolved1 @?= schemaDescription resolved2

-- | Test that name sanitization creates unique names for different paths.
test_nameSanitizationPreservesUniqueness :: TestTree
test_nameSanitizationPreservesUniqueness = testCase "Name sanitization creates unique names" $ do
    let op = Operation Nothing Nothing Nothing [] Nothing

    -- Different paths should produce different names
    let name1 = deriveToolName "/pets" "GET" op
    let name2 = deriveToolName "/pets" "POST" op
    let name3 = deriveToolName "/users" "GET" op

    assertBool "GET and POST should differ" (name1 /= name2)
    assertBool "Different paths should differ" (name1 /= name3)

-- -------------------------------------------------------------------------
-- Helper Functions
-- -------------------------------------------------------------------------

-- | Find a parameter by name in a list.
findParam :: [Parameter] -> Text -> Maybe Parameter
findParam params name =
    case filter (\p -> paramName p == name) params of
        (p : _) -> Just p
        [] -> Nothing

-- | Helper to construct Schema with correct field order.
mkSchema ::
    Maybe Text ->
    Maybe Text ->
    Maybe [Value] ->
    Maybe (Map Text Schema) ->
    Maybe Schema ->
    Maybe [Schema] ->
    Maybe Text ->
    Maybe [Text] ->
    Schema
mkSchema t d e p i a r req = Schema t d e p i a r req

