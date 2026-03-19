{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Comprehensive tests for LuaToolbox JSON functionality
--
-- Tests json.encode, json.decode, and round-trip operations
module LuaToolboxTests
    ( luaToolboxTestSuite
    ) where

import Data.Aeson (Value(..), decode, encode)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Tools.LuaToolbox
import System.Agents.Tools.LuaToolbox.Modules.Json

-- | Main test suite for LuaToolbox
luaToolboxTestSuite :: TestTree
luaToolboxTestSuite =
    testGroup
        "LuaToolbox Tests"
        [ jsonAvailableTests
        , jsonEncodeTests
        , jsonDecodeTests
        , jsonRoundTripTests
        ]

-------------------------------------------------------------------------------
-- JSON Module Availability Tests
-------------------------------------------------------------------------------

jsonAvailableTests :: TestTree
jsonAvailableTests =
    testGroup
        "JSON Module Availability"
        [ testCase "json module is available" testJsonAvailable
        , testCase "json.encode function exists" testJsonEncodeExists
        , testCase "json.decode function exists" testJsonDecodeExists
        ]

-- | Test that the json module is available in the toolbox
testJsonAvailable :: Assertion
testJsonAvailable = withTestToolbox $ \box -> do
    result <- executeScript box $
        "return type(json)"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            -- json should be a table
            execResult.resultValues @?= [String "table"]

-- | Test that json.encode function is available
testJsonEncodeExists :: Assertion
testJsonEncodeExists = withTestToolbox $ \box -> do
    result <- executeScript box $
        "return type(json.encode)"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [String "function"]

-- | Test that json.decode function is available
testJsonDecodeExists :: Assertion
testJsonDecodeExists = withTestToolbox $ \box -> do
    result <- executeScript box $
        "return type(json.decode)"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [String "function"]

-------------------------------------------------------------------------------
-- json.encode Tests
-------------------------------------------------------------------------------

jsonEncodeTests :: TestTree
jsonEncodeTests =
    testGroup
        "json.encode"
        [ testCase "encode simple object" testJsonEncodeObject
        , testCase "encode array" testJsonEncodeArray
        , testCase "encode nested tables" testJsonEncodeNested
        , testCase "encode mixed types" testJsonEncodeMixedTypes
        , testCase "encode empty table" testJsonEncodeEmpty
        , testCase "encode with null values" testJsonEncodeWithNull
        , testCase "encode boolean values" testJsonEncodeBooleans
        , testCase "encode numbers" testJsonEncodeNumbers
        , testCase "encode strings with special characters" testJsonEncodeSpecialStrings
        ]

-- | Encode a simple table (object)
testJsonEncodeObject :: Assertion
testJsonEncodeObject = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local data = {name = \"test\", count = 42}\n" ++
        "return json.encode(data)"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            -- Should return a JSON string - check it's valid JSON
            case execResult.resultValues of
                [String jsonStr] -> do
                    -- Parse the result to verify it's valid JSON
                    case decode (encode jsonStr) :: Maybe Value of
                        Just _ -> pure ()  -- Valid JSON
                        Nothing -> assertFailure "Result is not valid JSON"
                _ -> assertFailure "Expected single string result"

-- | Encode an array
testJsonEncodeArray :: Assertion
testJsonEncodeArray = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local arr = {1, 2, 3}\n" ++
        "return json.encode(arr)"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [String jsonStr] -> do
                    -- Verify it's a valid JSON array
                    case decode (encode jsonStr) :: Maybe Value of
                        Just _ -> pure ()
                        Nothing -> assertFailure "Result is not valid JSON array"
                _ -> assertFailure "Expected single string result"

-- | Encode nested tables
testJsonEncodeNested :: Assertion
testJsonEncodeNested = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local data = {person = {name = \"John\", age = 30}}\n" ++
        "return json.encode(data)"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [String jsonStr] -> do
                    case decode (encode jsonStr) :: Maybe Value of
                        Just _ -> pure ()
                        Nothing -> assertFailure "Result is not valid JSON"
                _ -> assertFailure "Expected single string result"

-- | Encode mixed types
testJsonEncodeMixedTypes :: Assertion
testJsonEncodeMixedTypes = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local data = {\n" ++
        "  str = \"hello\",\n" ++
        "  num = 42,\n" ++
        "  float = 3.14,\n" ++
        "  bool = true,\n" ++
        "  arr = {1, 2, 3},\n" ++
        "  obj = {nested = \"value\"}\n" ++
        "}\n" ++
        "return json.encode(data)"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [String jsonStr] -> do
                    case decode (encode jsonStr) :: Maybe Value of
                        Just _ -> pure ()
                        Nothing -> assertFailure "Result is not valid JSON"
                _ -> assertFailure "Expected single string result"

-- | Encode empty table
testJsonEncodeEmpty :: Assertion
testJsonEncodeEmpty = withTestToolbox $ \box -> do
    result <- executeScript box $
        "return json.encode({})"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [String jsonStr] -> do
                    -- Empty table encodes to empty object {} or array []
                    jsonStr `elem` ["{}", "[]"] @? 
                        "Expected empty object or array, got: " ++ Text.unpack jsonStr
                _ -> assertFailure "Expected single string result"

-- | Encode with null values
testJsonEncodeWithNull :: Assertion
testJsonEncodeWithNull = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local data = {value = \"test\", nothing = nil}\n" ++
        "return json.encode(data)"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [String jsonStr] -> do
                    case decode (encode jsonStr) :: Maybe Value of
                        Just _ -> pure ()
                        Nothing -> assertFailure "Result is not valid JSON"
                _ -> assertFailure "Expected single string result"

-- | Encode boolean values
testJsonEncodeBooleans :: Assertion
testJsonEncodeBooleans = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local data = {active = true, deleted = false}\n" ++
        "return json.encode(data)"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [String jsonStr] -> do
                    -- Verify booleans are encoded correctly
                    Text.isInfixOf "true" jsonStr @? "Expected 'true' in JSON"
                    Text.isInfixOf "false" jsonStr @? "Expected 'false' in JSON"
                _ -> assertFailure "Expected single string result"

-- | Encode numbers (integers and floats)
testJsonEncodeNumbers :: Assertion
testJsonEncodeNumbers = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local data = {int = 42, neg = -17, float = 3.14159, exp = 1.5e10}\n" ++
        "return json.encode(data)"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [String jsonStr] -> do
                    case decode (encode jsonStr) :: Maybe Value of
                        Just _ -> pure ()
                        Nothing -> assertFailure "Result is not valid JSON"
                _ -> assertFailure "Expected single string result"

-- | Encode strings with special characters
testJsonEncodeSpecialStrings :: Assertion
testJsonEncodeSpecialStrings = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local data = {\n" ++
        "  quote = 'say \"hello\"',\n" ++
        "  newline = \"line1\\nline2\",\n" ++
        "  tab = \"col1\\tcol2\",\n" ++
        "  backslash = \"path\\\\to\\\\file\"\n" ++
        "}\n" ++
        "return json.encode(data)"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [String jsonStr] -> do
                    case decode (encode jsonStr) :: Maybe Value of
                        Just _ -> pure ()
                        Nothing -> assertFailure "Result is not valid JSON"
                _ -> assertFailure "Expected single string result"

-------------------------------------------------------------------------------
-- json.decode Tests
-------------------------------------------------------------------------------

jsonDecodeTests :: TestTree
jsonDecodeTests =
    testGroup
        "json.decode"
        [ testCase "decode simple object" testJsonDecodeObject
        , testCase "decode array" testJsonDecodeArray
        , testCase "decode nested JSON" testJsonDecodeNested
        , testCase "decode with error handling" testJsonDecodeError
        , testCase "decode numbers" testJsonDecodeNumbers
        , testCase "decode booleans and null" testJsonDecodeBooleansNull
        , testCase "decode empty object" testJsonDecodeEmptyObject
        , testCase "decode empty array" testJsonDecodeEmptyArray
        ]

-- | Decode JSON object to table
testJsonDecodeObject :: Assertion
testJsonDecodeObject = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local data, err = json.decode('{\"key\": \"value\", \"num\": 123}')\n" ++
        "if err then return nil, err end\n" ++
        "return data.key, data.num"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            -- Should return the decoded values
            case execResult.resultValues of
                [String val, Number num] -> do
                    val @?= "value"
                    num @?= 123
                _ -> assertFailure "Expected string and number results"

-- | Decode JSON array
testJsonDecodeArray :: Assertion
testJsonDecodeArray = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local data, err = json.decode('[1, 2, 3]')\n" ++
        "if err then return nil, err end\n" ++
        "return #data, data[1], data[2], data[3]"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            -- Should return length and array elements
            case execResult.resultValues of
                [Number len, Number n1, Number n2, Number n3] -> do
                    len @?= 3
                    n1 @?= 1
                    n2 @?= 2
                    n3 @?= 3
                _ -> assertFailure "Expected four number results"

-- | Decode nested JSON
testJsonDecodeNested :: Assertion
testJsonDecodeNested = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local jsonStr = '{\"person\": {\"name\": \"John\", \"age\": 30}, \"items\": [1, 2]}'\n" ++
        "local data, err = json.decode(jsonStr)\n" ++
        "if err then return nil, err end\n" ++
        "return data.person.name, data.person.age, #data.items"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [String name, Number age, Number itemCount] -> do
                    name @?= "John"
                    age @?= 30
                    itemCount @?= 2
                _ -> assertFailure "Expected name, age, and item count"

-- | Decode with error handling - invalid JSON
testJsonDecodeError :: Assertion
testJsonDecodeError = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local data, err = json.decode('{invalid json}')\n" ++
        "return data, err"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            -- Should return nil and error message
            case execResult.resultValues of
                [Null, String errMsg] -> do
                    assertBool "Error message should mention invalid/parsing" $
                        Text.isInfixOf "Invalid" errMsg || 
                        Text.isInfixOf "parse" errMsg ||
                        Text.isInfixOf "error" (Text.toLower errMsg)
                _ -> assertFailure "Expected nil and error message"

-- | Decode numbers (integers, floats, scientific notation)
testJsonDecodeNumbers :: Assertion
testJsonDecodeNumbers = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local data, err = json.decode('{\"int\": 42, \"float\": 3.14, \"neg\": -17, \"sci\": 1.5e10}')\n" ++
        "if err then return nil, err end\n" ++
        "return data.int, data.float, data.neg, data.sci"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [Number n1, Number n2, Number n3, Number n4] -> do
                    n1 @?= 42
                    n2 @?= 3.14
                    n3 @?= -17
                    n4 @?= 1.5e10
                _ -> assertFailure "Expected four number results"

-- | Decode booleans and null
testJsonDecodeBooleansNull :: Assertion
testJsonDecodeBooleansNull = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local data, err = json.decode('{\"active\": true, \"deleted\": false, \"empty\": null}')\n" ++
        "if err then return nil, err end\n" ++
        "return data.active, data.deleted, data.empty"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [Bool True, Bool False, Null] -> pure ()
                _ -> assertFailure "Expected true, false, and null"

-- | Decode empty object
testJsonDecodeEmptyObject :: Assertion
testJsonDecodeEmptyObject = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local data, err = json.decode('{}')\n" ++
        "if err then return nil, err end\n" ++
        "return type(data), #data"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [String t, Number n] -> do
                    t @?= "table"
                    n @?= 0
                _ -> assertFailure "Expected type and length"

-- | Decode empty array
testJsonDecodeEmptyArray :: Assertion
testJsonDecodeEmptyArray = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local data, err = json.decode('[]')\n" ++
        "if err then return nil, err end\n" ++
        "return type(data), #data"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [String t, Number n] -> do
                    t @?= "table"
                    n @?= 0
                _ -> assertFailure "Expected type and length"

-------------------------------------------------------------------------------
-- Round-trip Tests (encode then decode)
-------------------------------------------------------------------------------

jsonRoundTripTests :: TestTree
jsonRoundTripTests =
    testGroup
        "Round-trip (encode -> decode)"
        [ testCase "round-trip simple object" testRoundTripObject
        , testCase "round-trip array" testRoundTripArray
        , testCase "round-trip nested structure" testRoundTripNested
        , testCase "round-trip with mixed types" testRoundTripMixedTypes
        , testCase "round-trip empty table" testRoundTripEmpty
        ]

-- | Round-trip simple object
testRoundTripObject :: Assertion
testRoundTripObject = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local original = {name = \"test\", count = 42}\n" ++
        "local encoded = json.encode(original)\n" ++
        "local decoded, err = json.decode(encoded)\n" ++
        "if err then return nil, err end\n" ++
        "return decoded.name == original.name and decoded.count == original.count"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Bool True]

-- | Round-trip array
testRoundTripArray :: Assertion
testRoundTripArray = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local original = {10, 20, 30}\n" ++
        "local encoded = json.encode(original)\n" ++
        "local decoded, err = json.decode(encoded)\n" ++
        "if err then return nil, err end\n" ++
        "return #decoded == #original and decoded[1] == original[1] and decoded[2] == original[2] and decoded[3] == original[3]"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Bool True]

-- | Round-trip nested structure
testRoundTripNested :: Assertion
testRoundTripNested = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local original = {\n" ++
        "  person = {name = \"Alice\", age = 25},\n" ++
        "  hobbies = {\"reading\", \"coding\"},\n" ++
        "  address = {city = \"NYC\", zip = 10001}\n" ++
        "}\n" ++
        "local encoded = json.encode(original)\n" ++
        "local decoded, err = json.decode(encoded)\n" ++
        "if err then return nil, err end\n" ++
        "return decoded.person.name == original.person.name and\n" ++
        "       decoded.hobbies[1] == original.hobbies[1] and\n" ++
        "       decoded.address.zip == original.address.zip"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Bool True]

-- | Round-trip with mixed types
testRoundTripMixedTypes :: Assertion
testRoundTripMixedTypes = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local original = {\n" ++
        "  str = \"hello\",\n" ++
        "  num = 42,\n" ++
        "  float = 3.14,\n" ++
        "  bool = true,\n" ++
        "  arr = {1, 2, 3}\n" ++
        "}\n" ++
        "local encoded = json.encode(original)\n" ++
        "local decoded, err = json.decode(encoded)\n" ++
        "if err then return nil, err end\n" ++
        "return decoded.str == original.str and\n" ++
        "       decoded.num == original.num and\n" ++
        "       decoded.float == original.float and\n" ++
        "       decoded.bool == original.bool and\n" ++
        "       #decoded.arr == #original.arr"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Bool True]

-- | Round-trip empty table
testRoundTripEmpty :: Assertion
testRoundTripEmpty = withTestToolbox $ \box -> do
    result <- executeScript box $
        "local original = {}\n" ++
        "local encoded = json.encode(original)\n" ++
        "local decoded, err = json.decode(encoded)\n" ++
        "if err then return nil, err end\n" ++
        "return type(decoded) == \"table\" and #decoded == 0"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Bool True]

-------------------------------------------------------------------------------
-- Test Helpers
-------------------------------------------------------------------------------

-- | Run a test with a fresh toolbox instance
withTestToolbox :: (LuaToolbox -> IO a) -> IO a
withTestToolbox = withToolbox

