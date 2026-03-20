{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Tests for luaToJsonValue function covering all scenarios from toto.lua.
--
-- These tests verify the conversion of Lua values to JSON values in various
-- situations including:
-- * Basic tables with mixed types
-- * Arrays (sequential integer keys starting at 1)
-- * Nested tables/objects
-- * Empty tables
-- * Edge cases with nil values
module LuaToolboxJsonValueTests where

import Control.Exception (bracket)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Test.Tasty
import Test.Tasty.HUnit

import Prod.Tracer (Tracer (..), silent)

import System.Agents.Base (LuaToolboxDescription (..))
import System.Agents.Tools.LuaToolbox as LuaToolbox

luaToJsonValueTests :: TestTree
luaToJsonValueTests =
    testGroup
        "luaToJsonValue Tests (from toto.lua scenarios)"
        [ testGroup "Test 1: Basic Table Encoding" basicTableTests
        , testGroup "Test 2: Array Encoding/Decoding" arrayTests
        , testGroup "Test 3: Data Type Handling" dataTypeTests
        , testGroup "Test 4: Nested Structures" nestedStructureTests
        , testGroup "Test 5: Edge Cases" edgeCaseTests
        , testGroup "Test 6: JSON Round-trip" roundTripTests
        , testGroup "Array Sequence Verification" arraySequenceTests
        ]

-------------------------------------------------------------------------------
-- Test Configuration
-------------------------------------------------------------------------------

testLuaToolbox :: LuaToolboxDescription
testLuaToolbox =
    LuaToolboxDescription
        { luaToolboxName = "test"
        , luaToolboxDescription = "Test toolbox"
        , luaToolboxMaxMemoryMB = 64
        , luaToolboxMaxExecutionTimeSeconds = 10
        , luaToolboxAllowedTools = []
        , luaToolboxAllowedPaths = []
        , luaToolboxAllowedHosts = []
        }

withTestToolbox :: (LuaToolbox.Toolbox -> IO ()) -> IO ()
withTestToolbox action = do
    initResult <- LuaToolbox.initializeToolbox silent testLuaToolbox
    case initResult of
        Left err -> assertFailure $ "Failed to initialize toolbox: " ++ err
        Right box -> do
            bracket
                (pure box)
                (\b -> LuaToolbox.closeToolbox silent b)
                action

-------------------------------------------------------------------------------
-- Test 1: Basic Table Encoding (from toto.lua Test 1)
-- Tests conversion of tables with:
-- * String values
-- * Integer values
-- * Boolean values
-- * Nested tables (objects)
-- * Array values (tables with sequential integer keys)
-------------------------------------------------------------------------------

basicTableTests :: [TestTree]
basicTableTests =
    [ testCase "Table with string, number, boolean" testBasicMixedTable
    , testCase "Table with nested object" testNestedObject
    , testCase "Table with array field" testTableWithArrayField
    , testCase "Complete Test 1 scenario" testCompleteTest1Scenario
    ]

testBasicMixedTable :: Assertion
testBasicMixedTable = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return {name = \"John\", age = 30, isStudent = false}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            let expected = Aeson.object
                    [ "name" .= ("John" :: Text)
                    , "age" .= (30 :: Int)
                    , "isStudent" .= False
                    ]
            execResult.resultValues @?= [expected]

testNestedObject :: Assertion
testNestedObject = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return {address = {city = \"New York\", zip = \"10001\"}}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            let expected = Aeson.object
                    [ "address" .= Aeson.object
                        [ "city" .= ("New York" :: Text)
                        , "zip" .= ("10001" :: Text)
                        ]
                    ]
            execResult.resultValues @?= [expected]

testTableWithArrayField :: Assertion
testTableWithArrayField = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return {scores = {95, 87, 92}}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            let expected = Aeson.object
                    [ "scores" .= Aeson.Array (Vector.fromList
                        [ Aeson.Number 95
                        , Aeson.Number 87
                        , Aeson.Number 92
                        ])
                    ]
            execResult.resultValues @?= [expected]

testCompleteTest1Scenario :: Assertion
testCompleteTest1Scenario = withTestToolbox $ \box -> do
    -- This is the exact table structure from toto.lua Test 1
    result <- LuaToolbox.executeScript box $ Text.unlines
        [ "local testTable = {"
        , "    name = \"John\","
        , "    age = 30,"
        , "    isStudent = false,"
        , "    scores = {95, 87, 92},"
        , "    address = {"
        , "        city = \"New York\","
        , "        zip = \"10001\""
        , "    }"
        , "}"
        , "return testTable"
        ]
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            let expected = Aeson.object
                    [ "name" .= ("John" :: Text)
                    , "age" .= (30 :: Int)
                    , "isStudent" .= False
                    , "scores" .= Aeson.Array (Vector.fromList
                        [ Aeson.Number 95
                        , Aeson.Number 87
                        , Aeson.Number 92
                        ])
                    , "address" .= Aeson.object
                        [ "city" .= ("New York" :: Text)
                        , "zip" .= ("10001" :: Text)
                        ]
                    ]
            execResult.resultValues @?= [expected]

-------------------------------------------------------------------------------
-- Test 2 & 3: Array Encoding/Decoding (from toto.lua Test 3)
-- Tests arrays with sequential integer keys starting at 1
-------------------------------------------------------------------------------

arrayTests :: [TestTree]
arrayTests =
    [ testCase "Simple string array" testSimpleStringArray
    , testCase "Number array" testNumberArray
    , testCase "Mixed type array" testMixedTypeArray
    , testCase "Empty array" testEmptyArray
    , testCase "Array with json.encode (from toto.lua Test 3)" testArrayWithJsonEncode
    ]

testSimpleStringArray :: Assertion
testSimpleStringArray = withTestToolbox $ \box -> do
    -- This is the exact test case from toto.lua Test 3
    result <- LuaToolbox.executeScript box $
        "return {\"apple\", \"banana\", \"cherry\"}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            let expected = Aeson.Array $ Vector.fromList
                    [ Aeson.String "apple"
                    , Aeson.String "banana"
                    , Aeson.String "cherry"
                    ]
            execResult.resultValues @?= [expected]

testNumberArray :: Assertion
testNumberArray = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return {1, 2, 3, 4, 5}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            let expected = Aeson.Array $ Vector.fromList
                    [ Aeson.Number 1
                    , Aeson.Number 2
                    , Aeson.Number 3
                    , Aeson.Number 4
                    , Aeson.Number 5
                    ]
            execResult.resultValues @?= [expected]

testMixedTypeArray :: Assertion
testMixedTypeArray = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return {\"hello\", 42, true, false}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            let expected = Aeson.Array $ Vector.fromList
                    [ Aeson.String "hello"
                    , Aeson.Number 42
                    , Aeson.Bool True
                    , Aeson.Bool False
                    ]
            execResult.resultValues @?= [expected]

testEmptyArray :: Assertion
testEmptyArray = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $ "return {}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            -- Empty table {} is detected as an empty array
            let expected = Aeson.Array Vector.empty
            execResult.resultValues @?= [expected]

testArrayWithJsonEncode :: Assertion
testArrayWithJsonEncode = withTestToolbox $ \box -> do
    -- Test the actual scenario from toto.lua: encoding then returning
    result <- LuaToolbox.executeScript box $ Text.unlines
        [ "local arr = {\"apple\", \"banana\", \"cherry\"}"
        , "local encoded = json.encode(arr)"
        , "return encoded"
        ]
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            -- json.encode returns a string
            let expected = Aeson.String "[\"apple\",\"banana\",\"cherry\"]"
            execResult.resultValues @?= [expected]

-------------------------------------------------------------------------------
-- Test 4: Data Type Handling (from toto.lua Test 4)
-- Tests various Lua data types and their JSON conversion
-------------------------------------------------------------------------------

dataTypeTests :: [TestTree]
dataTypeTests =
    [ testCase "String type" testStringType
    , testCase "Integer number" testIntegerNumber
    , testCase "Float number" testFloatNumber
    , testCase "Boolean true" testBooleanTrue
    , testCase "Boolean false" testBooleanFalse
    , testCase "Nil value" testNilValue
    , testCase "Nil in table (should be skipped)" testNilInTable
    , testCase "Complete Test 4 scenario" testCompleteTest4Scenario
    ]

testStringType :: Assertion
testStringType = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return {string = \"hello\"}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.object ["string" .= ("hello" :: Text)]]

testIntegerNumber :: Assertion
testIntegerNumber = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return {number = 42}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.object ["number" .= (42 :: Int)]]

testFloatNumber :: Assertion
testFloatNumber = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return {float = 3.14159}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            -- Check that we get a number close to the expected value
            case execResult.resultValues of
                [Aeson.Object obj] -> do
                    case KeyMap.lookup "float" obj of
                        Just (Aeson.Number n) -> do
                            let d = realToFrac n :: Double
                            assertBool "Float should be approximately 3.14159" $
                                abs (d - 3.14159) < 0.0001
                        _ -> assertFailure "Expected number value"
                _ -> assertFailure "Expected object result"

testBooleanTrue :: Assertion
testBooleanTrue = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return {boolean_true = true}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.object ["boolean_true" .= True]]

testBooleanFalse :: Assertion
testBooleanFalse = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return {boolean_false = false}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.object ["boolean_false" .= False]]

testNilValue :: Assertion
testNilValue = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return nil"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.Null]

testNilInTable :: Assertion
testNilInTable = withTestToolbox $ \box -> do
    -- Lua tables with nil values - the nil key-value pair is effectively
    -- not stored in Lua tables
    result <- LuaToolbox.executeScript box $
        "return {a = 1, b = nil, c = 3}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            -- nil values in tables are not encoded (Lua doesn't store them)
            case execResult.resultValues of
                [Aeson.Object obj] -> do
                    case KeyMap.lookup "b" obj of
                        Nothing -> pure () -- Expected: nil key is absent
                        Just _ -> assertFailure "nil value should not be present"
                    case KeyMap.lookup "a" obj of
                        Just (Aeson.Number 1) -> pure ()
                        _ -> assertFailure "Expected a=1"
                    case KeyMap.lookup "c" obj of
                        Just (Aeson.Number 3) -> pure ()
                        _ -> assertFailure "Expected c=3"
                _ -> assertFailure "Expected object result"

testCompleteTest4Scenario :: Assertion
testCompleteTest4Scenario = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $ Text.unlines
        [ "local typesTest = {"
        , "    string = \"hello\","
        , "    number = 42,"
        , "    float = 3.14159,"
        , "    boolean_true = true,"
        , "    boolean_false = false"
        , "}"
        , "return typesTest"
        ]
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [Aeson.Object obj] -> do
                    KeyMap.lookup "string" obj @?= Just (Aeson.String "hello")
                    KeyMap.lookup "number" obj @?= Just (Aeson.Number 42)
                    KeyMap.lookup "boolean_true" obj @?= Just (Aeson.Bool True)
                    KeyMap.lookup "boolean_false" obj @?= Just (Aeson.Bool False)
                    case KeyMap.lookup "float" obj of
                        Just (Aeson.Number n) -> do
                            let d = realToFrac n :: Double
                            assertBool "Float should be approximately 3.14159" $
                                abs (d - 3.14159) < 0.0001
                        _ -> assertFailure "Expected float number"
                _ -> assertFailure "Expected object result"

-------------------------------------------------------------------------------
-- Test 5 & 6: Nested Structures and Round-trip (from toto.lua Tests 5-6)
-- Tests deeply nested objects and arrays
-------------------------------------------------------------------------------

nestedStructureTests :: [TestTree]
nestedStructureTests =
    [ testCase "Array of objects" testArrayOfObjects
    , testCase "Object with nested arrays" testObjectWithNestedArrays
    , testCase "Deeply nested structure" testDeeplyNestedStructure
    , testCase "Complex structure (Test 5 scenario)" testComplexStructure
    ]

testArrayOfObjects :: Assertion
testArrayOfObjects = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return {{id = 1, name = \"Alice\"}, {id = 2, name = \"Bob\"}}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            let expected = Aeson.Array $ Vector.fromList
                    [ Aeson.object ["id" .= (1 :: Int), "name" .= ("Alice" :: Text)]
                    , Aeson.object ["id" .= (2 :: Int), "name" .= ("Bob" :: Text)]
                    ]
            execResult.resultValues @?= [expected]

testObjectWithNestedArrays :: Assertion
testObjectWithNestedArrays = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $ Text.unlines
        [ "return {"
        , "    product = \"Laptop\","
        , "    tags = {\"electronics\", \"computers\"}"
        , "}"
        ]
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            let expected = Aeson.object
                    [ "product" .= ("Laptop" :: Text)
                    , "tags" .= Aeson.Array (Vector.fromList
                        [ Aeson.String "electronics"
                        , Aeson.String "computers"
                        ])
                    ]
            execResult.resultValues @?= [expected]

testDeeplyNestedStructure :: Assertion
testDeeplyNestedStructure = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $ Text.unlines
        [ "return {"
        , "    level1 = {"
        , "        level2 = {"
        , "            level3 = {"
        , "                value = \"deep\""
        , "            }"
        , "        }"
        , "    }"
        , "}"
        ]
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            let expected = Aeson.object
                    [ "level1" .= Aeson.object
                        [ "level2" .= Aeson.object
                            [ "level3" .= Aeson.object
                                [ "value" .= ("deep" :: Text)
                                ]
                            ]
                        ]
                    ]
            execResult.resultValues @?= [expected]

testComplexStructure :: Assertion
testComplexStructure = withTestToolbox $ \box -> do
    -- This mimics the structure from toto.lua Test 5
    result <- LuaToolbox.executeScript box $ Text.unlines
        [ "return {"
        , "    product = \"Laptop\","
        , "    price = 999.99,"
        , "    inStock = true,"
        , "    tags = {\"electronics\", \"computers\"}"
        , "}"
        ]
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [Aeson.Object obj] -> do
                    KeyMap.lookup "product" obj @?= Just (Aeson.String "Laptop")
                    KeyMap.lookup "inStock" obj @?= Just (Aeson.Bool True)
                    case KeyMap.lookup "tags" obj of
                        Just (Aeson.Array tags) -> do
                            Vector.length tags @?= 2
                            tags Vector.! 0 @?= Aeson.String "electronics"
                            tags Vector.! 1 @?= Aeson.String "computers"
                        _ -> assertFailure "Expected tags array"
                    case KeyMap.lookup "price" obj of
                        Just (Aeson.Number n) -> do
                            let d = realToFrac n :: Double
                            assertBool "Price should be approximately 999.99" $
                                abs (d - 999.99) < 0.01
                        _ -> assertFailure "Expected price number"
                _ -> assertFailure "Expected object result"

-------------------------------------------------------------------------------
-- Edge Cases
-------------------------------------------------------------------------------

edgeCaseTests :: [TestTree]
edgeCaseTests =
    [ testCase "Single element array" testSingleElementArray
    , testCase "Sparse array (not really an array)" testSparseArray
    , testCase "Mixed keys (string and integer)" testMixedKeys
    , testCase "Empty string value" testEmptyString
    , testCase "Unicode string" testUnicodeString
    , testCase "Negative numbers" testNegativeNumbers
    , testCase "Zero values" testZeroValues
    ]

testSingleElementArray :: Assertion
testSingleElementArray = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $ "return {\"only\"}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            let expected = Aeson.Array $ Vector.fromList [Aeson.String "only"]
            execResult.resultValues @?= [expected]

testSparseArray :: Assertion
testSparseArray = withTestToolbox $ \box -> do
    -- A sparse array (non-contiguous indices) should be treated as an object
    result <- LuaToolbox.executeScript box $ "return {[1] = \"a\", [3] = \"c\"}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            -- This will be an object with string keys "1" and "3"
            -- because it's not a contiguous array
            case execResult.resultValues of
                [Aeson.Object obj] -> do
                    -- The keys should be strings "1" and "3"
                    assertBool "Should have key '1' or 1" $
                        KeyMap.member "1" obj
                    assertBool "Should have key '3' or 3" $
                        KeyMap.member "3" obj
                _ -> assertFailure "Expected object result"

testMixedKeys :: Assertion
testMixedKeys = withTestToolbox $ \box -> do
    -- Table with both string and integer keys is an object
    result <- LuaToolbox.executeScript box $
        "return {[1] = \"a\", name = \"test\"}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [Aeson.Object obj] -> do
                    assertBool "Should have 'name' key" $ KeyMap.member "name" obj
                _ -> assertFailure "Expected object result"

testEmptyString :: Assertion
testEmptyString = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $ "return {empty = \"\"}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.object ["empty" .= ("" :: Text)]]

testUnicodeString :: Assertion
testUnicodeString = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return {message = \"Hello 世界 🌍\"}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.object ["message" .= ("Hello 世界 🌍" :: Text)]]

testNegativeNumbers :: Assertion
testNegativeNumbers = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $ "return {neg = -42, negFloat = -3.14}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [Aeson.Object obj] -> do
                    KeyMap.lookup "neg" obj @?= Just (Aeson.Number (-42))
                    case KeyMap.lookup "negFloat" obj of
                        Just (Aeson.Number n) -> do
                            let d = realToFrac n :: Double
                            assertBool "Negative float should be approximately -3.14" $
                                abs (d + 3.14) < 0.01
                        _ -> assertFailure "Expected negFloat number"
                _ -> assertFailure "Expected object result"

testZeroValues :: Assertion
testZeroValues = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $ "return {zero = 0, bool = false}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.object ["zero" .= (0 :: Int), "bool" .= False]]

-------------------------------------------------------------------------------
-- Array Sequence Verification
-- Additional tests to ensure arrays are properly detected with sequential indices
-------------------------------------------------------------------------------

arraySequenceTests :: [TestTree]
arraySequenceTests =
    [ testCase "Array starting at index 1 is detected as array" testArrayStartsAtOne
    , testCase "Array not starting at index 1 is an object" testArrayNotStartingAtOne
    , testCase "Array with gap at beginning is an object" testArrayGapAtBeginning
    , testCase "Array with gap at end is an object" testArrayGapAtEnd
    , testCase "Array with gap in middle is an object" testArrayGapInMiddle
    , testCase "Array with n field is an object (mixed keys)" testArrayWithNField
    , testCase "Deeply nested array in object" testDeeplyNestedArrayInObject
    , testCase "Multiple array fields in object" testMultipleArrayFields
    , testCase "Large array preserves order" testLargeArrayPreservesOrder
    , testCase "Array index 0 makes it an object" testArrayIndexZero
    ]

testArrayStartsAtOne :: Assertion
testArrayStartsAtOne = withTestToolbox $ \box -> do
    -- Standard Lua array starting at 1
    result <- LuaToolbox.executeScript box $ "return {\"a\", \"b\", \"c\"}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            let expected = Aeson.Array $ Vector.fromList
                    [ Aeson.String "a"
                    , Aeson.String "b"
                    , Aeson.String "c"
                    ]
            execResult.resultValues @?= [expected]

testArrayNotStartingAtOne :: Assertion
testArrayNotStartingAtOne = withTestToolbox $ \box -> do
    -- Lua table starting at index 2 should be an object
    result <- LuaToolbox.executeScript box $ "return {[2] = \"a\", [3] = \"b\"}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            -- Should be object with string keys "2" and "3"
            case execResult.resultValues of
                [Aeson.Object obj] -> do
                    KeyMap.lookup "2" obj @?= Just (Aeson.String "a")
                    KeyMap.lookup "3" obj @?= Just (Aeson.String "b")
                _ -> assertFailure "Expected object (not array) when not starting at 1"

testArrayGapAtBeginning :: Assertion
testArrayGapAtBeginning = withTestToolbox $ \box -> do
    -- Gap at the beginning (nil at index 1)
    result <- LuaToolbox.executeScript box $ "return {[2] = \"a\", [3] = \"b\"}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [Aeson.Object obj] -> do
                    -- Should not have key "1"
                    KeyMap.lookup "1" obj @?= Nothing
                    KeyMap.lookup "2" obj @?= Just (Aeson.String "a")
                _ -> assertFailure "Expected object with gap"

testArrayGapAtEnd :: Assertion
testArrayGapAtEnd = withTestToolbox $ \box -> do
    -- Gap at the end would mean the table has non-contiguous keys
    -- {1, 2, nil, 4} - but nil values don't exist in Lua tables
    -- So we test with explicit keys
    result <- LuaToolbox.executeScript box $ "return {[1] = \"a\", [2] = \"b\", [4] = \"d\"}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [Aeson.Object obj] -> do
                    -- Should be object because index 3 is missing
                    KeyMap.lookup "1" obj @?= Just (Aeson.String "a")
                    KeyMap.lookup "2" obj @?= Just (Aeson.String "b")
                    KeyMap.lookup "4" obj @?= Just (Aeson.String "d")
                    -- Index 3 should not exist
                    KeyMap.lookup "3" obj @?= Nothing
                _ -> assertFailure "Expected object due to gap"

testArrayGapInMiddle :: Assertion
testArrayGapInMiddle = withTestToolbox $ \box -> do
    -- Gap in the middle
    result <- LuaToolbox.executeScript box $ "return {[1] = \"a\", [3] = \"c\"}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [Aeson.Object obj] -> do
                    KeyMap.lookup "1" obj @?= Just (Aeson.String "a")
                    KeyMap.lookup "3" obj @?= Just (Aeson.String "c")
                    KeyMap.lookup "2" obj @?= Nothing
                _ -> assertFailure "Expected object due to gap in middle"

testArrayWithNField :: Assertion
testArrayWithNField = withTestToolbox $ \box -> do
    -- Table with sequential indices but also an 'n' field
    result <- LuaToolbox.executeScript box $
        "return {[1] = \"a\", [2] = \"b\", n = 2}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            -- Should be object because it has a string key "n"
            case execResult.resultValues of
                [Aeson.Object obj] -> do
                    KeyMap.lookup "n" obj @?= Just (Aeson.Number 2)
                    KeyMap.lookup "1" obj @?= Just (Aeson.String "a")
                    KeyMap.lookup "2" obj @?= Just (Aeson.String "b")
                _ -> assertFailure "Expected object due to mixed keys"

testDeeplyNestedArrayInObject :: Assertion
testDeeplyNestedArrayInObject = withTestToolbox $ \box -> do
    -- Object with deeply nested array (like scores in toto.lua)
    result <- LuaToolbox.executeScript box $ Text.unlines
        [ "return {"
        , "    student = {"
        , "        name = \"Alice\","
        , "        grades = {95, 87, 92}"
        , "    }"
        , "}"
        ]
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [Aeson.Object obj] -> do
                    case KeyMap.lookup "student" obj of
                        Just (Aeson.Object student) -> do
                            KeyMap.lookup "name" student @?= Just (Aeson.String "Alice")
                            case KeyMap.lookup "grades" student of
                                Just (Aeson.Array grades) -> do
                                    Vector.length grades @?= 3
                                    grades Vector.! 0 @?= Aeson.Number 95
                                    grades Vector.! 1 @?= Aeson.Number 87
                                    grades Vector.! 2 @?= Aeson.Number 92
                                _ -> assertFailure "Expected grades to be array"
                        _ -> assertFailure "Expected student object"
                _ -> assertFailure "Expected object result"

testMultipleArrayFields :: Assertion
testMultipleArrayFields = withTestToolbox $ \box -> do
    -- Object with multiple array fields
    result <- LuaToolbox.executeScript box $ Text.unlines
        [ "return {"
        , "    names = {\"Alice\", \"Bob\"},"
        , "    scores = {95, 87},"
        , "    active = {true, false, true}"
        , "}"
        ]
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [Aeson.Object obj] -> do
                    case KeyMap.lookup "names" obj of
                        Just (Aeson.Array names) -> Vector.length names @?= 2
                        _ -> assertFailure "Expected names array"
                    case KeyMap.lookup "scores" obj of
                        Just (Aeson.Array scores) -> Vector.length scores @?= 2
                        _ -> assertFailure "Expected scores array"
                    case KeyMap.lookup "active" obj of
                        Just (Aeson.Array active) -> Vector.length active @?= 3
                        _ -> assertFailure "Expected active array"
                _ -> assertFailure "Expected object result"

testLargeArrayPreservesOrder :: Assertion
testLargeArrayPreservesOrder = withTestToolbox $ \box -> do
    -- Larger array to ensure order is preserved
    result <- LuaToolbox.executeScript box $
        "return {10, 20, 30, 40, 50, 60, 70, 80, 90, 100}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            let expected = Aeson.Array $ Vector.fromList
                    [ Aeson.Number 10, Aeson.Number 20, Aeson.Number 30
                    , Aeson.Number 40, Aeson.Number 50, Aeson.Number 60
                    , Aeson.Number 70, Aeson.Number 80, Aeson.Number 90
                    , Aeson.Number 100
                    ]
            execResult.resultValues @?= [expected]

testArrayIndexZero :: Assertion
testArrayIndexZero = withTestToolbox $ \box -> do
    -- Lua "array" with index 0 should be an object
    result <- LuaToolbox.executeScript box $
        "return {[0] = \"zero\", [1] = \"one\"}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [Aeson.Object obj] -> do
                    KeyMap.lookup "0" obj @?= Just (Aeson.String "zero")
                    KeyMap.lookup "1" obj @?= Just (Aeson.String "one")
                _ -> assertFailure "Expected object when index 0 is present"

-------------------------------------------------------------------------------
-- Round-trip Tests (from toto.lua Test 6)
-- Tests encoding to JSON then decoding back
-------------------------------------------------------------------------------

roundTripTests :: [TestTree]
roundTripTests =
    [ testCase "Simple round-trip" testSimpleRoundTrip
    , testCase "Complex round-trip (Test 6 scenario)" testComplexRoundTrip
    , testCase "Array round-trip" testArrayRoundTrip
    ]

testSimpleRoundTrip :: Assertion
testSimpleRoundTrip = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $ Text.unlines
        [ "local original = {foo = \"bar\", count = 42}"
        , "local encoded = json.encode(original)"
        , "local decoded = json.decode(encoded)"
        , "return decoded"
        ]
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            let expected = Aeson.object ["foo" .= ("bar" :: Text), "count" .= (42 :: Int)]
            execResult.resultValues @?= [expected]

testComplexRoundTrip :: Assertion
testComplexRoundTrip = withTestToolbox $ \box -> do
    -- This is the exact scenario from toto.lua Test 6
    result <- LuaToolbox.executeScript box $ Text.unlines
        [ "local original = {"
        , "    users = {"
        , "        {id = 1, name = \"Alice\"},"
        , "        {id = 2, name = \"Bob\"},"
        , "        {id = 3, name = \"Charlie\"}"
        , "    },"
        , "    count = 3,"
        , "    active = true"
        , "}"
        , "local roundTrip = json.decode(json.encode(original))"
        , "return roundTrip"
        ]
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            case execResult.resultValues of
                [Aeson.Object obj] -> do
                    KeyMap.lookup "count" obj @?= Just (Aeson.Number 3)
                    KeyMap.lookup "active" obj @?= Just (Aeson.Bool True)
                    case KeyMap.lookup "users" obj of
                        Just (Aeson.Array users) -> do
                            Vector.length users @?= 3
                            users Vector.! 0 @?= Aeson.object ["id" .= (1 :: Int), "name" .= ("Alice" :: Text)]
                            users Vector.! 1 @?= Aeson.object ["id" .= (2 :: Int), "name" .= ("Bob" :: Text)]
                            users Vector.! 2 @?= Aeson.object ["id" .= (3 :: Int), "name" .= ("Charlie" :: Text)]
                        _ -> assertFailure "Expected users array"
                _ -> assertFailure "Expected object result"

testArrayRoundTrip :: Assertion
testArrayRoundTrip = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box $ Text.unlines
        [ "local original = {1, 2, 3, \"four\", true}"
        , "local encoded = json.encode(original)"
        , "local decoded = json.decode(encoded)"
        , "return decoded"
        ]
    case result of
        Left err -> assertFailure $ show err
        Right execResult -> do
            let expected = Aeson.Array $ Vector.fromList
                    [ Aeson.Number 1
                    , Aeson.Number 2
                    , Aeson.Number 3
                    , Aeson.String "four"
                    , Aeson.Bool True
                    ]
            execResult.resultValues @?= [expected]

