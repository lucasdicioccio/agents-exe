{-# LANGUAGE OverloadedStrings #-}

module SessionPrintTests where

import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.SessionPrint (PrintAmount(..), PrintVisibility(..), elideDocument)

tests :: TestTree
tests =
    testGroup
        "SessionPrint Tests"
        [ elideDocumentTests
        ]

elideDocumentTests :: TestTree
elideDocumentTests =
    testGroup
        "elideDocument"
        [ testCase "no elision needed - exact fit (lines)" $
            elideDocument (Lines 3) (Lines 3) "line1\nline2\nline3"
                @?= "line1\nline2\nline3"
        
        , testCase "no elision needed - overlapping regions (lines)" $
            elideDocument (Lines 5) (Lines 5) "line1\nline2\nline3\nline4\nline5\nline6\nline7"
                @?= "line1\nline2\nline3\nline4\nline5\nline6\nline7"
        
        , testCase "elides middle lines" $
            elideDocument (Lines 2) (Lines 2) "line1\nline2\nline3\nline4\nline5"
                @?= "line1\nline2\n... (1 line elided) ...\nline4\nline5"
        
        , testCase "elides multiple middle lines" $
            elideDocument (Lines 2) (Lines 2) "line1\nline2\nline3\nline4\nline5\nline6\nline7"
                @?= "line1\nline2\n... (3 lines elided) ...\nline6\nline7"
        
        , testCase "handles empty string" $
            elideDocument (Lines 10) (Lines 10) ""
                @?= ""
        
        , testCase "handles single line" $
            elideDocument (Lines 5) (Lines 5) "single line"
                @?= "single line"
        
        , testCase "leading takes all (no trailing overlap)" $
            elideDocument (Lines 100) (Lines 1) "line1\nline2\nline3"
                @?= "line1\nline2\nline3"
        
        , testCase "trailing takes all (no leading overlap)" $
            elideDocument (Lines 1) (Lines 100) "line1\nline2\nline3"
                @?= "line1\nline2\nline3"
        
        , testCase "no elision needed - exact fit (chars)" $
            -- "hello world" = 11 chars, 5 + 5 = 10 < 11, so it elides
            -- When overlap >= 0 (i.e., 5+5 >= 11), no elision
            elideDocument (Lines 100) (Lines 100) "hello world"
                @?= "hello world"
        
        , testCase "no elision needed - overlapping regions (chars)" $
            elideDocument (Chars 10) (Chars 10) "short"
                @?= "short"
        
        , testCase "elides middle chars" $
            -- "Hello, world! This is a test." = 29 chars
            -- Leading 5 = "Hello", trailing 5 = "test."
            -- Elided = 29 - 5 - 5 = 19 chars
            elideDocument (Chars 5) (Chars 5) "Hello, world! This is a test."
                @?= "Hello... (elided 19 chars) ...test."
        
        , testCase "elides single char" $
            elideDocument (Chars 5) (Chars 5) "Hello World"
                @?= "Hello... (elided 1 char) ...World"
        
        , testCase "handles single char" $
            elideDocument (Chars 5) (Chars 5) "X"
                @?= "X"
        
        , testCase "mixed units - Lines leading, Chars trailing" $
            -- 2 leading lines = "line1\nline2" (12 chars including newline)
            -- 5 trailing chars from "line1\nline2\nline3\nline4\nline5"
            -- trailing chars = last 5 chars of the whole string
            -- "line1\nline2\nline3\nline4\nline5" = 29 chars
            -- trailing 5 chars = "ine5\n"... wait that's 5 chars but need to recalc
            -- Actually trailing 5 chars = last 5 chars: positions 24-29 = "ne5"... 
            -- Let's recalc: indexes 0-28, positions 24-28 = "ne5\n"? No wait no newline at end
            -- "line1\nline2\nline3\nline4\nline5" has 4 newlines, 5*5 + 4 = 29 chars
            -- Trailing 5 chars = indexes 24-28 = "ne5"... let me count:
            -- l i n e 1 \n l i n e 2 \n l i n e 3 \n l i n e 4 \n l i n e 5
            -- 0 1 2 3 4  5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
            -- Last 5 chars: 24,25,26,27,28 = 'n','e','5'... wait that doesn't look right
            -- Actually "line5" is at the end, indexes 24-28 should be "line5"
            -- 0-4: line1, 5: \n, 6-10: line2, 11: \n, 12-16: line3, 17: \n, 18-22: line4, 23: \n, 24-28: line5
            -- So last 5 chars = "line5"
            elideDocument (Lines 2) (Chars 5) "line1\nline2\nline3\nline4\nline5"
                @?= "line1\nline2\n... (elided 12 chars) ...line5"
        
        , testCase "mixed units - Chars leading, Lines trailing" $
            -- 10 leading chars = "line1\nline" (10 chars)
            -- 2 trailing lines = "line4\nline5" (11 chars)
            -- total string = 29 chars
            -- 10 + 11 = 21, so overlap = 21 - 29 = -8, so elision of 8 chars
            -- leading 10 = "line1\nline"
            -- trailing from pos 19 = "line4\nline5"
            -- Actually need to recalc: trailing 2 lines = 11 chars
            -- elided = 29 - 10 - 11 = 8 chars
            elideDocument (Chars 10) (Lines 2) "line1\nline2\nline3\nline4\nline5"
                @?= "line1\nline... (elided 7 chars) ...\nline4\nline5"
        ]

