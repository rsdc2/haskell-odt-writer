module HUnitSpec where

import Test.HUnit
import AppendSpec

assertIOBoolHappy :: String -> IO Bool -> Assertion
assertIOBoolHappy msg iob = assertBool msg =<< iob

assertIOBoolSad :: String -> IO Bool -> Assertion
assertIOBoolSad msg iob = assertBool msg =<< (not <$> iob)

test1 = TestCase $ assertIOBoolHappy "failure test1" appendBoldItalicTextStyleWithSpanHappy
test2 = TestCase $ assertIOBoolSad "failure test2" appendBoldItalicTextStyleWithSpanSad
test3 = TestCase $ assertIOBoolHappy "failure test3" appendItalicParaStyleWithParaHappy
test4 = TestCase $ assertIOBoolSad "failure test4" appendItalicParaStyleWithParaSad

tests = TestList [
      TestLabel "test1" test1
    , TestLabel "test2" test2
    , TestLabel "test3" test3
    , TestLabel "test4" test4]