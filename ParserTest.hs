import Data.Char

import HttpdConfigParser

import Text.Parsec
import Text.Parsec.String
import Test.HUnit
import Test.QuickCheck

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------
{-
a = parse (many configp) "" "<s>\n\n\no ddd dsads\n\n</s>\ntest test"

testDirectiveName = 
    TestCase (assertEqual "directiveName parser: positive" directiveNamep "test" "test")
    TestCase (assertEqual "directiveName parser: positive" ( run directiveNamep "test" "test2")

testDirectiveArg = 
    TestCase (assertEqual' "directiveArg parser: positive" directiveArgp " test" "test")


tests = TestList [  TestLabel "testDirectiveArg" testDirectiveArg,
                    TestLabel "testDirectiveName" testDirectiveName ]
-}                  
                    
-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

{-
    run p input : apply parser 'p' to 'input'.
-}
run :: Show a => Parser a -> String -> Maybe a
run p input = 
    case (parse p "" input) of
         Left err -> Nothing
         Right x -> Just x 
         
{-
    assertEqual' msg p i o : apply Parser p to input i and expect output o
-}
assertEqual' :: (Eq a, Show a) => String -> Parser a -> String -> a -> Assertion
assertEqual' msg p i o = 
    assertEqual msg (run p i) (Just o)