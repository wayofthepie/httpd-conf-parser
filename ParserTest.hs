import HttpdConfigParser
import Text.Parsec
import Text.Parsec.String
import Test.HUnit

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

testDirectiveName = 
    TestCase (assertEqual' "directiveName parser: positive" directiveName "test" "test")

testDirectiveArg = 
    TestCase (assertEqual' "directiveArg parser: positive" directiveArg " test" "test")
        


tests = TestList [  TestLabel "testDirectiveArg" testDirectiveArg,
                    TestLabel "testDirectiveName" testDirectiveName ]
                    
                    
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