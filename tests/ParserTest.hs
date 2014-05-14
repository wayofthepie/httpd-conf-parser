import Data.Char

import Parser.HttpdConfigParser
import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import Text.Parsec.String
import Test.HUnit hiding (Test)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.Test hiding (test)
import Test.Framework.Providers.QuickCheck2
import Test.Framework
import Test.Framework.Providers.API
-------------------------------------------------------------------------------
-- Unit Tests
-------------------------------------------------------------------------------


{-
testDirectiveArg = 
    TestCase (assertEqual' "directiveArg parser: positive" directiveArgp " test" "test")
    -}

commentpTests = test [ 
    "single comment single line" ~: 
        Just () ~=?  run commentp "#test",
            
    "multiple comments multiple lines" ~: 
        Just [(),()] ~=? run (many1 commentp) "#test\n#test\n" ] 


-- dArgAllowed tests
dArgAllowedTests = test [
    "all allowed symbols" ~: 
        Just "/~.-_,\"\\^" ~=? run (many dArgAllowed) "/~.-_,\"\\^" ]
 
 
 
-------------------------------------------------------------------------------
-- QuickCheck Tests
-------------------------------------------------------------------------------
newtype AllowedString = AllowedString { unwrapChar :: String } deriving Show

genAllowedChar :: Gen Char
genAllowedChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "/~.-_,\"\\^"

genAllowedString :: Gen String
genAllowedString = listOf genAllowedChar


instance Arbitrary AllowedString where
    arbitrary = AllowedString <$> genAllowedString

    
propCharsAllowed :: AllowedString -> Bool
propCharsAllowed (AllowedString x) = run (many dArgAllowed) x == Just x

dArgAllowedQTest :: [Test]
dArgAllowedQTest =  [ testGroup "a" [ testProperty ""  propCharsAllowed ] ]

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
