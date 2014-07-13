module ParserQTest (parserTests) where

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
-- QuickCheck Tests
-------------------------------------------------------------------------------
newtype AllowedChar     = AllowedChar   { unwrapChar :: Char } deriving Show
newtype AllowedString   = AllowedString { unwrapString :: String } deriving Show


genAllowedChar :: Gen Char
genAllowedChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "/~.-_,\"\\^"

genAllowedString :: Gen String
genAllowedString = listOf1 genAllowedChar


instance Arbitrary AllowedChar where
    arbitrary = AllowedChar <$> genAllowedChar
    
instance Arbitrary AllowedString where
    arbitrary = AllowedString <$> genAllowedString

    
{-
    Properties to test.
-}
propCharsAllowed :: AllowedChar -> Bool
propCharsAllowed (AllowedChar x) = run dArgAllowed [x] == Just x

propStringsAllowed :: AllowedString -> Bool
propStringsAllowed (AllowedString x) = run directiveArgp x == Just x


simpleDirectiveQTest :: [Test]
simpleDirectiveQTest =  [ 
    testGroup "Directive Allowed Arguments" [ 
        testProperty "chars allowed"  propCharsAllowed, 
        testProperty "strings allowed" propStringsAllowed ] ]

parserTests :: [Test]
parserTests = simpleDirectiveQTest

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

