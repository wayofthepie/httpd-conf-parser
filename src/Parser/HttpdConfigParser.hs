
module Parser.HttpdConfigParser where

import Text.Parsec hiding (Line)
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)
import Control.Monad

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
newtype Config = Config [AbsDirective] deriving (Eq, Show)

emptyConfig :: Config
emptyConfig = Config ([])


data AbsDirective   = D Directive | S SDirective deriving (Eq, Show)

data Directive      = Directive { name :: String, args :: [String] }  deriving (Eq, Show)

data SDirective     = SDirective Directive Config String deriving (Eq, Show)
                  

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-
    configp : parser for apache configuration
    
    This is the top-level function for the overall config parser. 
-}
configp :: Parser Config
configp = Config <$> many1 directivep

    
{-
    directivep : high level parser for directives
-}
directivep :: Parser AbsDirective
directivep = skipMany whitespace
    *> skipMany commentp 
    *> ( try ( S <$> sectionDirectivep )
        <|> ( D <$> simpleDirectivep ) <?> "Directive" ) 
    <* skipMany commentp 
    <* skipMany whitespace
    
    
{-
    sectionDirectivep : parser for section directives
-}    
sectionDirectivep :: Parser SDirective
sectionDirectivep = SDirective
    <$> sectionOpenp   
    <*> ( try ( lookAhead ( sectionClosep >> return emptyConfig ) )
                    <|> configp <?> "SectionClose or Config" )
    <*> sectionClosep
    
    
{-
    simpleDirectivep : parser for simple directive
    
    A SimpleDirective must have a DirectiveName and can have 0 or more 
    arguments. the DirectiveName must be seperated from its arguments
    by one or more spaces and the arguments, if more than one, must 
    also be seperated from each other by one or more spaces.
-}
simpleDirectivep :: Parser Directive
simpleDirectivep = Directive 
    <$> directiveNamep 
    <*  ( many $ oneOf " " ) 
    <*> ( endBy directiveArgp $ many $ oneOf " " )

   
{-
    sectionOpenp : parser for opening sections of section directives
-}    
sectionOpenp :: Parser Directive
sectionOpenp = char '<' *> simpleDirectivep <* char '>' <*  skipMany newline 
    <?> "SectionOpen"    

    
{-
    sectionClosep : parser for closing sections of section directives
-}
sectionClosep :: Parser String
sectionClosep = ( char '<' *> char '/' *> directiveNamep <* char '>' )  
    <* skipMany whitespace 
    <?> "SectionClose"
    
    
{-
    directiveNamep : parser for directive names
-}
directiveNamep :: Parser String
directiveNamep = (:) <$> letter <*> many alphaNum    
    
    
{-
    directiveArgp : parser for directive arguments
-}
directiveArgp :: Parser String
directiveArgp = many1 dArgAllowed <?> "DirectiveArg"    
    

{-
    dArgAllowed : the allowed characters in a directive argument
-}
dArgAllowed :: Parser Char
dArgAllowed = try ( alphaNum ) <|> oneOf "/~.-_,\"\\^" 
    

{-
    commentp : parser for comments
-}
commentp :: Parser ()
commentp = char '#' 
    *> try ( skipMany ( noneOf "\n\r" ) ) 
    *> skipMany whitespace
    
       
{-
    whitespace : parser for whitespace
-}
whitespace :: Parser ()
whitespace = space >> spaces

