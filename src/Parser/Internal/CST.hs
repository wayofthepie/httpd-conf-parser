
module Parser.Internal.CST where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)
import Control.Lens hiding (noneOf)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
data CST = CST { _directives :: [Directive] } deriving (Eq, Show)

directives :: CST -> [Directive]
directives = _directives 

data Directive =    Directive { 
                        _name   :: String, 
                        _args   :: [String], 
                        _nds    :: [Directive] 
                    } 
                    | EmptyDirective deriving (Eq, Show)

name :: Directive -> String
name = _name 

args :: Directive -> [String]
args = _args

nested :: Directive -> [Directive]
nested = _nds

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-
    configp : parser for apache configuration

    This is the top-level function for the overall config parser.
-}
configp :: Parser CST
configp = CST <$> many1 directivep


{-
    directivep : high level parser for directives
-}
directivep :: Parser Directive
directivep = skipMany whitespace
    *> skipMany commentp
    *> ( try (  sectionDirectivep )
        <|> ( simpleDirectivep ) <?> "Directive" )
    <* skipMany commentp
    <* skipMany whitespace


{-
    sectionDirectivep : parser for section directives
-}
sectionDirectivep :: Parser Directive
sectionDirectivep = sectionDirective
    <$> sectionOpenp
    <*> ( try ( lookAhead ( sectionClosep >> return [] ) )
                    <|> many1 directivep <?> "SectionClose or Config" )
    <*> sectionClosep
    where   sectionDirective :: Directive -> [Directive] -> String -> Directive
            sectionDirective (Directive n as _) nds _ = Directive n as nds


{-
    simpleDirectivep : parser for simple directive

    A SimpleDirective must have a DirectiveName and can have 0 or more
    arguments. the DirectiveName must be seperated from its arguments
    by one or more spaces and the arguments, if more than one, must
    also be seperated from each other by one or more spaces.
-}
simpleDirectivep :: Parser Directive
simpleDirectivep = simpleDirective
    <$> directiveNamep
    <*  ( many $ oneOf " " )
    <*> ( endBy directiveArgp $ many $ oneOf " " )
    where   simpleDirective :: String -> [String] -> Directive
            simpleDirective n xs = Directive n xs []

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



