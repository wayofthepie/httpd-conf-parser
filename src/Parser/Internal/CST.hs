
module Parser.Internal.CST where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)
import Control.Lens hiding (noneOf)
import Control.Monad
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
directivep = skipMany ( commentp <|> whitespace)
    *> ( try ( sectionDirectivep )
        <|> simpleDirectivep <?> "Directive" )
    <* skipMany ( commentp <|> whitespace)


{-
    sectionDirectivep : parser for section directives
-}
sectionDirectivep :: Parser Directive
sectionDirectivep = sectionDirective
    <$> sectionOpenp
    <*  skipMany whitespace
    <*  skipMany commentp
    <*> ( try ( sectionClosep >> return [] )
        <|> many1 directivep
                    <?> "SectionClose or Config" )
    where   sectionDirective :: Directive -> [Directive] ->  Directive
            sectionDirective (Directive n as _) nds = Directive n as nds



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
    <*> ( endBy ( ( ( lookAhead ( char '\"' ) >> qdirectiveArgp ) <|>  directiveArgp) ) $ oneOf " ")
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
    <?> "SectionClose"


{-
    directiveNamep : parser for directive names
-}
directiveNamep :: Parser String
directiveNamep = (:) <$> letter <*> many alphaNum


-- | qdirectiveArgp : parser for quoted directive arguments
qdirectiveArgp :: Parser String
qdirectiveArgp = between ( char '\"' ) ( char '\"' ) allowedChars
    <?> "Expected a quoted directive argument."
    where allowedChars = many ( dArgAllowed <|> escapedp <|> oneOf " " )


-- |  escapedp : parser for escaped characters
escapedp :: Parser Char
escapedp = char '\\' >> choice charMap
    where charMap = foldl (\l (c,r) -> (char c *> return r) : l)
                        [] escapedCharMappings


-- | transformChar : discards 'c' and returns a parser for its replacement
-- character 'replacement'
transformChar :: Char -> Char -> Parser Char
transformChar c replacement = char c *> return replacement


-- | escapedCharMappings : escaped characters and their replacements
--
-- The first char in a tuple is the escaped character, teh second is what it
-- must be replaced with when returning its value. Note that when parsing
-- escaped characters are those preceeded with '\' e.g. "\n".
escapedCharMappings :: [ ( Char, Char ) ]
escapedCharMappings = [('\\', '\\'), ('\"', '\"'), ('n', '\n'), ('t', '\t')]


{-
    directiveArgp : parser for directive arguments

    This parser parsers Directive arguments quoted and unquoted. For the former
    the parsed string will retain its quotes, escaped.
-}
directiveArgp :: Parser String
directiveArgp = many dArgAllowed  <?> "DirectiveArg"
--    ( lookAhead ( char '\"' ) >>
--        ( many ( escapedp <|> oneOf " " <|> dArgAllowed ) ) )

{-
    dArgAllowed : the allowed characters in a directive argument
-}
dArgAllowed :: Parser Char
dArgAllowed = alphaNum <|> oneOf dArgAllowedSymbols


{-
    dArgAllowedSymbols : list of allowed unescaped characters
-}
dArgAllowedSymbols :: [Char]
dArgAllowedSymbols = ['/', '~', '@', '.', '-', '_', ',', '^', ':', '*', '%', '{', '}']


{-
    commentp : parser for comments
-}
commentp :: Parser ()
commentp = char '#'
    *> try ( skipMany ( noneOf "\n\r" ) )


{-
    whitespace : parser for whitespace
-}
whitespace :: Parser ()
whitespace = space >> spaces



