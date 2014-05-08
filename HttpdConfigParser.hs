{-
    A parser for Apache 2.2 configuration files.
    
    Following is what the grammar for an apache httpd.conf
    (and other config files) should be (this is taken from an old book on 
    configuring apache 2.0):
    
    config              ::= directive*
    directive           ::= section-directive | simple-directive
    section-directive   ::= section-open configuration section-closed
    section-open        ::= "<" directive-name directive-argument* ">"
    section-close       ::= "</" directive-name ">"
    simple-directive    ::= directive-name directive-argument*
    
    directive-name      ::= ...
        (see http://httpd.apache.org/docs/2.2/mod/directives.html for a list
            of possible directives and their arguments)
            
    directive-arg       ::= ...
        (see http://httpd.apache.org/docs/2.2/mod/directives.html for a list
            of possible directives and their arguments)
            
    This is a (massive) work in progress.
-}
module HttpdConfigParser where

import Text.Parsec hiding (Line)
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)


newtype Config      = Config ( [Directive] )  deriving Show

data Directive      = SectionDirective SectionOpen Config SectionClose 
                    | SimpleDirective DirectiveName [DirectiveArg] deriving Show

data SectionOpen    = SectionOpen DirectiveName [DirectiveArg] deriving Show

data SectionClose   = SectionClose DirectiveName  deriving Show
    
type DirectiveArg   = String 
    
type DirectiveName  = String 


emptyConfig :: Config
emptyConfig = Config ( [] )


config :: Parser Config
config = fmap Config $ many1 directive
    
    
directive :: Parser Directive
directive = try (sectionDirective) <|> simpleDirective <?> "Directive"


sectionDirective :: Parser Directive
sectionDirective = do
    so      <-  sectionOpen <* newline    
    next    <-  try $ lookAhead (sectionClose >> return emptyConfig) 
                    <|> Config <$> many1 directive <* newline                    
    sc      <-  sectionClose
    return $ SectionDirective so next sc

    
simpleDirective :: Parser Directive
simpleDirective = SimpleDirective 
    <$> directiveName 
    <*> many directiveArg
    <?> "SimpleDirective"

    
sectionOpen :: Parser SectionOpen
sectionOpen = SectionOpen
    <$> (char '<' *> directiveName) 
    <*> (many directiveArg) <* char '>'
    <?> "SectionOpen"

    
sectionClose :: Parser SectionClose
sectionClose = SectionClose
    <$> (char '<' *> char '/' *> directiveName <* char '>') 
    <?> "SectionClose"
    
    
{-
    A Directive name must start with a letter and can contain 
    any alpha-numeric characters.
-}
directiveName :: Parser DirectiveName
directiveName = (:) 
    <$> letter 
    <*> many alphaNum 
    <?> "DirectiveName"


{-
    A DirectiveArg is generic for now - it can contain any characters
    besides "<>" and " ". This will most likely be generalized per
    Directive.
-}
directiveArg :: Parser DirectiveArg
directiveArg = do
    whitespace
    arg <- many1 (try $ noneOf "< >") <?> "DirectiveArg"
    return arg

{-
    Parse an integer.
-}
num :: Parser Int
num = read <$> many digit

{-
    Parse a string literal.
-}
lit :: String -> Parser String
lit xs = string xs <* whitespace

{-
    Parse whitespace.
-}
whitespace = space >> spaces
