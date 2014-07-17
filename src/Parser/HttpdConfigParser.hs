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

module Parser.HttpdConfigParser where

import Text.Parsec hiding (Line)
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)
import Control.Monad

-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------
newtype Config = Config [AbsDirective] deriving (Eq, Show)

emptyConfig :: Config
emptyConfig = Config ( [] )

extractDirectives :: Config -> [AbsDirective]
extractDirectives ( Config([]) )    = []
extractDirectives ( Config(x) )     = x

data AbsDirective   = D Directive | S SDirective deriving (Eq, Show)

data Directive      = Directive String [String] deriving (Eq, Show)

data SDirective     = SDirective SOpen Config SClose deriving (Eq, Show)
                  
data SOpen          = SOpen (Directive) deriving (Eq, Show)

data SClose         = SClose String deriving (Eq, Show)



-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-
    configp : parse configuration
    
    This is the top-level function for the overall config parser. 
-}
configp :: Parser Config
configp = Config <$> many1 directivep

    
{-
    directivep : parse a directive
-}
directivep :: Parser AbsDirective
directivep = skipMany whitespace
    *> skipMany commentp 
    *> ( try ( S <$> sectionDirectivep )
        <|> ( D <$> simpleDirectivep ) <?> "Directive" ) 
    <* skipMany commentp 
    <* skipMany whitespace
    
    
sectionDirectivep :: Parser SDirective
sectionDirectivep = SDirective
    <$> sectionOpenp   
    <*> ( try ( lookAhead ( sectionClosep >> return emptyConfig ) )
                    <|> configp <?> "SectionClose or Config" )
    <*> sectionClosep
    
    
{-
    simpleDirectivep : parse a simple directive
    
    A SimpleDirective must have a DirectiveName and can have 0 or more 
    arguments. The DirectiveName must be seperated from its arguments
    by one or more spaces and the arguments, if more than one, must 
    also be seperated from each other by one or more spaces
-}

simpleDirectivep :: Parser Directive
simpleDirectivep = fmap Directive 
     directiveNamep 
    <*  ( many $ oneOf " " ) 
    <*> ( endBy directiveArgp $ many $ oneOf " " )
   
    
sectionOpenp :: Parser SOpen
sectionOpenp = SOpen 
    <$> ( char '<' *> simpleDirectivep <* char '>' <*  skipMany newline ) 
    <?> "SectionOpen"    

    
sectionClosep :: Parser SClose
sectionClosep = SClose
    <$> ( char '<' *> char '/' *> directiveNamep <* char '>' )  
    <* skipMany whitespace 
    <?> "SectionClose"
    
    
{-
    A Directive name must start with a letter and can contain 
    any alpha-numeric characters.
-}
directiveNamep :: Parser String
directiveNamep = (:) <$> letter <*> many alphaNum    
    
    
{-
    A DirectiveArg is generic for now - it can contain any characters
    besides "<>" and " ". This will most likely be generalized per
    Directive.
-}
directiveArgp :: Parser String
directiveArgp = many1 dArgAllowed <?> "DirectiveArg"    
    


dArgAllowed :: Parser Char
dArgAllowed = try ( alphaNum ) <|> oneOf "/~.-_,\"\\^" 
    

commentp :: Parser ()
commentp = char '#' 
    *> try ( skipMany ( noneOf "\n\r" ) ) 
    *> skipMany whitespace
       
{-
    Parse whitespace - includes newlines.
-}
whitespace :: Parser ()
whitespace = space >> spaces


   
         
