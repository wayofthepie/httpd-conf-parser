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


type Config         = [Line] 

data Line           = Line Directive deriving Show

data Directive      = SectionDirective SectionOpen Config SectionClose 
                    | SimpleDirective DirectiveName [DirectiveArg] deriving Show


data SectionOpen    = SectionOpen DirectiveName [DirectiveArg] deriving Show

data SectionClose   = SectionClose DirectiveName  deriving Show
    
type DirectiveArg   = String 
    
type DirectiveName  = String 


config :: Parser Config
config = many1 line 

line :: Parser Line
line = Line <$> (directive <* newline)

directive :: Parser Directive
directive = sectionDirective <|> simpleDirective

sectionDirective :: Parser Directive
sectionDirective = SectionDirective <$> sectionOpen <*> config <*> sectionClose

simpleDirective :: Parser Directive
simpleDirective = SimpleDirective <$> directiveName <*> (whitespace *> many (directiveArg))

sectionOpen :: Parser SectionOpen
sectionOpen = SectionOpen
    <$> (char '<' *> directiveName) 
    <*> (many (directiveArg) <* char '>')

sectionClose :: Parser SectionClose
sectionClose = SectionClose
    <$> (char '<' *> char '/' *> directiveName <* char '>')
    
directiveName :: Parser DirectiveName
directiveName = (:) <$> letter <*> many alphaNum

directiveArg :: Parser DirectiveArg
directiveArg =  (:) <$> anyChar <*> many alphaNum


{-
    Low Level.
-}
num :: Parser Int
num = read <$> many digit

whitespace = space >> spaces

lit :: String -> Parser String
lit xs = string xs <* whitespace


{-
    Result of applying parser 'p' to 'input'.
-}
run :: Show a => Parser a -> String -> IO()
run p input = 
    case (parse p "" input) of
         Left err -> do {
             putStr "Parse error at "; print err
         }
         Right x -> print x
