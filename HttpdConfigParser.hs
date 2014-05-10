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

emptyConfig :: Config
emptyConfig = Config ( [] )


data Directive      = Section SectionDirective 
                    | Simple SimpleDirective 
                    | EmptyDirective deriving Show

emptyDirective :: Directive
emptyDirective = EmptyDirective

data SectionDirective   = SectionDirective SectionOpen Directive SectionClose deriving Show

data SimpleDirective    = SimpleDirective DirectiveName [DirectiveArg] deriving Show

data SectionOpen    = SectionOpen DirectiveName [DirectiveArg] deriving Show

data SectionClose   = SectionClose DirectiveName  deriving Show
    
data DirectiveArg   = DirectiveArg String deriving Show
    
data DirectiveName  = DirectiveName String deriving Show

commentp :: Parser ()
commentp = do        
    skipMany whitespace
    char '#' *> skipMany (noneOf "\n\r")
    return ()
    

configp :: Parser Config
configp = fmap Config $ many1 directivep
    
    
directivep :: Parser Directive
directivep = do
    skipMany newline
    skipMany commentp 
    d <- try ( sectionDirectivep ) <|> simpleDirectivep <?> "Directive"    
    skipMany commentp 
    skipMany newline
    return d

    
sectionDirectivep :: Parser Directive
sectionDirectivep = do
    so      <-  sectionOpenp   
    next    <-  try ( lookAhead ( sectionClosep >> return emptyDirective ) )
                    <|> directivep 
    sc      <-  sectionClosep
    return $ Section $ SectionDirective so next sc

    
simpleDirectivep :: Parser Directive
simpleDirectivep = do     
    dname <- directiveNamep 
    dargs <- many directiveArgp 
    return $ Simple $ SimpleDirective dname dargs
-- 
    
sectionOpenp :: Parser SectionOpen
sectionOpenp = do
    char '<'
    Simple ( SimpleDirective d dargs )<- simpleDirectivep
    char '>'
    skipMany newline
    return $ SectionOpen d dargs
 

    
sectionClosep :: Parser SectionClose
sectionClosep = SectionClose
    <$> (char '<' *> char '/' *> directiveNamep <* char '>')  <* skipMany newline
    <?> "SectionClose"
    
    
{-
    A Directive name must start with a letter and can contain 
    any alpha-numeric characters.
-}
directiveNamep :: Parser DirectiveName
directiveNamep = do
    skipMany whitespace
    name <- (:) <$> letter <*> many alphaNum
    skipMany whitespace
    return $ DirectiveName ( name )


{-
    A DirectiveArg is generic for now - it can contain any characters
    besides "<>" and " ". This will most likely be generalized per
    Directive.
-}
directiveArgp :: Parser DirectiveArg
directiveArgp = do
    skipMany whitespace
    arg <- many1 alphaNum <?> "DirectiveArg"
    skipMany whitespace
    return $ DirectiveArg arg

    
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

         