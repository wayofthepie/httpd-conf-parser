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


-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------
class Directive a where 
    directiveName       :: a -> String
    directiveArgs       :: a -> [String]
    hasNestedConfig     :: a -> Bool
    nestedConfig        :: a -> Config

    
    
-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------
newtype Config      = Config ( [Line] )  deriving (Eq, Show)

emptyConfig :: Config
emptyConfig = Config ( [] )



-------------------------------------------------------------------------------
-- Line
-------------------------------------------------------------------------------
data Line           = Section SectionDirective 
                    | Simple SimpleDirective deriving (Eq, Show)


                    
-------------------------------------------------------------------------------
-- SectionDirective
-------------------------------------------------------------------------------
data SectionDirective   
        = SectionDirective SectionOpen Config SectionClose deriving (Eq, Show)

{-
    hasCorrectTags : whether given section directive's open/close tags match.
-}
hasCorrectTags :: SectionDirective -> Bool
hasCorrectTags (SectionDirective (SectionOpen sd) _ (SectionClose sc)) 
        | directiveName sd ==  sc  = True
        | otherwise                = False


instance Directive SectionDirective where
    
    directiveName 
        (SectionDirective (SectionOpen (SimpleDirective x _)) _ _)  = x
    
    directiveArgs 
        (SectionDirective (SectionOpen (SimpleDirective _ x)) _ _)  = x
    
    hasNestedConfig _                                               = True
    
    nestedConfig (SectionDirective _ x _ )                          = x

   
-------------------------------------------------------------------------------
-- SimpleDirective
-------------------------------------------------------------------------------
data SimpleDirective    = SimpleDirective String [String] deriving (Eq, Show)

instance Directive SimpleDirective where
    directiveName ( SimpleDirective ( x ) _)    = x
    directiveArgs ( SimpleDirective _ ( x ) )   = x
    hasNestedConfig _                           = False
    nestedConfig _                              = emptyConfig

    

-------------------------------------------------------------------------------
-- SectionOpen
-------------------------------------------------------------------------------
data SectionOpen    = SectionOpen SimpleDirective deriving (Eq, Show)


-------------------------------------------------------------------------------
-- SectionClose
-------------------------------------------------------------------------------
data SectionClose   = SectionClose String deriving (Eq, Show)



-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-
    configp : parse configuration
    
    This is the top-level function for the overall config parser. 
-}
configp :: Parser Config
configp = fmap Config $ many1 directivep
    
    
{-
    directivep : parse a directive
-}
directivep :: Parser Line
directivep = skipMany whitespace 
    *> skipMany commentp 
    *> ( try ( Section <$> sectionDirectivep )
        <|> ( Simple <$> simpleDirectivep ) <?> "Directive" ) 
    <* skipMany commentp 
    <* skipMany whitespace
    

    
sectionDirectivep :: Parser SectionDirective
sectionDirectivep = do
    so      <-  sectionOpenp   
    next    <-  try ( lookAhead ( sectionClosep >> return emptyConfig ) )
                    <|> configp <?> "SectionClose or Config"
    sc      <-  sectionClosep
    return $ SectionDirective so next sc

    
{-
    simpleDirectivep : parse a simple directive
    
    A SimpleDirective must have a DirectiveName and can have 0 or more 
    arguments. The DirectiveName must be seperated from its arguments
    by one or more spaces and the arguments, if more than one, must 
    also be seperated from each other by one or more spaces
-}
simpleDirectivep :: Parser SimpleDirective
simpleDirectivep = do     
    dname <- directiveNamep  
    many $ oneOf " " 
    dargs <- endBy directiveArgp $ many $ oneOf " "
    return $ SimpleDirective dname dargs
 
    
sectionOpenp :: Parser SectionOpen
sectionOpenp = SectionOpen 
    <$> (char '<' *> simpleDirectivep <*  char '>' <*  skipMany newline)    
 

    
sectionClosep :: Parser SectionClose
sectionClosep = SectionClose
    <$> (char '<' *> char '/' *> directiveNamep <* char '>')  <* skipMany whitespace
    <?> "SectionClose"
    
    
{-
    A Directive name must start with a letter and can contain 
    any alpha-numeric characters.
-}
directiveNamep :: Parser String
directiveNamep = do    
    name <- (:) <$> letter <*> many alphaNum    
    return $ name


{-
    A DirectiveArg is generic for now - it can contain any characters
    besides "<>" and " ". This will most likely be generalized per
    Directive.
-}
directiveArgp :: Parser String
directiveArgp = do    
    arg <- many1 dArgAllowed <?> "DirectiveArg"    
    return $ arg


dArgAllowed :: Parser Char
dArgAllowed = try ( alphaNum ) <|> oneOf "/~.-_,\"\\^" 
    

commentp :: Parser ()
commentp = do            
    char '#' *> try (skipMany (noneOf "\n\r"))
    skipMany whitespace
    return ()
    
    
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
    Parse whitespace - includes newlines.
-}
whitespace :: Parser ()
whitespace = space >> spaces


   
         