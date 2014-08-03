
module Parser.Internal.AST where

import Control.Monad.Reader
import Data.List (partition)
import qualified Data.Map as M 
import Parser.Internal.CST


type EnvMap = M.Map String [String]

buildGlobalEnv :: [Directive] -> EnvMap 
buildGlobalEnv ds = foldl mapper M.empty $ filter (\d -> null $ nested d) ds 
    where   mapper :: M.Map String [String] -> Directive -> M.Map String [String]
            mapper m d =    case M.lookup (name d) m of 
                                {-
                                    If we have seen this directive before 
                                    lookup how we should deal with this second
                                    declaration.  
                                -}
                                Just pargs  -> directiveMapper d m pargs
                                {- 
                                    If we have not seen this directive 
                                    yet just add it.
                                -}
                                Nothing     -> M.insert (name d) (args d) m
            

directiveMapper :: Directive -> EnvMap -> [String] -> EnvMap
directiveMapper d m pargs =
    case M.lookup (name d) directiveMap of
        Just f  -> f d m pargs
        Nothing -> m
        
                    
directiveMap :: M.Map String (Directive -> EnvMap -> [String] -> EnvMap)
directiveMap =  M.fromList [            
            ("ServerRoot",  \d m _      -> M.insert (name d) (args d) m),
            ("ServerAdmin", \d m _      -> M.insert (name d) (args d) m), 
            ("Listen",      \d m pargs  -> M.insert (name d) (pargs ++ args d) m),
            ("LoadModule",  \d m pargs  -> M.insert (name d) (pargs ++ args d) m)
        ]              
          

