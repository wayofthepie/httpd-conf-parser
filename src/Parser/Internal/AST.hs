
module Parser.Internal.AST where

import Control.Monad.Reader
import Data.List (partition)
import qualified Data.Map as M 
import Parser.Internal.CST

--makeLenses ''Config
--makeLenses ''Directive

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
                                Just x  -> directiveMapper d m 
                                {- 
                                    If we have not seen this directive 
                                    yet just add it.
                                -}
                                Nothing -> M.insert (name d) (args d) m
            

directiveMapper :: Directive -> EnvMap -> EnvMap
directiveMapper d m =
    case M.lookup (name d) directiveMap of
        Just f  -> f d m
        Nothing -> m
        
                    
directiveMap :: M.Map String (Directive -> EnvMap -> EnvMap)
directiveMap =  M.fromList [            
            ("ServerRoot",  \d m -> inserter d m (\d _      -> M.insert (name d) (args d) m)),
            ("ServerAdmin", \d m -> inserter d m (\d _      -> M.insert (name d) (args d) m)), 
            ("Listen",      \d m -> inserter d m (\d pargs  -> M.insert (name d) (pargs ++ args d) m)),
            ("LoadModule",  \d m -> inserter d m (\d pargs  -> M.insert (name d) (pargs ++ args d) m))
        ]              
    where   inserter :: Directive -> EnvMap -> (Directive -> [String] -> EnvMap) -> EnvMap
            inserter d m f =    case M.lookup (name d) m of
                                    Just pargs  -> f d pargs
                                    Nothing     -> m 
           

