module Apache.Directive where

import System.Directory (doesFileExist)
import Control.Monad
import Data.ByteString as B
import Data.ByteString.Char8 as BC


{-
    Data type for the "LoadModule" directive
-}
data Module m f = Module m f deriving (Eq, Show)


{-
    doesModuleLibraryExist m f :    
    whether the so for the module exists at the path ServerRoot/f
-}
doesModuleLibExist :: Module String FilePath -> IO Bool
doesModuleLibExist (Module _ f) = doesFileExist f


{-
    verify m : check the module so exists and the module name is correct
    
    This is a bit of a hack, as such its not in use currently. I need to 
    find a way of verifying the name of the module against the name defined
    in the so. This name is set at compile time, following is the deifinition
    for mod_ssl :
        
        module AP_MODULE_DECLARE_DATA ssl_module = {
        ...
-}
{- verify :: Module String FilePath -> IO Bool
verify (Module m f) = 
    doesFileExist f >>= (\x ->
        if x then (liftM isModNameCorrect $ B.readFile f)
        else return x)
    where isModNameCorrect = B.isInfixOf $ BC.pack $ "." ++ m ++ "."-}


