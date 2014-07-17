module Apache.Directive where

import qualified Data.Map.Strict as M


verifyLoadModule :: [String] -> Bool
verifyLoadModule (x:xs) = True


initDirectiveLookup :: M.Map String ([String] -> Bool)
initDirectiveLookup = 
    M.fromList [
        ("LoadModule", verifyLoadModule)
    ]
    

