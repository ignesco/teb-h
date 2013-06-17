module Teb.Utils where

import System.Directory
import System.FilePath
import Teb.Consts

existenceSnip :: Bool -> String
existenceSnip exists = if exists then "to exist" else "not to exist"

findParentOf :: String -> String -> IO (Maybe String)
findParentOf dir what = do
	     	 exists <- doesDirectoryExist $ dir </> what
		 if exists then
		    return $ Just dir
		 else
		    if isDrive dir then
		       return Nothing
		    else
		       findParentOf (takeDirectory dir) what

existsInTree :: String -> String -> IO Bool
existsInTree dir what = do
	      	       	 maybeDir <- findParentOf dir what
	      	       	 return $ case maybeDir of
			 	Nothing -> False
				Just _ -> True

outputStrings :: [String] -> IO ()
outputStrings = mapM_ putStrLn
