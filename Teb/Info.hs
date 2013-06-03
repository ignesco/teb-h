module Teb.Info where

import System.FilePath ((</>))
import Teb.Consts (coreDirectory)
import Teb.ManifestLocator (getManifestLocation)
import Teb.ProjectDefinition (TebProject(..), loadTebProjectListFromManifest)
import Teb.TebUtils (getTebProjectBase)
import Teb.Types
import Teb.Utils (checkDotTebDirExistence)

info :: CurrentWorkingDirectory -> Arguments -> IO ()
info cwd args = do
     checkDotTebDirExistence cwd True
     maybeTebParentDir <- getTebProjectBase cwd
     case maybeTebParentDir of
       Just tebParentDir -> do
     	  putStrLn $ "[TEB INFO] Base Directory : " ++ tebParentDir
	  manifestFile <- getManifestLocation tebParentDir
	  projectList <- loadTebProjectListFromManifest $ tebParentDir </> coreDirectory </> manifestFile
	  mapM_ (singleProjectInfo tebParentDir) projectList
	  return ()
       Nothing -> do
     	  putStrLn "Opps"
	  return ()

singleProjectInfo :: CurrentWorkingDirectory -> TebProject -> IO ()
singleProjectInfo baseDir project = do
		  putStrLn $ "Project Id : " ++ projid project
		  putStr "Project Status : "
		  putStrLn.show $ projectStatus project

		  putStrLn $ "Remote : " ++ repolocation project
		  putStrLn $ "Branch : " ++ branch project
		  putStrLn $ "Working Directory : " ++ workingdirectory project
		  putStrLn ""
