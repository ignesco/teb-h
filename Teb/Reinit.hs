module Teb.Reinit where

import System.FilePath ((</>))
import Teb.Consts (coreDirectory)
import Teb.Git (gitClone)
import Teb.ManifestLocator (getManifestLocation)
import Teb.ProjectDefinition (loadTebProjectListFromManifest)
import Teb.TebUtils (getTebProjectBase, onlyProjectsInState, cloneProject)
import Teb.Types
import Teb.Utils (outputStrings, checkDotTebDirExistence)

reinit :: CurrentWorkingDirectory -> Arguments -> IO ()
reinit cwd args = do
	   checkDotTebDirExistence cwd True
	   maybeTebParentDir <- getTebProjectBase cwd
	   case maybeTebParentDir of
	     Just tebParentDir -> do
		manifestFile <- getManifestLocation tebParentDir
		projectList <- loadTebProjectListFromManifest $ tebParentDir </> coreDirectory </> manifestFile
		mapM_ (cloneProject cwd) projectList
	   	return ()
	     Nothing -> do
	   	putStrLn "Opps"
		return ()

