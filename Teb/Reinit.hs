module Teb.Reinit where

import Control.Monad (filterM)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Teb.Consts (coreDirectory)
import Teb.Git (gitClone, gitFetch, gitRebase)
import Teb.ManifestLocator (getManifestLocation, getManifestTebDirectory)
import Teb.ProjectDefinition (TebProject (..), loadTebProjectListFromManifest)
import Teb.TebUtils (getTebProjectBase, onlyProjectsInState, cloneProject, checkDotTebDirExistence)
import Teb.Types
import Teb.Utils (outputStrings)

reinit :: CurrentWorkingDirectory -> Arguments -> IO ()
reinit cwd args = do
	   checkDotTebDirExistence cwd True
	   maybeTebParentDir <- getTebProjectBase cwd
	   case maybeTebParentDir of
	     Just tebParentDir -> do
	        putStrLn $ "[TEB INFO] reinit"
	     	manifestDir <- getManifestTebDirectory tebParentDir
	     	let manifestCwd = tebParentDir </> coreDirectory </> manifestDir
		gitFetch manifestCwd >>= outputStrings
		gitRebase manifestCwd "origin" "master"
		manifestFile <- getManifestLocation tebParentDir
		projectList <- loadTebProjectListFromManifest $ tebParentDir </> coreDirectory </> manifestFile
		mapM_ (possiblyCloneProject cwd) projectList
	   	return ()
	     Nothing -> do
	   	putStrLn "Opps"
		return ()

possiblyCloneProject :: CurrentWorkingDirectory -> TebProject -> IO ()
possiblyCloneProject cwd project = do
		     	 	     exists <- doesDirectoryExist (cwd </> workingdirectory project)
				     if exists
		     	 	       then putStrLn $ "Project already present : " ++ projid project
				       else cloneProject cwd project



