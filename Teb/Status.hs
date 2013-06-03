module Teb.Status where

import System.FilePath ((</>))
import Teb.Consts
import Teb.Git (gitStatus)
import Teb.ManifestLocator
import Teb.ProjectDefinition
import Teb.TebUtils
import Teb.Types
import Teb.Utils

status :: CurrentWorkingDirectory -> Arguments -> IO ()
status cwd args = do
           checkDotTebDirExistence cwd True
       	   maybeTebParentDir <- getTebProjectBase cwd
	   case maybeTebParentDir of
	     Just tebParentDir -> do
		manifestFile <- getManifestLocation tebParentDir
		projectList <- loadTebProjectListFromManifest $ tebParentDir </> coreDirectory </> manifestFile
		mapM_ (singleProjectStatus tebParentDir) (filter noDisabledProjects projectList)
	   	return ()
	     Nothing -> do
	   	putStrLn "Opps"
		return ()

noDisabledProjects :: TebProject -> Bool
noDisabledProjects project = if projectStatus project == Disabled then False else True

singleProjectStatus :: CurrentWorkingDirectory -> TebProject -> IO ()
singleProjectStatus baseDir project = do
	     let projectDir = workingdirectory project
	     let projectBase = baseDir </> projectDir
	     putStrLn.unwords $ ["[TEB INFO]", projectDir, "(", (show.projectStatus) project, ")"]
	     gitStatus projectBase >>= outputStrings
	     return ()
