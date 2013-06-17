module Teb.Fetch where

import System.FilePath ((</>))
import Teb.Consts (coreDirectory)
import Teb.Git (gitFetch)
import Teb.ManifestLocator (getManifestLocation)
import Teb.ProjectDefinition (ProjectStatus(..), TebProject(..), loadTebProjectListFromManifest)
import Teb.TebUtils (getTebProjectBase, onlyProjectsInState, checkDotTebDirExistence)
import Teb.Types
import Teb.Utils (outputStrings)

fetch :: CurrentWorkingDirectory -> Arguments -> IO ()
fetch cwd args =
      	 let
	   decodeProjectStatus (_:"active":_) = Active
      	   decodeProjectStatus _ = Inactive
	   requiredProjectStatus = decodeProjectStatus args
      	 in do
	   checkDotTebDirExistence cwd True
	   maybeTebParentDir <- getTebProjectBase cwd
	   case maybeTebParentDir of
	     Just tebParentDir -> do
		manifestFile <- getManifestLocation tebParentDir
		projectList' <- loadTebProjectListFromManifest $ tebParentDir </> coreDirectory </> manifestFile
		let projectList = filter (onlyProjectsInState requiredProjectStatus) projectList'
		mapM_ (singleProjectFetch tebParentDir) projectList
		putStrLn $ unwords [ "[TEB INFO] [fetch] updated", (show.length) projectList, show requiredProjectStatus, if length projectList==1 then "project" else "projects" ]
	   	return ()
	     Nothing -> do
	   	putStrLn "Opps"
		return ()

singleProjectFetch :: CurrentWorkingDirectory -> TebProject -> IO ()
singleProjectFetch baseDir project = do
	     let projectDir = workingdirectory project
	     let projectBase = baseDir </> projectDir
	     putStrLn $ unwords [ "[TEB INFO] [fetch]", workingdirectory project,  "(", (show.projectStatus) project, ")" ]
	     gitFetch projectBase >>= outputStrings
	     putStrLn ""
	     return ()
