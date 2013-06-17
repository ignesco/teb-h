module Teb.Sync where

import System.FilePath ((</>))

import Teb.Consts (coreDirectory)
import Teb.Git (gitRebase)
import Teb.ManifestLocator (getManifestLocation)
import Teb.ProjectDefinition (ProjectStatus(..), TebProject(..), loadTebProjectListFromManifest)
import Teb.Types
import Teb.TebUtils (getTebProjectBase, onlyProjectsInState, checkDotTebDirExistence)
import Teb.Utils (outputStrings)

sync :: CurrentWorkingDirectory -> Arguments -> IO ()
sync cwd args = do
     checkDotTebDirExistence cwd True
     maybeTebParentDir <- getTebProjectBase cwd
     case maybeTebParentDir of
     	  Just tebParentDir -> do
	       manifestFile <- getManifestLocation tebParentDir
	       projectList' <- loadTebProjectListFromManifest $ tebParentDir </> coreDirectory </> manifestFile
	       let projectList = filter (onlyProjectsInState Inactive) projectList'
	       mapM_ (singleProjectSync tebParentDir) projectList
	       return ()
	  Nothing -> do
	       putStrLn "Opps"
	       return ()

singleProjectSync :: CurrentWorkingDirectory -> TebProject -> IO ()
singleProjectSync baseDir project = do
	     let projectDir = workingdirectory project
	     let projectBase = baseDir </> projectDir
	     let remote = "origin"
	     let projectBranch = branch project
	     putStrLn $ unwords [ "[TEB INFO] [rebase " ++ remote ++ "/" ++ branch project ++ "]", workingdirectory project, "(", (show.projectStatus) project, ")" ]
	     gitRebase projectBase remote projectBranch  >>= outputStrings
	     putStrLn $ unwords [ "[TEB INFO]", workingdirectory project, "rebased." ]
	     putStrLn ""

