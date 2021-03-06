module Teb.TebUtils where

import System.FilePath ((</>))
import Teb.Consts
import Teb.Git (gitClone, gitCheckout)
import Teb.ProjectDefinition (ProjectStatus(..), TebProject(..))
import Teb.Types
import Teb.Utils

getTebProjectBase :: String -> IO (Maybe String)
getTebProjectBase dir = findParentOf dir coreDirectory

checkDotTebDirExistence :: String -> Bool -> IO Bool
checkDotTebDirExistence dir neededExistence = do
			exists <- existsInTree dir coreDirectory
			if exists == neededExistence then
			   return exists
			else
			   error $ unwords ["The", coreDirectory, "fails the existence test, was expecting it", existenceSnip neededExistence, "in", dir]

onlyProjectsInState :: ProjectStatus -> TebProject -> Bool
onlyProjectsInState state project = projectStatus project == state

cloneProject :: CurrentWorkingDirectory -> TebProject -> IO ()
cloneProject cwd project = do
	     let projectDir = workingdirectory project
	     putStrLn.unwords $ ["[TEB INFO]", projectDir, "(", (show.projectStatus) project, ")"]
	     gitClone cwd (repolocation project) (Just projectDir) >>= outputStrings
	     putStrLn.unwords $ ["[TEB INFO]", projectDir, "cloned."]
	     gitCheckout (cwd </> projectDir) (branch project) >>= outputStrings
	     return ()
