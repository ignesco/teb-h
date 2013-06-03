module Teb.Init where

import System.Directory (createDirectory, getDirectoryContents, doesFileExist)
import System.FilePath ((</>))
import Teb.Consts (manifestFilename, coreDirectory, manifestLocationFilename)
import Teb.Error (reportError)
import Teb.Git (gitClone, gitCheckout)
import Teb.ProjectDefinition
import Teb.Types
import Teb.TebUtils (cloneProject)
import Teb.Utils

getInitArgs :: [String] -> Maybe (RemoteLocator, TebManifestFile)
getInitArgs (_:rloc:manifest:_) = Just (rloc, manifest)
getInitArgs (_:rloc:_) = Just (rloc, manifestFilename)
getInitArgs _ = Nothing

initRepoFrom :: String -> RemoteLocator -> TebManifestFile -> IO TebManifestFile
initRepoFrom cwd remoteLocator manifestFile = do
	     let tebDir = cwd </> coreDirectory
	     createDirectory $ tebDir
	     before <- getDirectoryContents tebDir
	     gitClone tebDir remoteLocator Nothing >>= outputStrings
	     after <- getDirectoryContents tebDir
	     let repoDirRelativeToTeb = findNew before after
	     let manifestFileRelativeToTebDir = repoDirRelativeToTeb </> manifestFile
	     exists <- doesFileExist $ tebDir </> manifestFileRelativeToTebDir
	     if exists
	     	then writeManifestLocator tebDir repoDirRelativeToTeb manifestFileRelativeToTebDir >> return manifestFileRelativeToTebDir
		else error $ "manifest file " ++ manifestFile ++ " does not exits"
	     return manifestFileRelativeToTebDir
	     where
		findNew before after = head $ filter (flip notElem before) after

getFinalManifestLocatorXML :: String -> String -> String
getFinalManifestLocatorXML repoDirRelativeToTeb manifestFileRelativeToTebDir = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><teb version=\"1.0\"><TebManifestRepoDir>" ++ repoDirRelativeToTeb ++ "</TebManifestRepoDir><TebManifestLocator>" ++ manifestFileRelativeToTebDir ++ "</TebManifestLocator></teb>"

writeManifestLocator :: String -> String -> String -> IO ()
writeManifestLocator tebDir repoDirRelativeToTeb manifestFileRelativeToTebDir = do
		     putStrLn $ "repo dir : " ++ repoDirRelativeToTeb ++ ", maifest rel to .teb :" ++ manifestFileRelativeToTebDir
		     writeFile (tebDir </> manifestLocationFilename) (getFinalManifestLocatorXML repoDirRelativeToTeb manifestFileRelativeToTebDir)
		     return ()

initialiseRepository :: CurrentWorkingDirectory -> Arguments -> IO ()
initialiseRepository cwd args =
		     let args' = getInitArgs args in
		     case args' of
		         Just (remoteLocator, manifestFile) -> do
			     checkDotTebDirExistence cwd False
			     manifestFile <- initRepoFrom cwd remoteLocator manifestFile
			     projectList <- loadTebProjectListFromManifest $ cwd </> coreDirectory </> manifestFile
			     mapM_ (cloneProject cwd) projectList
			     return ()
		         Nothing -> reportError args
 