module Teb where

import System (getArgs)
import System.Directory (getCurrentDirectory, doesDirectoryExist)
import System.FilePath (isAbsolute)
import Teb.Consts
import Teb.Fetch (fetch)
import Teb.Init (initialiseRepository)
import Teb.Info (info)
import Teb.Reinit (reinit)
import Teb.Status (status)
import Teb.Sync (sync)
import Teb.Types

data TebCommand = Init |
     		  Status |
		  Fetch |
		  Sync |
		  Info |
		  Reinit |
		  Version |
		  Usage
		  deriving (Show)

getCommand :: [String] -> Maybe TebCommand
getCommand ("init":_) = Just Init
getCommand ("status":_) = Just Status
getCommand ("fetch":_) = Just Fetch
getCommand ("sync":_) = Just Sync
getCommand ("info":_) = Just Info
getCommand ("reinit":_) = Just Reinit
getCommand ("version":_) = Just Version
getCommand ("-v":_) = Just Version
getCommand ("-h":_) = Just Usage
getCommand (_:_) = Nothing
getCommand _ = Nothing

showUsage :: IO ()
showUsage = mapM_ putStrLn usageStrings

outputVersionDescription = putStrLn versionDescription

tebMain' :: CurrentWorkingDirectory -> Arguments -> IO ()
tebMain' currentDirectory args =
	 do
 	   validDir <- doesDirectoryExist currentDirectory
	   let pathIsAbsolute = isAbsolute currentDirectory
	   if validDir && pathIsAbsolute
	     then
	       case getCommand args of
	         Just Init      -> initialiseRepository currentDirectory args
	         Just Status	-> status currentDirectory args
	         Just Fetch	-> fetch currentDirectory args
	         Just Sync	-> sync currentDirectory args
	         Just Info	-> info currentDirectory args
	         Just Reinit	-> reinit currentDirectory args
	         Just Version	-> outputVersionDescription
	         Just Usage	-> showUsage
	         Nothing	-> putStrLn "Unknown command : try teb -h for help."
	     else putStrLn $ "Current Directory is invalid: " ++ currentDirectory

tebMain :: IO ()
tebMain = do
     currentDirectory <- getCurrentDirectory
     args <- getArgs
     tebMain' currentDirectory args