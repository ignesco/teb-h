module Teb.Git where

import System.IO
import System.Process
import Teb.Types
import Teb.Utils

import Data.Text (Text (..), split, pack, unpack)

gitCheckout :: String -> String -> IO [String]
gitCheckout cwd branch = gitRunner cwd ["checkout",branch]

gitClone :: String -> RemoteLocator -> Maybe String -> IO [String]
gitClone cwd remoteLocator localDirectory = gitRunner cwd ( ["clone",remoteLocator] ++ maybe [] (\s -> [s]) localDirectory )

gitStatus ::String -> IO [String]
gitStatus cwd = gitRunner cwd ["status"]

gitFetch ::String -> IO [String]
gitFetch cwd = gitRunner cwd ["fetch"]

gitRebase ::String -> String -> String -> IO [String]
gitRebase cwd remote branch = gitRunner cwd ["rebase", remote++"/"++branch]

getarr :: Text -> [Text]
getarr t = split (\c -> if c=='\n' then True else False) t

gitRunner :: String -> [String] -> IO [String]
gitRunner wd params  = do
     let p = (proc "git" params){std_out = CreatePipe, std_err = CreatePipe, cwd=Just wd}
     (_, Just hOut, Just hErr, ph) <- createProcess p
     waitForProcess ph
     outs <- hGetContents hOut
     let outArray = map unpack $ getarr . pack $ outs
     errs <- hGetContents hErr
     let errArray = map unpack $ getarr . pack $ errs
     return . clean $ outArray ++ errArray
     where
	clean arr = filter (\s -> if length s==0 then False else True) arr
