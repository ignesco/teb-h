{-# LANGUAGE Arrows,  NoMonomorphismRestriction #-}
module Teb.ManifestLocator where

import System.FilePath ((</>))
import Teb.Consts
import Teb.Types
import Teb.XmlUtils
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Core

getManifestLocation :: CurrentWorkingDirectory -> IO String
getManifestLocation cwd = do
		    ss <- runX $ (readDocument [] (getLocatorFile cwd)) >>> getLocatorText
		    return $ head ss

getLocatorFile :: CurrentWorkingDirectory -> String
getLocatorFile cwd = cwd </> coreDirectory </> manifestLocationFilename

getLocatorText = getChildren >>> hasName "teb" >>> getChildren >>> hasName "TebManifestLocator" >>> getChildren >>> getText
