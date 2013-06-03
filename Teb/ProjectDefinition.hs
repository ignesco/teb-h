{-# LANGUAGE Arrows,  NoMonomorphismRestriction #-}
module Teb.ProjectDefinition where

import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Core
import Text.XML.HXT.DOM.FormatXmlTree

data ProjectStatus = Active | Inactive | Disabled deriving (Show, Eq)
        
data TebProject = TebProject {  projid :: String,
                                repolocation :: String,
                                projectStatus :: ProjectStatus,
                                workingdirectory :: String,
                                branch :: String
                        }
        deriving (Show)

loadTebProjectListFromManifest :: String -> IO [TebProject]
loadTebProjectListFromManifest xmlFile = do
			       trees <- runX (getProjectTrees xmlFile)
			       return $ concat $ map (runLA mapToTebProject) trees

getProjectTrees :: String -> IOSLA (XIOState s) a XmlTree
getProjectTrees xml = documentRoot xml >>> getChildren >>> hasName "projects" >>> getChildren >>> hasName "project"

documentRoot :: String -> IOSLA (XIOState s) a XmlTree
documentRoot xml = readDocument [] xml >>> getChildren >>> hasName "teb"

mapToTebProject :: ArrowXml t => t XmlTree TebProject
mapToTebProject = proc tree -> do
                         pid    <- getAttrValue "id" -< tree
                         rloc   <- getChildren >>> hasName "repoLocation" >>> getChildren >>> getText -< tree
                         st'    <- getChildren >>> hasName "status" >>> getChildren >>> getText -< tree
                         let st = if st'=="active" then Active else Inactive
                         wd     <- getChildren >>> hasName "workingDirectory" >>> getChildren >>> getText -< tree
                         br     <- getChildren >>> hasName "branch" >>> getChildren >>> getText -< tree
                         returnA -< TebProject {projid = pid,
                                repolocation = rloc,
                                projectStatus = st,
                                workingdirectory = wd,
                                branch = br }
