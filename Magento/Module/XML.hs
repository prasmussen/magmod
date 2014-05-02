module Magento.Module.XML (
    readModuleInfo
) where

import Magento.Module (ModuleInfo, moduleInfo, getConfigXml, getNamespace, getName)
import Data.List.Split (splitOn)
import Control.Category ((>>>))
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow (runX)
import Text.XML.HXT.Arrow.ReadDocument (readDocument)
import Text.XML.HXT.Arrow.XmlArrow (getElemName)
import Text.XML.HXT.DOM.QualifiedName (localPart)
import Text.XML.HXT.XPath.Arrows (getXPathTrees)


readModuleInfo :: FilePath -> IO ModuleInfo
readModuleInfo path = do
    names <- runX (
        readDocument [] path >>>
        getXPathTrees "/config/modules/*" >>>
        getElemName)
    return $ newModuleInfo path (localPart $ head names)

newModuleInfo :: FilePath -> String -> ModuleInfo
newModuleInfo path str =
    let [namespace, name] = splitOn "_" str in
        moduleInfo path namespace name
