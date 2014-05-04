module Magento.Module.XML (
    readModuleInfo,
    readVersion,
    readEntities
) where

import Magento.Module (
    ModuleInfo,
    moduleInfo,
    getConfigXml,
    getNamespace,
    getName)
import Magento.Module.Version (Version, parseVersion)
import Data.List.Split (splitOn)
import Control.Category ((>>>))
import Control.Arrow.ArrowTree (getChildren)
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow (runX)
import Text.XML.HXT.Arrow.ReadDocument (readDocument)
import Text.XML.HXT.Arrow.XmlArrow (getLocalPart, getText)
import Text.XML.HXT.DOM.QualifiedName (localPart)
import Text.XML.HXT.XPath.Arrows (getXPathTrees)


readModuleInfo :: FilePath -> IO ModuleInfo
readModuleInfo path = do
    names <- runX $
        readDocument [] path >>>
        getXPathTrees "/config/modules/*" >>>
        getLocalPart
    return $ newModuleInfo path (head names)

readVersion :: FilePath -> IO Version
readVersion path = do
    texts <- runX $
        readDocument [] path >>>
        getXPathTrees "/config/modules/*/version" >>>
        getChildren >>>
        getText
    return $ parseVersion . head $ texts

readEntities :: FilePath -> IO [String]
readEntities path = do
    runX $
        readDocument [] path >>>
        getXPathTrees "/config/global/models/*/entities/*" >>>
        getLocalPart

newModuleInfo :: FilePath -> String -> ModuleInfo
newModuleInfo path str =
    let [namespace, name] = splitOn "_" str in
        moduleInfo path namespace name
