module Magento.Module.XML (
    readNamespaceAndName
) where

import Data.List.Split (splitOn)
import Control.Category ((>>>))
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow (runX)
import Text.XML.HXT.Arrow.ReadDocument (readDocument)
import Text.XML.HXT.Arrow.XmlArrow (getElemName)
import Text.XML.HXT.DOM.QualifiedName (localPart)
import Text.XML.HXT.XPath.Arrows (getXPathTrees)


readNamespaceAndName :: FilePath -> IO (String, String)
readNamespaceAndName path = do
    names <- runX (
        readDocument [] path >>>
        getXPathTrees "/config/modules/*" >>>
        getElemName)
    return $ namespaceAndName . localPart $ head names

namespaceAndName :: String -> (String, String)
namespaceAndName str =
    let [namespace, name] = splitOn "_" str in (namespace, name)
