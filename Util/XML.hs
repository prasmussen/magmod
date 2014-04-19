module Util.XML (
    toXmlTree,
    xPathExists
) where

import Control.Category ((>>>))
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Arrow.ReadDocument (readDocument, xread)
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow (runX)
import Text.XML.HXT.Arrow.XmlArrow (getElemName, ArrowXml)
import Text.XML.HXT.XPath.Arrows (getXPathTrees)
import Control.Arrow.ArrowList (constA)

toXmlTree :: (ArrowXml a) => String -> a b XmlTree
toXmlTree str = constA str >>> xread

xPathExists :: FilePath -> String -> IO Bool
xPathExists fpath xpath = do
    res <- runX (
        readDocument [] fpath >>>
        getXPathTrees xpath >>>
        getElemName)
    case res of
        [] -> return False
        _ -> return True

