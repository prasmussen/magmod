module Util.XML (
    toXmlTree,
    xPathExists,
    insertXmlIfMissing,
    printXml,
    indentXml
) where

import Control.Monad (when)
import Control.Category ((>>>))
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Arrow.ReadDocument (readDocument, readString, xread)
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow (runX)
import Text.XML.HXT.Arrow.XmlArrow (getElemName, ArrowXml)
import Text.XML.HXT.Arrow.Edit (indentDoc)
import Text.XML.HXT.Arrow.WriteDocument (writeDocument, writeDocumentToString)
import Text.XML.HXT.Arrow.XmlState.SystemConfig (
    withPreserveComment,
    withOutputEncoding,
    withXmlPi)
import Text.XML.HXT.XPath.Arrows (getXPathTrees, processXPathTrees)
import Control.Arrow.ArrowList (constA, this)
import Control.Arrow.ArrowTree (insertChildrenAt)
import Data.String.Utils (join, split)
import Data.String.EncodingNames (utf8)
import Util (renameWithBackupAndPrint, tmpFname)

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

insertXmlIfMissing :: FilePath -> String -> String -> IO ()
insertXmlIfMissing _ "" _ = return ()
insertXmlIfMissing fpath xpath xml = do
    -- Do recursive insert on all missing xpath parents
    insertXmlIfMissing
        fpath
        (xPathParent xpath)
        (xmlElement $ xPathCurrent $ xPathParent xpath)
    -- Check if xpath exists in xml file
    exists <- xPathExists fpath xpath
    when (not exists) $ insertXml fpath (xPathParent xpath) xml

insertXml :: FilePath -> String -> String -> IO ()
insertXml fpath xpath xml = do
    let path = tmpFname fpath
    _ <- runX $
        readDocument [withPreserveComment True] fpath >>>
        processXPathTrees
            (this >>> insertChildrenAt 0 (toXmlTree xml)) xpath >>>
        indentDoc >>>
        writeDocument [] path
    renameWithBackupAndPrint path fpath
    return ()

indentXml :: String -> IO String
indentXml xml = do
    [rc] <- runX $
        readString [withPreserveComment True] xml >>>
        indentDoc >>>
        writeDocumentToString [withOutputEncoding utf8, withXmlPi True]
    return rc

printXml :: String -> IO ()
printXml xml = do
    _ <- runX $
        readString [withPreserveComment True] xml >>>
        indentDoc >>>
        writeDocument [] "-"
    return ()

xmlElement :: String -> String
xmlElement name = join "" ["<", name, ">", "</", name, ">"]

xPathParent :: String -> String
xPathParent xpath = join "/" $ init $ split "/" xpath

xPathCurrent :: String -> String
xPathCurrent xpath = last $ split "/" xpath
