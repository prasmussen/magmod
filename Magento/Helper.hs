module Magento.Helper (
    insertHelperXmlIfMissing
) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (joinPath, takeDirectory, takeBaseName)
import Template.Helper (helperXml)
import Util (capitalize, lowercase, tmpFname, renameWithBackup)
import Util.XML (xPathExists, toXmlTree)
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow (runX)
import Text.XML.HXT.Arrow.ReadDocument (readDocument, xread)
import Text.XML.HXT.Arrow.XmlArrow (hasName, ArrowXml)
import Text.XML.HXT.Arrow.WriteDocument (writeDocument)
import Text.XML.HXT.Arrow.Edit (indentDoc)
import Control.Category ((>>>))
import Control.Arrow.ArrowList (this)
import Control.Arrow.ArrowTree (processTopDown, insertChildrenAt)
import Control.Arrow.ArrowIf (ifA)


insertHelperXmlIfMissing :: FilePath -> String -> String -> IO ()
insertHelperXmlIfMissing configXmlPath namespace name = do
    insertGlobalIfMissing configXmlPath
    exists <- xPathExists configXmlPath "/config/global/helpers"
    case exists of
        True -> return ()
        False -> insertHelperXml configXmlPath namespace name

insertHelperXml :: FilePath -> String -> String -> IO ()
insertHelperXml configXmlPath namespace name = do
    let path = tmpFname configXmlPath
    xml <- helperXml (lowercase name) (className namespace name)
    runX (
        readDocument [] configXmlPath >>>
        processTopDown (ifA
            (hasName "global")
            (insertChildrenAt 0 (toXmlTree xml))
            this) >>>
        indentDoc >>>
        writeDocument [] path)
    renameWithBackup path configXmlPath
    return ()

insertGlobalIfMissing :: FilePath -> IO ()
insertGlobalIfMissing configXmlPath = do
    exists <- xPathExists configXmlPath "/config/global"
    case exists of
        True -> return ()
        False -> insertGlobal configXmlPath

insertGlobal :: FilePath -> IO ()
insertGlobal configXmlPath = do
    let path = tmpFname configXmlPath
    runX (
        readDocument [] configXmlPath >>>
        processTopDown (ifA
            (hasName "config")
            (insertChildrenAt 0 globalElement)
            this) >>>
        indentDoc >>>
        writeDocument [] path)
    renameWithBackup path configXmlPath
    return ()

globalElement :: (ArrowXml a) => a b XmlTree
globalElement = toXmlTree "<global></global>"

className :: String -> String -> String
className namespace name =
    foldl (++) "" [
        capitalize namespace,
        "_",
        capitalize name,
        "_",
        "Helper"
    ]
