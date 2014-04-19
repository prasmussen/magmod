module Magento.Helper (
    addHelper
) where

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory, takeBaseName)
import Template.Helper (helperXml, helperPhp)
import Magento.Module (codeRootPath)
import Util (capitalize,
    capitalizePath,
    lowercase,
    tmpFname,
    renameWithBackup)
import Data.String.Utils (replace)
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


addHelper :: FilePath -> String -> String -> String -> IO ()
addHelper configXmlPath namespace moduleName helperName = do
    insertHelperXmlIfMissing configXmlPath namespace moduleName
    createHelperPhpIfMissing configXmlPath namespace moduleName helperName

insertHelperXmlIfMissing :: FilePath -> String -> String -> IO ()
insertHelperXmlIfMissing configXmlPath namespace moduleName = do
    insertGlobalIfMissing configXmlPath
    exists <- xPathExists configXmlPath "/config/global/helpers"
    case exists of
        True -> return ()
        False -> insertHelperXml configXmlPath namespace moduleName

insertHelperXml :: FilePath -> String -> String -> IO ()
insertHelperXml configXmlPath namespace moduleName = do
    let path = tmpFname configXmlPath
    xml <- helperXml
        (lowercase moduleName)
        (classNamePrefix namespace moduleName)
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

composeHelperPhpPath :: FilePath -> String -> String
composeHelperPhpPath configXmlPath helperName =
    joinPath [
        codeRootPath configXmlPath,
        "Helper",
        (capitalizePath helperName) ++ ".php"
    ]

createHelperPhpIfMissing :: FilePath -> String -> String -> String -> IO ()
createHelperPhpIfMissing configXmlPath namespace moduleName helperName =
    let helperPhpPath = composeHelperPhpPath configXmlPath helperName
    in do
        createDirectoryIfMissing True (takeDirectory helperPhpPath)
        writeHelperPhpIfMissing helperPhpPath namespace moduleName helperName

writeHelperPhpIfMissing :: FilePath -> String -> String -> String -> IO ()
writeHelperPhpIfMissing path namespace moduleName helperName = do
    exists <- doesFileExist path
    case exists of
        False -> writeHelperPhp path namespace moduleName helperName
        True -> return ()

writeHelperPhp :: FilePath -> String -> String -> String -> IO ()
writeHelperPhp path namespace moduleName helperName = do
    php <- helperPhp $ className namespace moduleName helperName
    writeFile path php

globalElement :: (ArrowXml a) => a b XmlTree
globalElement = toXmlTree "<global></global>"

classNamePrefix :: String -> String -> String
classNamePrefix namespace moduleName =
    foldl (++) "" [
        capitalize namespace,
        "_",
        capitalize moduleName,
        "_",
        "Helper"
    ]

className :: String -> String -> String -> String
className namespace moduleName helperName =
    foldl (++) "" [
        classNamePrefix namespace moduleName,
        "_",
        replace "/" "_" $ capitalizePath helperName
    ]
