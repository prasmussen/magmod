module Magento.Model (
    addModel
) where

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory, takeBaseName)
import Template.Model (modelXml, modelPhp)
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


addModel :: FilePath -> String -> String -> String -> IO ()
addModel configXmlPath namespace moduleName modelName = do
    insertModelXmlIfMissing configXmlPath namespace moduleName
    createModelPhpIfMissing configXmlPath namespace moduleName modelName

insertModelXmlIfMissing :: FilePath -> String -> String -> IO ()
insertModelXmlIfMissing configXmlPath namespace moduleName = do
    insertGlobalIfMissing configXmlPath
    exists <- xPathExists configXmlPath "/config/global/models"
    case exists of
        True -> return ()
        False -> insertModelXml configXmlPath namespace moduleName

insertModelXml :: FilePath -> String -> String -> IO ()
insertModelXml configXmlPath namespace moduleName = do
    let path = tmpFname configXmlPath
    xml <- modelXml
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

composeModelPhpPath :: FilePath -> String -> String
composeModelPhpPath configXmlPath modelName =
    joinPath [
        codeRootPath configXmlPath,
        "Model",
        (capitalizePath modelName) ++ ".php"
    ]

createModelPhpIfMissing :: FilePath -> String -> String -> String -> IO ()
createModelPhpIfMissing configXmlPath namespace moduleName modelName =
    let modelPhpPath = composeModelPhpPath configXmlPath modelName
    in do
        createDirectoryIfMissing True (takeDirectory modelPhpPath)
        writeModelPhpIfMissing modelPhpPath namespace moduleName modelName

writeModelPhpIfMissing :: FilePath -> String -> String -> String -> IO ()
writeModelPhpIfMissing path namespace moduleName modelName = do
    exists <- doesFileExist path
    case exists of
        False -> writeModelPhp path namespace moduleName modelName
        True -> return ()

writeModelPhp :: FilePath -> String -> String -> String -> IO ()
writeModelPhp path namespace moduleName modelName = do
    php <- modelPhp $ className namespace moduleName modelName
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
        "Model"
    ]

className :: String -> String -> String -> String
className namespace moduleName modelName =
    foldl (++) "" [
        classNamePrefix namespace moduleName,
        "_",
        replace "/" "_" $ capitalizePath modelName
    ]
