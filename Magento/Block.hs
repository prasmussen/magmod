module Magento.Block (
    addBlock
) where

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory, takeBaseName)
import Template.Block (blockXml, blockPhp)
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


addBlock :: FilePath -> String -> String -> String -> IO ()
addBlock configXmlPath namespace moduleName blockName = do
    insertBlockXmlIfMissing configXmlPath namespace moduleName
    createBlockPhpIfMissing configXmlPath namespace moduleName blockName

insertBlockXmlIfMissing :: FilePath -> String -> String -> IO ()
insertBlockXmlIfMissing configXmlPath namespace moduleName = do
    insertGlobalIfMissing configXmlPath
    exists <- xPathExists configXmlPath "/config/global/blocks"
    case exists of
        True -> return ()
        False -> insertBlockXml configXmlPath namespace moduleName

insertBlockXml :: FilePath -> String -> String -> IO ()
insertBlockXml configXmlPath namespace moduleName = do
    let path = tmpFname configXmlPath
    xml <- blockXml
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

composeBlockPhpPath :: FilePath -> String -> String
composeBlockPhpPath configXmlPath blockName =
    joinPath [
        codeRootPath configXmlPath,
        "Block",
        (capitalizePath blockName) ++ ".php"
    ]

createBlockPhpIfMissing :: FilePath -> String -> String -> String -> IO ()
createBlockPhpIfMissing configXmlPath namespace moduleName blockName =
    let blockPhpPath = composeBlockPhpPath configXmlPath blockName
    in do
        createDirectoryIfMissing True (takeDirectory blockPhpPath)
        writeBlockPhpIfMissing blockPhpPath namespace moduleName blockName

writeBlockPhpIfMissing :: FilePath -> String -> String -> String -> IO ()
writeBlockPhpIfMissing path namespace moduleName blockName = do
    exists <- doesFileExist path
    case exists of
        False -> writeBlockPhp path namespace moduleName blockName
        True -> return ()

writeBlockPhp :: FilePath -> String -> String -> String -> IO ()
writeBlockPhp path namespace moduleName blockName = do
    php <- blockPhp $ className namespace moduleName blockName
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
        "Block"
    ]

className :: String -> String -> String -> String
className namespace moduleName blockName =
    foldl (++) "" [
        classNamePrefix namespace moduleName,
        "_",
        replace "/" "_" $ capitalizePath blockName
    ]
