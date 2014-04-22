module Magento.Block (
    addBlock
) where

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Template.Block (blockXml, blockPhp)
import Magento.Module (codeRootPath)
import Util (
    capitalize,
    capitalizePath,
    lowercase)
import Data.String.Utils (replace)
import Util.XML (insertXmlIfMissing)


addBlock :: FilePath -> String -> String -> String -> IO ()
addBlock configXmlPath namespace moduleName blockName = do
    insertBlockXmlIfMissing configXmlPath namespace moduleName
    createBlockPhpIfMissing configXmlPath namespace moduleName blockName

insertBlockXmlIfMissing :: FilePath -> String -> String -> IO ()
insertBlockXmlIfMissing configXmlPath namespace moduleName = do
    xml <- blockXml
        (lowercase moduleName)
        (classNamePrefix namespace moduleName)
    insertXmlIfMissing configXmlPath "/config/global/blocks" xml

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
