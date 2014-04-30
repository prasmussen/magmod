module Magento.Block (
    addBlock
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Template.Block (blockXml, blockPhp)
import Magento.Module (codeRootPath)
import Util (
    writeFileAndPrint,
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

blockPath :: FilePath -> String -> String
blockPath configXmlPath blockName =
    joinPath [
        codeRootPath configXmlPath,
        "Block",
        (capitalizePath blockName) ++ ".php"
    ]

createBlockPhpIfMissing :: FilePath -> String -> String -> String -> IO ()
createBlockPhpIfMissing configXmlPath namespace moduleName blockName =
    let path = blockPath configXmlPath blockName
    in do
        createDirectoryIfMissing True (takeDirectory path)
        writeBlockPhpIfMissing path namespace moduleName blockName

writeBlockPhpIfMissing :: FilePath -> String -> String -> String -> IO ()
writeBlockPhpIfMissing path namespace moduleName blockName = do
    exists <- doesFileExist path
    when (not exists) $ writeBlockPhp path namespace moduleName blockName

writeBlockPhp :: FilePath -> String -> String -> String -> IO ()
writeBlockPhp path namespace moduleName blockName = do
    php <- blockPhp $ className namespace moduleName blockName
    writeFileAndPrint path php

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
