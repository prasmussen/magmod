module Magento.Block (
    addBlock
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Template.Block (blockXml, blockPhp)
import Magento.Module (
    ModuleInfo,
    blockBasePath,
    getName,
    getNamespace,
    getConfigXml)
import Util (
    writeFileAndPrint,
    capitalize,
    capitalizePath,
    lowercase)
import Data.String.Utils (replace, join)
import Util.XML (insertXmlIfMissing)


addBlock :: ModuleInfo -> String -> IO ()
addBlock info blockName = do
    insertBlockXmlIfMissing info
    createBlockPhpIfMissing info blockName

insertBlockXmlIfMissing :: ModuleInfo -> IO ()
insertBlockXmlIfMissing info = do
    xml <- blockXml (lowercase $ getName info) (classNamePrefix info)
    insertXmlIfMissing (getConfigXml info) "/config/global/blocks" xml

createBlockPhpIfMissing :: ModuleInfo -> String -> IO ()
createBlockPhpIfMissing info blockName =
    let path = blockPath info blockName in do
        createDirectoryIfMissing True (takeDirectory path)
        writeBlockPhpIfMissing info path blockName

writeBlockPhpIfMissing :: ModuleInfo -> FilePath -> String -> IO ()
writeBlockPhpIfMissing info path blockName = do
    exists <- doesFileExist path
    when (not exists) $ writeBlockPhp info path blockName

writeBlockPhp :: ModuleInfo -> FilePath -> String -> IO ()
writeBlockPhp info path blockName = do
    php <- blockPhp $ className info blockName
    writeFileAndPrint path php

blockPath :: ModuleInfo -> String -> String
blockPath info blockName =
    joinPath [
        blockBasePath info,
        (capitalizePath blockName) ++ ".php"
    ]

classNamePrefix :: ModuleInfo -> String
classNamePrefix info =
    join "_" [
        capitalize $ getNamespace info,
        capitalize $ getName info,
        "Block"
    ]

className :: ModuleInfo -> String -> String
className info blockName =
    join "_" [
        classNamePrefix info,
        replace "/" "_" $ capitalizePath blockName
    ]
