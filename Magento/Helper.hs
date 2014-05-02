module Magento.Helper (
    addHelper
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Template.Helper (helperXml, helperPhp)
import Magento.Module (
    ModuleInfo,
    codeRootPath,
    getName,
    getNamespace,
    getConfigXml,
    getFullName)
import Util (
    writeFileAndPrint,
    capitalize,
    capitalizePath,
    lowercase)
import Data.String.Utils (replace, join)
import Util.XML (insertXmlIfMissing)


addHelper :: ModuleInfo -> String -> IO ()
addHelper info helperName = do
    insertHelperXmlIfMissing info
    createHelperPhpIfMissing info helperName

insertHelperXmlIfMissing :: ModuleInfo -> IO ()
insertHelperXmlIfMissing info = do
    xml <- helperXml (lowercase $ getName info) (classNamePrefix info)
    insertXmlIfMissing (getConfigXml info) "/config/global/helpers" xml

createHelperPhpIfMissing :: ModuleInfo -> String -> IO ()
createHelperPhpIfMissing info helperName =
    let path = helperPath info helperName in do
        createDirectoryIfMissing True (takeDirectory path)
        writeHelperPhpIfMissing info path helperName

writeHelperPhpIfMissing :: ModuleInfo -> FilePath -> String -> IO ()
writeHelperPhpIfMissing info path helperName = do
    exists <- doesFileExist path
    when (not exists) $ writeHelperPhp info path helperName

writeHelperPhp :: ModuleInfo -> FilePath -> String -> IO ()
writeHelperPhp info path helperName = do
    php <- helperPhp $ className info helperName
    writeFileAndPrint path php

helperPath :: ModuleInfo -> String -> String
helperPath info helperName =
    joinPath [
        codeRootPath info,
        "Helper",
        (capitalizePath helperName) ++ ".php"
    ]

classNamePrefix :: ModuleInfo -> String
classNamePrefix info =
    join "_" [
        capitalize $ getNamespace info,
        capitalize $ getName info,
        "Helper"
    ]

className :: ModuleInfo -> String -> String
className info helperName =
    join "" [
        classNamePrefix info,
        replace "/" "_" $ capitalizePath helperName
    ]
