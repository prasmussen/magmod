module Magento.Helper (
    addHelper
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Template.Helper (helperXml, helperPhp)
import Magento.Module (codeRootPath)
import Util (
    capitalize,
    capitalizePath,
    lowercase)
import Data.String.Utils (replace)
import Util.XML (insertXmlIfMissing)


addHelper :: FilePath -> String -> String -> String -> IO ()
addHelper configXmlPath namespace moduleName helperName = do
    insertHelperXmlIfMissing configXmlPath namespace moduleName
    createHelperPhpIfMissing configXmlPath namespace moduleName helperName

insertHelperXmlIfMissing :: FilePath -> String -> String -> IO ()
insertHelperXmlIfMissing configXmlPath namespace moduleName = do
    xml <- helperXml
        (lowercase moduleName)
        (classNamePrefix namespace moduleName)
    insertXmlIfMissing configXmlPath "/config/global/helpers" xml

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
    when (not exists) $
        writeHelperPhp path namespace moduleName helperName

writeHelperPhp :: FilePath -> String -> String -> String -> IO ()
writeHelperPhp path namespace moduleName helperName = do
    php <- helperPhp $ className namespace moduleName helperName
    writeFile path php

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
