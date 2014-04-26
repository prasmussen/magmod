module Magento.Model (
    addModel
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Template.Model (modelXml, modelPhp)
import Magento.Module (codeRootPath)
import Util (
    capitalize,
    capitalizePath,
    lowercase)
import Data.String.Utils (replace)
import Util.XML (insertXmlIfMissing)


addModel :: FilePath -> String -> String -> String -> IO ()
addModel configXmlPath namespace moduleName modelName = do
    insertModelXmlIfMissing configXmlPath namespace moduleName
    createModelPhpIfMissing configXmlPath namespace moduleName modelName

insertModelXmlIfMissing :: FilePath -> String -> String -> IO ()
insertModelXmlIfMissing configXmlPath namespace moduleName = do
    xml <- modelXml
        (lowercase moduleName)
        (classNamePrefix namespace moduleName)
    insertXmlIfMissing configXmlPath "/config/global/models" xml

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
    when (not exists) $
        writeModelPhp path namespace moduleName modelName

writeModelPhp :: FilePath -> String -> String -> String -> IO ()
writeModelPhp path namespace moduleName modelName = do
    php <- modelPhp $ className namespace moduleName modelName
    writeFile path php

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
