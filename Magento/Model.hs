module Magento.Model (
    addModel,
    modelPath
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Template.Model (modelXml, modelPhp)
import Magento.Module.Path (modelBasePath)
import Magento.Module (
    ModuleInfo,
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


addModel :: ModuleInfo -> String -> IO ()
addModel info modelName = do
    insertModelXmlIfMissing info
    createModelPhpIfMissing info modelName

insertModelXmlIfMissing :: ModuleInfo -> IO ()
insertModelXmlIfMissing info = do
    xml <- modelXml (lowercase $ getName info) (classNamePrefix info)
    insertXmlIfMissing (getConfigXml info) "/config/global/models" xml

createModelPhpIfMissing :: ModuleInfo -> String -> IO ()
createModelPhpIfMissing info modelName =
    let path = modelPath info modelName in do
        createDirectoryIfMissing True (takeDirectory path)
        writeModelPhpIfMissing info path modelName

writeModelPhpIfMissing :: ModuleInfo -> FilePath -> String -> IO ()
writeModelPhpIfMissing info path modelName = do
    exists <- doesFileExist path
    when (not exists) $ writeModelPhp info path modelName

writeModelPhp :: ModuleInfo -> FilePath -> String -> IO ()
writeModelPhp info path modelName = do
    php <- modelPhp $ className info modelName
    writeFileAndPrint path php

modelPath :: ModuleInfo -> String -> String
modelPath info modelName =
    joinPath [
        modelBasePath info,
        (capitalizePath modelName) ++ ".php"
    ]

classNamePrefix :: ModuleInfo -> String
classNamePrefix info =
    join "_" [
        capitalize $ getNamespace info,
        capitalize $ getName info,
        "Model"
    ]

className :: ModuleInfo -> String -> String
className info modelName =
    join "_" [
        classNamePrefix info,
        replace "/" "_" $ capitalizePath modelName
    ]
