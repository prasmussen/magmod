module Magento.Resource (
    addResource
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory, takeBaseName)
import Template.Resource (
    resourceXml,
    entityXml,
    modelConstructor,
    resourcePhp,
    collectionPhp)
import Magento.Module (codeRootPath, fullModuleName)
import Magento.Model (addModel, modelPath)
import Util (
    writeFileAndPrint,
    capitalize,
    capitalizePath,
    lowercase)
import Data.String.Utils (replace, join)
import Util.XML (insertXmlIfMissing)
import Util.PHP (insertPhpMethod)


addResource :: FilePath -> String -> String -> String -> IO ()
addResource configXmlPath namespace moduleName entityName = do
    addModel configXmlPath namespace moduleName entityName
    insertModelConstructor
        (modelPath configXmlPath entityName) moduleName entityName
    insertModelResourceXmlIfMissing configXmlPath moduleName
    insertResourceXmlIfMissing configXmlPath namespace moduleName
    insertEntityXmlIfMissing configXmlPath moduleName entityName
    createResourcePhpIfMissing
        configXmlPath namespace moduleName entityName
    createCollectionPhpIfMissing
        configXmlPath namespace moduleName entityName

insertModelConstructor :: FilePath -> String -> String -> IO ()
insertModelConstructor path moduleName entityName = do
    php <- modelConstructor (lowercase moduleName) (lowercase entityName)
    insertPhpMethod path php

insertModelResourceXmlIfMissing :: FilePath -> String -> IO ()
insertModelResourceXmlIfMissing configXmlPath moduleName = do
    insertXmlIfMissing
        configXmlPath modelResourceXpath (modelResourceXml moduleName)

insertResourceXmlIfMissing :: FilePath -> String -> String -> IO ()
insertResourceXmlIfMissing configXmlPath namespace moduleName = do
    xml <- resourceXml
        (classPrefix namespace moduleName)
        (resourceName moduleName)
    insertXmlIfMissing configXmlPath (resourceXpath moduleName) xml

insertEntityXmlIfMissing :: FilePath -> String -> String -> IO ()
insertEntityXmlIfMissing configXmlPath moduleName entityName = do
    xml <- entityXml entityName (tableName moduleName entityName)
    insertXmlIfMissing configXmlPath (entityXpath entityName) xml

createResourcePhpIfMissing :: FilePath -> String -> String -> String -> IO ()
createResourcePhpIfMissing configXmlPath namespace moduleName entityName =
    let path = resourcePath configXmlPath entityName
    in do
        createDirectoryIfMissing True (takeDirectory path)
        writeResourcePhpIfMissing
            path namespace moduleName entityName

writeResourcePhpIfMissing :: FilePath -> String -> String -> String -> IO ()
writeResourcePhpIfMissing path namespace moduleName entityName = do
    exists <- doesFileExist path
    when (not exists) $
        writeResourcePhp path namespace moduleName entityName

writeResourcePhp :: FilePath -> String -> String -> String -> IO ()
writeResourcePhp path namespace moduleName entityName = do
    php <- resourcePhp
        (lowercase moduleName)
        (className namespace moduleName entityName)
        (lowercase entityName)
    writeFileAndPrint path php

createCollectionPhpIfMissing :: FilePath -> String -> String -> String -> IO ()
createCollectionPhpIfMissing configXmlPath namespace moduleName entityName =
    let path = collectionPath configXmlPath entityName
    in do
        createDirectoryIfMissing True (takeDirectory path)
        writeCollectionPhpIfMissing
            path namespace moduleName entityName

writeCollectionPhpIfMissing :: FilePath -> String -> String -> String -> IO ()
writeCollectionPhpIfMissing path namespace moduleName entityName = do
    exists <- doesFileExist path
    when (not exists) $
        writeCollectionPhp path namespace moduleName entityName

writeCollectionPhp :: FilePath -> String -> String -> String -> IO ()
writeCollectionPhp path namespace moduleName entityName = do
    php <- collectionPhp
        (lowercase moduleName)
        (className namespace moduleName entityName)
        (lowercase entityName)
    writeFileAndPrint path php

modelResourceXml :: String -> String
modelResourceXml moduleName =
    "<resourceModel>" ++ (resourceName moduleName)  ++ "</resourceModel>"

modelResourceXpath :: String
modelResourceXpath = "/config/global/models/*[not(entities)]/resourceModel"

resourceXpath :: String -> String
resourceXpath moduleName =
    join "" ["/config/global/models/", resourceName moduleName]

entityXpath :: String -> String
entityXpath entityName =
    join "" ["/config/global/models/*/entities/", entityName]

resourcePath :: FilePath -> String -> String
resourcePath configXmlPath entityName =
    joinPath [
        codeRootPath configXmlPath,
        "Model",
        "Resource",
        (capitalizePath entityName) ++ ".php"
    ]

collectionPath :: FilePath -> String -> String
collectionPath configXmlPath entityName =
    let path = resourcePath configXmlPath entityName in
        joinPath [
            takeDirectory path,
            takeBaseName path,
            "Collection.php"
        ]

classPrefix :: String -> String -> String
classPrefix namespace moduleName =
    join "_" [
        capitalize namespace,
        capitalize moduleName,
        "Model",
        "Resource"
    ]

resourceName :: String -> String
resourceName moduleName =
    join "_" [lowercase moduleName, "resource"]

tableName :: String -> String -> String
tableName moduleName entityName =
    join "_" [lowercase moduleName, lowercase entityName]

className :: String -> String -> String -> String
className namespace moduleName entityName =
    join "_" [
        classPrefix namespace moduleName,
        (capitalize entityName)
    ]
