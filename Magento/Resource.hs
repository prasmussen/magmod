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
import Magento.Module.Path (resourceBasePath)
import Magento.Module (
    ModuleInfo,
    getName,
    getNamespace,
    getConfigXml,
    getFullName)
import Magento.Model (addModel, modelPath)
import Util (
    writeFileAndPrint,
    capitalize,
    capitalizePath,
    lowercase)
import Data.String.Utils (replace, join)
import Util.XML (insertXmlIfMissing)
import Util.PHP (insertPhpMethod)


addResource :: ModuleInfo -> String -> IO ()
addResource info entityName = do
    addModel info entityName
    insertModelConstructor info (modelPath info entityName) entityName
    insertModelResourceXmlIfMissing info
    insertResourceXmlIfMissing info
    insertEntityXmlIfMissing info entityName
    createResourcePhpIfMissing info entityName
    createCollectionPhpIfMissing info entityName

insertModelConstructor :: ModuleInfo -> FilePath -> String -> IO ()
insertModelConstructor info path entityName = do
    php <- modelConstructor (lowercase $ getName info) (lowercase entityName)
    insertPhpMethod path php

insertModelResourceXmlIfMissing :: ModuleInfo -> IO ()
insertModelResourceXmlIfMissing info =
    insertXmlIfMissing
        (getConfigXml info) modelResourceXpath (modelResourceXml info)

insertResourceXmlIfMissing :: ModuleInfo -> IO ()
insertResourceXmlIfMissing info = do
    xml <- resourceXml (classPrefix info) (resourceName info)
    insertXmlIfMissing (getConfigXml info) (resourceXpath info) xml

insertEntityXmlIfMissing :: ModuleInfo -> String -> IO ()
insertEntityXmlIfMissing info entityName = do
    xml <- entityXml entityName $ tableName info entityName
    insertXmlIfMissing (getConfigXml info) (entityXpath entityName) xml

createResourcePhpIfMissing :: ModuleInfo -> String -> IO ()
createResourcePhpIfMissing info entityName =
    let path = resourcePath info entityName in do
        createDirectoryIfMissing True (takeDirectory path)
        writeResourcePhpIfMissing info path entityName

writeResourcePhpIfMissing :: ModuleInfo -> FilePath -> String -> IO ()
writeResourcePhpIfMissing info path entityName = do
    exists <- doesFileExist path
    when (not exists) $ writeResourcePhp info path entityName

writeResourcePhp :: ModuleInfo -> FilePath -> String -> IO ()
writeResourcePhp info path entityName = do
    php <- resourcePhp
        (lowercase $ getName info)
        (className info entityName)
        (lowercase entityName)
    writeFileAndPrint path php

createCollectionPhpIfMissing :: ModuleInfo -> String -> IO ()
createCollectionPhpIfMissing info entityName =
    let path = collectionPath info entityName in do
        createDirectoryIfMissing True (takeDirectory path)
        writeCollectionPhpIfMissing info path entityName

writeCollectionPhpIfMissing :: ModuleInfo -> FilePath -> String -> IO ()
writeCollectionPhpIfMissing info path entityName = do
    exists <- doesFileExist path
    when (not exists) $ writeCollectionPhp info path entityName

writeCollectionPhp :: ModuleInfo -> FilePath -> String -> IO ()
writeCollectionPhp info path entityName = do
    php <- collectionPhp
        (lowercase $ getName info)
        (className info entityName)
        (lowercase entityName)
    writeFileAndPrint path php

modelResourceXml :: ModuleInfo -> String
modelResourceXml info =
    "<resourceModel>" ++ (resourceName info)  ++ "</resourceModel>"

modelResourceXpath :: String
modelResourceXpath = "/config/global/models/*[not(entities)]/resourceModel"

resourceXpath :: ModuleInfo -> String
resourceXpath info =
    join "" ["/config/global/models/", resourceName info]

entityXpath :: String -> String
entityXpath entityName =
    join "" ["/config/global/models/*/entities/", entityName]

resourcePath :: ModuleInfo -> String -> String
resourcePath info entityName =
    joinPath [
        resourceBasePath info,
        (capitalizePath entityName) ++ ".php"
    ]

collectionPath :: ModuleInfo -> String -> String
collectionPath info entityName =
    let path = resourcePath info entityName in
        joinPath [
            takeDirectory path,
            takeBaseName path,
            "Collection.php"
        ]

classPrefix :: ModuleInfo -> String
classPrefix info =
    join "_" [
        capitalize $ getNamespace info,
        capitalize $ getName info,
        "Model",
        "Resource"
    ]

resourceName :: ModuleInfo -> String
resourceName info =
    join "_" [lowercase $ getName info, "resource"]

tableName :: ModuleInfo -> String -> String
tableName info entityName =
    join "_" [lowercase $ getName info, lowercase entityName]

className :: ModuleInfo -> String -> String
className info entityName =
    join "_" [
        classPrefix info,
        (capitalize entityName)
    ]
