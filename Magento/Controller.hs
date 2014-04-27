module Magento.Controller (
    addController
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Template.Controller (controllerXml, controllerPhp)
import Magento.Module (codeRootPath)
import Util (
    writeFileAndPrint,
    capitalize,
    capitalizePath,
    lowercase)
import Data.String.Utils (replace, join)
import Util.XML (insertXmlIfMissing)


addController :: FilePath -> String -> String -> String -> String -> IO ()
addController configXmlPath namespace moduleName scope controllerName = do
    insertControllerXmlIfMissing
        configXmlPath namespace moduleName scope
    createControllerPhpIfMissing
        configXmlPath namespace moduleName scope controllerName

insertControllerXmlIfMissing :: FilePath -> String -> String -> String -> IO ()
insertControllerXmlIfMissing configXmlPath namespace moduleName scope = do
    xml <- controllerXml
        (lowercase moduleName)
        (fullModuleName namespace moduleName)
        (router scope)
    insertXmlIfMissing configXmlPath (xpath scope) xml

xpath :: String -> String
xpath "admin" = "/config/admin/routers"
xpath "frontend" = "/config/frontend/routers"

router :: String -> String
router "admin" = "admin"
router "frontend" = "standard"

fullModuleName :: String -> String -> String
fullModuleName namespace moduleName = join "_" [namespace, moduleName]

composeControllerPhpPath :: FilePath -> String -> String
composeControllerPhpPath configXmlPath controllerName =
    joinPath [
        codeRootPath configXmlPath,
        "controllers",
        (capitalizePath controllerName) ++ ".php"
    ]

createControllerPhpIfMissing :: FilePath -> String -> String -> String -> String -> IO ()
createControllerPhpIfMissing configXmlPath namespace moduleName scope controllerName =
    let controllerPhpPath = composeControllerPhpPath configXmlPath controllerName
    in do
        createDirectoryIfMissing True (takeDirectory controllerPhpPath)
        writeControllerPhpIfMissing
            controllerPhpPath namespace moduleName scope controllerName

writeControllerPhpIfMissing :: FilePath -> String -> String -> String -> String -> IO ()
writeControllerPhpIfMissing path namespace moduleName scope controllerName = do
    exists <- doesFileExist path
    when (not exists) $
        writeControllerPhp path namespace moduleName scope controllerName

writeControllerPhp :: FilePath -> String -> String -> String -> String -> IO ()
writeControllerPhp path namespace moduleName scope controllerName = do
    php <- controllerPhp
        (className namespace moduleName controllerName)
        (parentClassName scope)
    writeFileAndPrint path php

classNamePrefix :: String -> String -> String
classNamePrefix namespace moduleName =
    join "_" [
        capitalize namespace,
        capitalize moduleName
    ]

classNameFromPath :: String -> String
classNameFromPath path = replace "/" "_" $ capitalizePath path

className :: String -> String -> String -> String
className namespace moduleName controllerName =
    join "_" [
        classNamePrefix namespace moduleName,
        (classNameFromPath controllerName) ++ "Controller"
    ]

parentClassName :: String -> String
parentClassName "frontend" = "Mage_Core_Controller_Front_Action"
parentClassName "admin" = "Mage_Adminhtml_Controller_Action"
