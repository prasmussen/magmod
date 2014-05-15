module Magento.Controller (
    addController,
    parentClassName
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Template.Controller (controllerXml, controllerPhp)
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


addController :: ModuleInfo -> String -> String -> IO ()
addController info scope controllerName = do
    insertCtrlXmlIfMissing info scope
    createCtrlPhpIfMissing info scope controllerName

insertCtrlXmlIfMissing :: ModuleInfo -> String -> IO ()
insertCtrlXmlIfMissing info scope = do
    xml <- controllerXml
        (lowercase $ getName info) (getFullName info) (router scope)
    insertXmlIfMissing (getConfigXml info) (xpath scope) xml

createCtrlPhpIfMissing :: ModuleInfo -> String -> String -> IO ()
createCtrlPhpIfMissing info scope controllerName =
    let path = controllerPath info controllerName in do
        createDirectoryIfMissing True (takeDirectory path)
        writeCtrlPhpIfMissing info path scope controllerName

writeCtrlPhpIfMissing :: ModuleInfo -> FilePath -> String -> String -> IO ()
writeCtrlPhpIfMissing info path scope controllerName = do
    exists <- doesFileExist path
    when (not exists) $ writeCtrlPhp info path scope controllerName

writeCtrlPhp :: ModuleInfo -> FilePath -> String -> String -> IO ()
writeCtrlPhp info path scope controllerName = do
    php <- controllerPhp
        (className info controllerName) (parentClassName scope)
    writeFileAndPrint path php

xpath :: String -> String
xpath "admin" = "/config/admin/routers"
xpath "frontend" = "/config/frontend/routers"

router :: String -> String
router "admin" = "admin"
router "frontend" = "standard"

controllerPath :: ModuleInfo -> String -> String
controllerPath info controllerName =
    joinPath [
        codeRootPath info,
        "controllers",
        (capitalizePath controllerName) ++ "Controller.php"
    ]

classNameFromPath :: String -> String
classNameFromPath path = replace "/" "_" $ capitalizePath path

classNamePrefix :: ModuleInfo -> String
classNamePrefix info =
    join "_" [
        capitalize $ getNamespace info,
        capitalize $ getName info
    ]

className :: ModuleInfo -> String -> String
className info controllerName =
    join "_" [
        classNamePrefix info,
        (classNameFromPath controllerName) ++ "Controller"
    ]

parentClassName :: String -> String
parentClassName "frontend" = "Mage_Core_Controller_Front_Action"
parentClassName "admin" = "Mage_Adminhtml_Controller_Action"
