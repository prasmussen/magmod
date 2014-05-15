module Magento.Observer (
    addObserver
) where

import System.FilePath.Posix (joinPath)
import Template.Observer (observerXml, observerPhp)
import Magento.Model (addModel)
import Magento.Module.Path (modelBasePath)
import Magento.Module (
    ModuleInfo,
    getName,
    getNamespace,
    getConfigXml,
    getFullName)
import Util (camelcase, lowercase)
import Util.XML (insertXmlIfMissing)
import Util.PHP (insertPhpMethod)
import Data.String.Utils (join)

addObserver :: ModuleInfo -> String -> String -> IO ()
addObserver info scope eventName = do
    insertObserverXmlIfMissing info scope eventName
    createObserverPhpIfMissing info eventName

insertObserverXmlIfMissing :: ModuleInfo -> String -> String -> IO ()
insertObserverXmlIfMissing info scope eventName = do
    xml <- observerXml
        (lowercase $ getName info) eventName (methodName eventName)
    insertXmlIfMissing (getConfigXml info) (xpath scope eventName) xml

createObserverPhpIfMissing :: ModuleInfo -> String -> IO ()
createObserverPhpIfMissing info eventName = do
    -- Add observer model
    addModel info "Observer"
    insertObserverPhp (observerPath info) eventName

insertObserverPhp :: FilePath -> String -> IO ()
insertObserverPhp path eventName = do
    php <- observerPhp $ methodName eventName
    insertPhpMethod path php

xpath :: String -> String -> String
xpath scope eventName =
    join "" ["/config/", scopeName scope, "/events/", eventName]

methodName :: String -> String
methodName eventName = camelcase eventName

scopeName :: String -> String
scopeName "frontend" = "frontend"
scopeName "admin" = "adminhtml"
scopeName "global" = "global"

observerPath :: ModuleInfo -> String
observerPath info =
    joinPath [
        modelBasePath info,
        "Observer.php"
    ]
