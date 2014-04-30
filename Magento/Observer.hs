module Magento.Observer (
    addObserver
) where

import System.FilePath.Posix (joinPath)
import Template.Observer (observerXml, observerPhp)
import Magento.Model (addModel)
import Magento.Module (codeRootPath)
import Util (camelcase, lowercase)
import Util.XML (insertXmlIfMissing)
import Util.PHP (insertPhpMethod)
import Data.String.Utils (join)

addObserver :: FilePath -> String -> String -> String -> String -> IO ()
addObserver configXmlPath namespace moduleName scope eventName = do
    insertObserverXmlIfMissing configXmlPath moduleName scope eventName
    createObserverPhpIfMissing configXmlPath namespace moduleName eventName

insertObserverXmlIfMissing :: FilePath -> String -> String -> String -> IO ()
insertObserverXmlIfMissing configXmlPath moduleName scope eventName = do
    xml <- observerXml
        (lowercase moduleName) eventName (composeMethodName eventName)
    insertXmlIfMissing configXmlPath (xpath scope eventName) xml

xpath :: String -> String -> String
xpath scope eventName =
    join "" ["/config/", scopeName scope, "/events/", eventName]

composeMethodName :: String -> String
composeMethodName eventName = camelcase eventName

scopeName :: String -> String
scopeName "frontend" = "frontend"
scopeName "admin" = "adminhtml"
scopeName "global" = "global"

observerPath :: FilePath -> String
observerPath configXmlPath =
    joinPath [
        codeRootPath configXmlPath,
        "Model",
        "Observer.php"
    ]

createObserverPhpIfMissing :: FilePath -> String -> String -> String -> IO ()
createObserverPhpIfMissing configXmlPath namespace moduleName eventName = do
    -- Add observer model
    addModel configXmlPath namespace moduleName "Observer"
    insertObserverPhp
        (observerPath configXmlPath)
        (composeMethodName eventName)

insertObserverPhp :: FilePath -> String -> IO ()
insertObserverPhp path methodName = do
    php <- observerPhp methodName
    insertPhpMethod path php
