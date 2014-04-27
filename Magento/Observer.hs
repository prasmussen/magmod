module Magento.Observer (
    addObserver
) where

import System.FilePath.Posix (joinPath)
import Template.Observer (observerXml, observerPhp)
import Magento.Model (addModel)
import Magento.Module (codeRootPath)
import Util (camelcase)
import Util.XML (insertXmlIfMissing)
import Util.PHP (insertPhpMethod)
import Data.String.Utils (join)

addObserver :: FilePath -> String -> String -> String -> String -> IO ()
addObserver configXmlPath namespace moduleName scope eventName = do
    insertObserverXmlIfMissing configXmlPath moduleName scope eventName
    createObserverPhpIfMissing configXmlPath namespace moduleName eventName

insertObserverXmlIfMissing :: FilePath -> String -> String -> String -> IO ()
insertObserverXmlIfMissing configXmlPath moduleName scope eventName = do
    let xpath = join "" ["/config/", scopeName scope, "/events/", eventName]
    xml <- (observerXml moduleName eventName (composeMethodName eventName))
    insertXmlIfMissing configXmlPath xpath xml

composeMethodName :: String -> String
composeMethodName eventName = camelcase eventName

scopeName :: String -> String
scopeName "frontend" = "frontend"
scopeName "admin" = "adminhtml"
scopeName "global" = "global"

composeObserverPhpPath :: FilePath -> String
composeObserverPhpPath configXmlPath =
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
        (composeObserverPhpPath configXmlPath)
        (composeMethodName eventName)

insertObserverPhp :: FilePath -> String -> IO ()
insertObserverPhp path methodName = do
    php <- observerPhp methodName
    insertPhpMethod path php
