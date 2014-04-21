module Magento.Observer (
    addObserver
) where

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory, takeBaseName)
import Template.Observer (observerXml, observerPhp)
import Magento.Module (codeRootPath)
import Util (capitalize,
    capitalizePath,
    lowercase,
    camelcase,
    tmpFname,
    renameWithBackup)
import Util.XML (insertXmlIfMissing)
import Data.String.Utils (replace, join)

addObserver :: FilePath -> String -> String -> String -> String -> IO ()
addObserver configXmlPath namespace moduleName scope eventName = do
    insertObserverXmlIfMissing configXmlPath moduleName scope eventName
    --createObserverPhpIfMissing configXmlPath namespace moduleName observerName

insertObserverXmlIfMissing :: FilePath -> String -> String -> String -> IO ()
insertObserverXmlIfMissing configXmlPath moduleName scope eventName = do
    let xpath = join "" ["/config/", scope, "/events/", eventName]
    xml <- (observerXml moduleName eventName (methodName eventName))
    insertXmlIfMissing configXmlPath xpath xml

methodName :: String -> String
methodName eventName = camelcase eventName

composeObserverPhpPath :: FilePath -> String -> String
composeObserverPhpPath configXmlPath observerName =
    joinPath [
        codeRootPath configXmlPath,
        "Observer",
        (capitalizePath observerName) ++ ".php"
    ]

createObserverPhpIfMissing :: FilePath -> String -> String -> String -> IO ()
createObserverPhpIfMissing configXmlPath namespace moduleName observerName =
    let observerPhpPath = composeObserverPhpPath configXmlPath observerName
    in do
        createDirectoryIfMissing True (takeDirectory observerPhpPath)
        writeObserverPhpIfMissing observerPhpPath namespace moduleName observerName

writeObserverPhpIfMissing :: FilePath -> String -> String -> String -> IO ()
writeObserverPhpIfMissing path namespace moduleName observerName = do
    exists <- doesFileExist path
    case exists of
        False -> writeObserverPhp path namespace moduleName observerName
        True -> return ()

writeObserverPhp :: FilePath -> String -> String -> String -> IO ()
writeObserverPhp path namespace moduleName observerName = do
    php <- observerPhp $ className namespace moduleName observerName
    writeFile path php

classNamePrefix :: String -> String -> String
classNamePrefix namespace moduleName =
    foldl (++) "" [
        capitalize namespace,
        "_",
        capitalize moduleName,
        "_",
        "Observer"
    ]

className :: String -> String -> String -> String
className namespace moduleName observerName =
    foldl (++) "" [
        classNamePrefix namespace moduleName,
        "_",
        replace "/" "_" $ capitalizePath observerName
    ]
