module Magento.Layout (
    addLayout
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Template.Layout (layoutXml, layoutConfigXml)
import Magento.Module (basePath)
import Util (lowercase)
import Util.XML (insertXmlIfMissing)


addLayout :: FilePath -> String -> String -> IO ()
addLayout configXmlPath moduleName scope = do
    insertLayoutConfigXmlIfMissing configXmlPath moduleName scope
    createLayoutXmlIfMissing configXmlPath moduleName scope

insertLayoutConfigXmlIfMissing :: FilePath -> String -> String -> IO ()
insertLayoutConfigXmlIfMissing configXmlPath moduleName scope = do
    xml <- layoutConfigXml (lowercase moduleName)
    insertXmlIfMissing configXmlPath (xpath scope) xml

createLayoutXmlIfMissing :: FilePath -> String -> String -> IO ()
createLayoutXmlIfMissing configXmlPath moduleName scope =
    let layoutXmlPath = composeLayoutXmlPath configXmlPath moduleName scope
    in do
        createDirectoryIfMissing True (takeDirectory layoutXmlPath)
        writeLayoutXmlIfMissing layoutXmlPath

writeLayoutXmlIfMissing :: FilePath -> IO ()
writeLayoutXmlIfMissing path = do
    exists <- doesFileExist path
    when (not exists) $ writeLayoutXml path

writeLayoutXml :: FilePath -> IO ()
writeLayoutXml path = do
    xml <- layoutXml
    writeFile path xml

xpath :: String -> String
xpath "admin" = "/config/adminhtml/layout"
xpath "frontend" = "/config/frontend/layout"

scopeName :: String -> String
scopeName "frontend" = "frontend"
scopeName "admin" = "adminhtml"

composeLayoutXmlPath :: FilePath -> String -> String -> FilePath
composeLayoutXmlPath configXmlPath moduleName scope =
    joinPath [
        basePath configXmlPath,
        "app",
        "design",
        scopeName scope,
        "base",
        "default",
        "layout",
        (lowercase moduleName) ++ ".xml"
    ]