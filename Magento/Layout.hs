module Magento.Layout (
    addLayout
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Template.Layout (layoutXml, layoutConfigXml)
import Magento.Module (basePath)
import Util (lowercase, writeFileAndPrint)
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
    let path = layoutPath configXmlPath moduleName scope
    in do
        createDirectoryIfMissing True (takeDirectory path)
        writeLayoutXmlIfMissing path

writeLayoutXmlIfMissing :: FilePath -> IO ()
writeLayoutXmlIfMissing path = do
    exists <- doesFileExist path
    when (not exists) $ writeLayoutXml path

writeLayoutXml :: FilePath -> IO ()
writeLayoutXml path = do
    xml <- layoutXml
    writeFileAndPrint path xml

xpath :: String -> String
xpath "admin" = "/config/adminhtml/layout"
xpath "frontend" = "/config/frontend/layout"

scopeName :: String -> String
scopeName "frontend" = "frontend"
scopeName "admin" = "adminhtml"

layoutPath :: FilePath -> String -> String -> FilePath
layoutPath configXmlPath moduleName scope =
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
