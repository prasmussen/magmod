module Magento.Layout (
    addLayout
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Template.Layout (layoutXml, layoutConfigXml)
import Magento.Module.Path (layoutBasePath)
import Magento.Module (
    ModuleInfo,
    getName,
    getNamespace,
    getConfigXml,
    getFullName)
import Util (lowercase, writeFileAndPrint)
import Util.XML (insertXmlIfMissing)


addLayout :: ModuleInfo -> String -> IO ()
addLayout info scope = do
    insertLayoutConfigXmlIfMissing info scope
    createLayoutXmlIfMissing info scope

insertLayoutConfigXmlIfMissing :: ModuleInfo -> String -> IO ()
insertLayoutConfigXmlIfMissing info scope = do
    xml <- layoutConfigXml (lowercase $ getName info)
    insertXmlIfMissing (getConfigXml info) (xpath scope) xml

createLayoutXmlIfMissing :: ModuleInfo -> String -> IO ()
createLayoutXmlIfMissing info scope =
    let path = layoutPath info scope in do
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

layoutPath :: ModuleInfo -> String -> FilePath
layoutPath info scope =
    joinPath [
        layoutBasePath info (scopeName scope),
        (lowercase $ getName info) ++ ".xml"
    ]
