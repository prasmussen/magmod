module Magento.Locale (
    addLocale
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Template.Locale (localeXml)
import Magento.Module (basePath, codeRootPath, fullModuleName)
import Util (
    writeFileAndPrint,
    capitalize,
    capitalizePath,
    lowercase)
import Data.String.Utils (join)
import Util.XML (insertXmlIfMissing)


addLocale :: FilePath -> String -> String -> String -> String -> IO ()
addLocale configXmlPath namespace moduleName scope localeName = do
    insertLocaleXmlIfMissing configXmlPath namespace moduleName scope
    createLocaleCsvIfMissing configXmlPath namespace moduleName localeName

insertLocaleXmlIfMissing :: FilePath -> String -> String -> String -> IO ()
insertLocaleXmlIfMissing configXmlPath namespace moduleName scope = do
    xml <- localeXml
        (lowercase $ fullModuleName namespace moduleName)
        (localeFname namespace moduleName)
    insertXmlIfMissing configXmlPath (xpath scope) xml

createLocaleCsvIfMissing :: FilePath -> String -> String -> String -> IO ()
createLocaleCsvIfMissing configXmlPath namespace moduleName localeName =
    let path = localePath configXmlPath namespace moduleName localeName
    in do
        createDirectoryIfMissing True (takeDirectory path)
        writeLocaleCsvIfMissing path

writeLocaleCsvIfMissing :: FilePath -> IO ()
writeLocaleCsvIfMissing path = do
    exists <- doesFileExist path
    when (not exists) $ writeFileAndPrint path ""

xpath :: String -> String
xpath scope = join "" ["/config/", scopeName scope, "/translate"]

scopeName :: String -> String
scopeName "frontend" = "frontend"
scopeName "admin" = "adminhtml"

localeFname :: String -> String -> String
localeFname namespace moduleName =
    join "" [fullModuleName namespace moduleName, ".csv"]

localePath :: FilePath -> String -> String -> String -> String
localePath configXmlPath namespace moduleName localeName =
    joinPath [
        basePath configXmlPath,
        "app",
        "locale",
        localeName,
        localeFname namespace moduleName
    ]
