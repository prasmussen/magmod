module Magento.Locale (
    addLocale
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Template.Locale (localeXml)
import Magento.Module (
    ModuleInfo,
    localeBasePath,
    getName,
    getConfigXml,
    getFullName)
import Util (
    writeFileAndPrint,
    capitalize,
    capitalizePath,
    lowercase)
import Data.String.Utils (join)
import Util.XML (insertXmlIfMissing)


addLocale :: ModuleInfo -> String -> String -> IO ()
addLocale info scope localeName = do
    insertLocaleXmlIfMissing info scope
    createLocaleCsvIfMissing info localeName

insertLocaleXmlIfMissing :: ModuleInfo -> String -> IO ()
insertLocaleXmlIfMissing info scope = do
    xml <- localeXml (lowercase $ getFullName info) (localeFname info)
    insertXmlIfMissing (getConfigXml info) (xpath scope) xml

createLocaleCsvIfMissing :: ModuleInfo -> String -> IO ()
createLocaleCsvIfMissing info localeName =
    let path = localePath info localeName in do
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

localeFname :: ModuleInfo -> String
localeFname info =
    join "" [getFullName info, ".csv"]

localePath :: ModuleInfo -> String -> String
localePath info localeName =
    joinPath [
        localeBasePath info,
        localeName,
        localeFname info
    ]
