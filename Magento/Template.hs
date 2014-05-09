module Magento.Template (
    addTemplate
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Data.String.Utils (replace)
import Magento.Module (
    ModuleInfo,
    basePath,
    getName)
import Util (lowercase, writeFileAndPrint)
import Util.XML (insertXmlIfMissing)


addTemplate :: ModuleInfo -> String -> String -> IO ()
addTemplate info scope name = do
    createTemplateIfMissing info scope name

createTemplateIfMissing :: ModuleInfo -> String -> String -> IO ()
createTemplateIfMissing info scope name =
    let path = templatePath info scope name in do
        createDirectoryIfMissing True (takeDirectory path)
        writeTemplateIfMissing path

writeTemplateIfMissing :: FilePath -> IO ()
writeTemplateIfMissing path = do
    exists <- doesFileExist path
    when (not exists) $ writeTemplate path

writeTemplate :: FilePath -> IO ()
writeTemplate path = do
    writeFileAndPrint path ""

scopeName :: String -> String
scopeName "frontend" = "frontend"
scopeName "admin" = "adminhtml"

stripPhtml :: String -> String
stripPhtml str = replace ".phtml" "" str

templatePath :: ModuleInfo -> String -> String -> FilePath
templatePath info scope name =
    joinPath [
        basePath info,
        "app",
        "design",
        scopeName scope,
        "base",
        "default",
        "template",
        lowercase $ getName info,
        (stripPhtml . lowercase $ name) ++ ".phtml"
    ]
