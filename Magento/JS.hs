module Magento.JS (
    addJs
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Data.String.Utils (replace)
import Magento.Module (
    ModuleInfo,
    jsBasePath,
    getName)
import Util (lowercase, writeFileAndPrint)


addJs :: ModuleInfo -> String -> IO ()
addJs info name = do
    createJsIfMissing info name

createJsIfMissing :: ModuleInfo -> String -> IO ()
createJsIfMissing info name =
    let path = jsPath info name in do
        createDirectoryIfMissing True (takeDirectory path)
        writeJsIfMissing path

writeJsIfMissing :: FilePath -> IO ()
writeJsIfMissing path = do
    exists <- doesFileExist path
    when (not exists) $ writeJs path

writeJs :: FilePath -> IO ()
writeJs path = do
    writeFileAndPrint path ""

ensureJsExt :: String -> String
ensureJsExt str = (replace ".js" "" str) ++ ".js"

jsPath :: ModuleInfo -> String -> FilePath
jsPath info name =
    joinPath [
        jsBasePath info,
        lowercase $ getName info,
        ensureJsExt $ lowercase name
    ]
