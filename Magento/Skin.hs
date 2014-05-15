module Magento.Skin (
    addSkin
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Data.String.Utils (replace)
import Magento.Module.Path (skinBasePath)
import Magento.Module (
    ModuleInfo,
    getName)
import Util (lowercase, writeFileAndPrint)
import Util.XML (insertXmlIfMissing)


addSkin :: ModuleInfo -> String -> String -> IO ()
addSkin info scope name = do
    createSkinIfMissing info scope name

createSkinIfMissing :: ModuleInfo -> String -> String -> IO ()
createSkinIfMissing info scope name =
    let path = skinPath info scope name in do
        createDirectoryIfMissing True (takeDirectory path)
        writeSkinIfMissing path

writeSkinIfMissing :: FilePath -> IO ()
writeSkinIfMissing path = do
    exists <- doesFileExist path
    when (not exists) $ writeSkin path

writeSkin :: FilePath -> IO ()
writeSkin path = do
    writeFileAndPrint path ""

scopeName :: String -> String
scopeName "frontend" = "frontend"
scopeName "admin" = "adminhtml"

skinPath :: ModuleInfo -> String -> String -> FilePath
skinPath info scope name =
    joinPath [
        skinBasePath info (scopeName scope),
        lowercase $ getName info,
        (lowercase name)
    ]
