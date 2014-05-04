module Magento.Setup (
    addInstall,
    addUpgrade
) where

import Data.Functor ((<$>))
import Data.List (isPrefixOf)
import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Template.Setup (setupXml, installPhp, upgradePhp)
import Magento.Module.XML (readVersion, readEntities)
import Magento.Module.Version (
    Version,
    incrementMinor,
    parseVersion,
    parseVersionRange,
    showVersion)
import Magento.Module (
    ModuleInfo,
    findSetupFiles,
    codeRootPath,
    getFullName,
    getName,
    getNamespace,
    getConfigXml)
import Util (
    writeFileAndPrint,
    capitalize,
    capitalizePath,
    lowercase)
import Data.String.Utils (replace, join)
import Util.XML (insertXmlIfMissing)


addInstall :: ModuleInfo -> IO ()
addInstall info = do
    insertSetupXmlIfMissing info
    version <- readVersion (getConfigXml info)
    createInstallPhpIfMissing info (incrementMinor version)

addUpgrade :: ModuleInfo -> IO ()
addUpgrade info = do
    versions <- map parseFname <$> findSetupFiles
    current <- readVersion (getConfigXml info)
    case (length versions) of
        0 -> putStrLn "An install script must be created first"
        _ ->
            createUpgradePhpIfMissing
                info (latest versions, incrementMinor current)

insertSetupXmlIfMissing :: ModuleInfo -> IO ()
insertSetupXmlIfMissing info = do
    xml <- setupXml (setupIdentifier info) (getFullName info)
    insertXmlIfMissing (getConfigXml info) "/config/global/resources" xml

createInstallPhpIfMissing :: ModuleInfo -> Version -> IO ()
createInstallPhpIfMissing info version =
    let path = installPath info version in do
        createDirectoryIfMissing True (takeDirectory path)
        writeInstallPhpIfMissing info path

writeInstallPhpIfMissing :: ModuleInfo -> FilePath -> IO ()
writeInstallPhpIfMissing info path = do
    exists <- doesFileExist path
    when (not exists) $ writeInstallPhp info path

writeInstallPhp :: ModuleInfo -> FilePath -> IO ()
writeInstallPhp info path = do
    entities <- readEntities (getConfigXml info)
    php <- installPhp (lowercase $ getName info) entities
    writeFileAndPrint path php

createUpgradePhpIfMissing :: ModuleInfo -> (Version, Version) -> IO ()
createUpgradePhpIfMissing info versionRange =
    let path = upgradePath info versionRange in do
        createDirectoryIfMissing True (takeDirectory path)
        writeUpgradePhpIfMissing info path

writeUpgradePhpIfMissing :: ModuleInfo -> FilePath -> IO ()
writeUpgradePhpIfMissing info path = do
    exists <- doesFileExist path
    when (not exists) $ writeUpgradePhp info path

writeUpgradePhp :: ModuleInfo -> FilePath -> IO ()
writeUpgradePhp info path = do
    entities <- readEntities (getConfigXml info)
    php <- upgradePhp (lowercase $ getName info) entities
    writeFileAndPrint path php

setupPath :: ModuleInfo -> String
setupPath info =
    joinPath [
        codeRootPath info,
        "sql",
        setupIdentifier info
    ]

installPath :: ModuleInfo -> Version -> String
installPath info vsn =
    joinPath [
        setupPath info,
        join "" ["install-", showVersion vsn, ".php"]
    ]

upgradePath :: ModuleInfo -> (Version, Version) -> String
upgradePath info (vsn, vsn') =
    joinPath [
        setupPath info,
        join "" ["upgrade-", showVersion vsn, "-", showVersion vsn', ".php"]
    ]

setupIdentifier :: ModuleInfo -> String
setupIdentifier info = join "_" [lowercase $ getName info, "setup"]

parseFname :: String -> Version
parseFname str
    | isPrefixOf "install" str = parseVersion str
    | isPrefixOf "upgrade" str = snd $ parseVersionRange str
    | otherwise = parseVersion "0.0.0"

latest :: [Version] -> Version
latest versions = foldl max (parseVersion "0.0.0") versions
