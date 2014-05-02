module Magento.Module (
    ModuleInfo,
    moduleInfo,
    getConfigXml,
    getNamespace,
    getName,
    newModule,
    findConfigXml,
    getFullName,
    basePath,
    codeRootPath
) where

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath.Posix (joinPath, takeDirectory)
import Data.Functor ((<$>))
import System.FilePath.Find (find, always, fileName, (==?))
import Template.Module (moduleXml, configXml)
import Util (capitalize, lowercase, ensureSinglePath, writeFileAndPrint)
import Data.String.Utils (join)

type ModuleInfo = ModuleInfoData

data ModuleInfoData = ModuleInfoData {
    getConfigXml :: String,
    getName :: String,
    getNamespace :: String
}

newModule :: String -> String -> String -> IO ()
newModule codepool namespace name = do
    exists <- doesDirectoryExist $ lowercase name
    case exists of
        True ->
            putStrLn "A directory with that name already exists"
        False -> do
            createModuleXml codepool namespace name
            createConfigXml codepool namespace name

findConfigXml :: IO (Maybe FilePath)
findConfigXml =
    ensureSinglePath <$> find always (fileName ==? "config.xml") "."

codeRootPath :: ModuleInfo -> FilePath
codeRootPath info =
    joinPath [takeDirectory $ getConfigXml info, ".."]

basePath :: ModuleInfo -> FilePath
basePath info =
    joinPath $ (takeDirectory $ getConfigXml info):(replicate 6 "..")

moduleXmlFname :: String -> String -> String
moduleXmlFname namespace name =
    join "_" [
        capitalize namespace,
        (capitalize name) ++ ".xml"
    ]

composeModuleXmlPath :: String -> String -> String
composeModuleXmlPath namespace name =
    joinPath [
        lowercase name,
        "app",
        "etc",
        "modules",
        moduleXmlFname namespace name
    ]

composeConfigXmlPath :: String -> String -> String -> String
composeConfigXmlPath codepool namespace name =
    joinPath [
        lowercase name,
        "app",
        "code",
        lowercase codepool,
        capitalize namespace,
        capitalize name,
        "etc",
        "config.xml"
    ]

writeModuleXml :: String -> String -> String -> String -> IO ()
writeModuleXml path codepool namespace name = do
    xml <- moduleXml (lowercase codepool) (fullName namespace name)
    writeFileAndPrint path xml

writeConfigXml :: String -> String -> String -> IO ()
writeConfigXml path namespace name = do
    xml <- configXml (fullName namespace name)
    writeFileAndPrint path xml

createModuleXml :: String -> String -> String -> IO ()
createModuleXml codepool namespace name =
    let moduleXmlPath = composeModuleXmlPath namespace name
    in do
        createDirectoryIfMissing True (takeDirectory moduleXmlPath)
        writeModuleXml moduleXmlPath codepool namespace name

createConfigXml :: String -> String -> String -> IO ()
createConfigXml codepool namespace name =
    let configXmlPath = composeConfigXmlPath codepool namespace name
    in do
        createDirectoryIfMissing True (takeDirectory configXmlPath)
        writeConfigXml configXmlPath namespace name

moduleInfo :: FilePath -> String -> String -> ModuleInfo
moduleInfo configXmlPath namespace name =
    ModuleInfoData{
        getConfigXml=configXmlPath,
        getNamespace=namespace,
        getName=name
    }


fullName :: String -> String -> String
fullName namespace name = join "_" [capitalize namespace, capitalize name]

getFullName :: ModuleInfo -> String
getFullName info = fullName (getNamespace info) (getName info)
