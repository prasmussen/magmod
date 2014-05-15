module Magento.Module (
    ModuleInfo,
    moduleInfo,
    getConfigXml,
    getNamespace,
    getName,
    newModule,
    findConfigXml,
    findSetupFiles,
    getFullName,
    basePath,
    jsBasePath,
    skinBasePath,
    templateBasePath,
    controllersBasePath,
    blockBasePath,
    modelBasePath,
    helperBasePath,
    layoutBasePath,
    localeBasePath,
    resourceBasePath,
    setupBasePath,
    codeRootPath
) where

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath.Posix (joinPath, takeDirectory, takeBaseName)
import Data.Functor ((<$>))
import System.FilePath.Glob (globDir, compile)
import Template.Module (moduleXml, configXml)
import Util (capitalize, lowercase, ensureSinglePath, writeFileAndPrint)
import Data.String.Utils (join)

type ModuleInfo = ModuleInfoData

data ModuleInfoData = ModuleInfo {
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
    ensureSinglePath . concat . fst <$>
        globDir [compile "**/config.xml"] "."

findSetupFiles :: IO [String]
findSetupFiles =
    (map takeBaseName) . concat . fst <$>
        globDir (map compile ["**/install-*.php", "**/upgrade-*.php"]) "."

codeRootPath :: ModuleInfo -> FilePath
codeRootPath info =
    joinPath [takeDirectory $ getConfigXml info, ".."]

basePath :: ModuleInfo -> FilePath
basePath info =
    joinPath $ (takeDirectory $ getConfigXml info):(replicate 6 "..")

jsBasePath :: ModuleInfo -> FilePath
jsBasePath info = joinPath [basePath info, "js"]

skinBasePath :: ModuleInfo -> String -> FilePath
skinBasePath info "frontend" = skinBasePath' info "frontend"
skinBasePath info "adminhtml" = skinBasePath' info "adminhtml"

skinBasePath' :: ModuleInfo -> String -> FilePath
skinBasePath' info scope =
    joinPath [
        basePath info,
        "skin",
        scope,
        "base",
        "default"
    ]

templateBasePath :: ModuleInfo -> String -> FilePath
templateBasePath info "frontend" = templateBasePath' info "frontend"
templateBasePath info "adminhtml" = templateBasePath' info "adminhtml"

templateBasePath' :: ModuleInfo -> String -> FilePath
templateBasePath' info scope =
    joinPath [
        basePath info,
        "app",
        "design",
        scope,
        "base",
        "default",
        "template"
    ]

blockBasePath :: ModuleInfo -> FilePath
blockBasePath info =
    joinPath [
        codeRootPath info,
        "Block"
    ]

helperBasePath :: ModuleInfo -> FilePath
helperBasePath info =
    joinPath [
        codeRootPath info,
        "Helper"
    ]

modelBasePath :: ModuleInfo -> String
modelBasePath info =
    joinPath [
        codeRootPath info,
        "Model"
    ]

resourceBasePath :: ModuleInfo -> String
resourceBasePath info =
    joinPath [
        modelBasePath info,
        "Resource"
    ]

setupBasePath :: ModuleInfo -> String
setupBasePath info =
    joinPath [
        codeRootPath info,
        "sql"
    ]

controllersBasePath :: ModuleInfo -> FilePath
controllersBasePath info =
    joinPath [
        codeRootPath info,
        "controllers"
    ]

layoutBasePath :: ModuleInfo -> String -> FilePath
layoutBasePath info "frontend" = layoutBasePath' info "frontend"
layoutBasePath info "adminhtml" = layoutBasePath' info "adminhtml"

layoutBasePath' :: ModuleInfo -> String -> FilePath
layoutBasePath' info scope =
    joinPath [
        basePath info,
        "app",
        "design",
        scope,
        "base",
        "default",
        "layout"
    ]

localeBasePath :: ModuleInfo -> String
localeBasePath info =
    joinPath [
        basePath info,
        "app",
        "locale"
    ]

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
    ModuleInfo{
        getConfigXml=configXmlPath,
        getNamespace=namespace,
        getName=name
    }


fullName :: String -> String -> String
fullName namespace name = join "_" [capitalize namespace, capitalize name]

getFullName :: ModuleInfo -> String
getFullName info = fullName (getNamespace info) (getName info)
