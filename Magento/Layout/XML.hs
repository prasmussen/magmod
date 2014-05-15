module Magento.Layout.XML (
    generateLayout
) where

import Data.Functor ((<$>))
import Data.List (intersect)
import System.FilePath.Posix (makeRelative, pathSeparator)
import System.FilePath.Glob (globDir, compile)
import System.FilePath.Posix (
    joinPath,
    dropExtension,
    splitDirectories)
import Template.Layout (genXml)
import Magento.Controller (parentClassName)
import Magento.Module (
    ModuleInfo,
    basePath,
    jsBasePath,
    skinBasePath,
    templateBasePath,
    blockBasePath,
    controllersBasePath,
    getName,
    getNamespace,
    getConfigXml,
    getFullName)
import Util (lowercase)
import Util.XML (printXml)
import Util.PHP (
    PhpClass,
    readClass,
    getClassName,
    getParentName,
    getPublicFunctions)
import Data.String.Utils (join, split, replace)


generateLayout :: ModuleInfo -> String -> IO ()
generateLayout info scope = do
    controllers <- collectControllers info scope
    stylesheets <- collectStylesheets info scope
    skinJavascripts <- collectSkinJavascripts info scope
    javascripts <- collectJavascripts info
    blocks <- collectBlocks info scope
    xml <- genXml controllers stylesheets skinJavascripts javascripts blocks
    printXml xml

collectControllers :: ModuleInfo -> String -> IO [String]
collectControllers info scope =
    let basePath = controllersBasePath info in do
    files <- findPhpFiles basePath
    readHandles info scope $ map (\x -> joinPath [basePath, x]) files

collectStylesheets :: ModuleInfo -> String -> IO [FilePath]
collectStylesheets info scope =
    findCssFiles (skinBasePath info $ scopeName scope)

collectSkinJavascripts :: ModuleInfo -> String -> IO [FilePath]
collectSkinJavascripts info scope =
    findJsFiles (skinBasePath info $ scopeName scope)

collectJavascripts :: ModuleInfo -> IO [FilePath]
collectJavascripts info = findJsFiles (jsBasePath info)

collectBlocks :: ModuleInfo -> String -> IO [(String, String, String)]
collectBlocks info scope = do
    blocks <- findPhpFiles (blockBasePath info)
    templates <- findPhtmlFiles (templateBasePath info $ scopeName scope)
    return $ map blockTuple
        (intersectingPaths (prefixModuleName info blocks) templates)

blockTuple :: FilePath -> (String, String, String)
blockTuple path = (blockClassPath path, templatePath path, blockName path)

readHandles :: ModuleInfo -> String -> [FilePath] -> IO [String]
readHandles info scope paths = do
    classes <- mapM readClass paths
    return $ concat $ map (handles info) (filterByScope scope classes)

handles :: ModuleInfo -> PhpClass -> [String]
handles info cls = map (handle info cls) (getPublicFunctions cls)

handle :: ModuleInfo -> PhpClass -> String -> String
handle info cls fn =
    lowercase $ join "_" [
        getName info,
        replace "Controller" "" $ last $ split "_" $ getClassName cls,
        replace "Action()" "" fn
    ]

filterByScope :: String -> [PhpClass] -> [PhpClass]
filterByScope scope classes =
    filter (\x -> getParentName x == parentClassName scope) classes

findJsFiles :: FilePath -> IO [FilePath]
findJsFiles path = findFiles path ["**/*.js"]

findCssFiles :: FilePath -> IO [FilePath]
findCssFiles path = findFiles path ["**/*.css"]

findPhpFiles :: FilePath -> IO [FilePath]
findPhpFiles path = findFiles path ["**/*.php"]

findPhtmlFiles :: FilePath -> IO [FilePath]
findPhtmlFiles path = findFiles path ["**/*.phtml"]

findFiles :: FilePath -> [String] -> IO [FilePath]
findFiles path patterns = do
    files <- concat . fst <$> globDir (map compile patterns) path
    return $ map (makeRelative path) files

blockName :: String -> String
blockName path = replace [pathSeparator] "_" path

blockClassPath :: FilePath -> String
blockClassPath path =
    let xs = splitDirectories path in
    (head xs) ++ "/" ++ join "_" (tail xs)

templatePath :: FilePath -> String
templatePath path = (join "/" (splitDirectories path)) ++ ".phtml"

prefixModuleName :: ModuleInfo -> [FilePath] -> [FilePath]
prefixModuleName info paths =
    map (\path -> joinPath [getName info, path]) paths

normalizeAndSplit :: FilePath -> [String]
normalizeAndSplit str = splitDirectories $ lowercase $ dropExtension str

intersectingPaths :: [FilePath] -> [FilePath] -> [FilePath]
intersectingPaths paths paths' =
    map joinPath $ intersect
        (map normalizeAndSplit paths)
        (map normalizeAndSplit paths')

scopeName :: String -> String
scopeName "frontend" = "frontend"
scopeName "admin" = "adminhtml"
