module Magento.Module (
    createModuleXml,
    createConfigXml
) where

import System.Directory (createDirectoryIfMissing, renameFile, removeFile)
import System.FilePath.Posix (joinPath, takeDirectory)
import Template.Module (moduleXml)
import Template.Config (configXml)
import Util (capitalize, lowercase)


moduleXmlFname :: String -> String -> String
moduleXmlFname namespace name =
    foldl (++) "" [
        capitalize namespace,
        "_",
        capitalize name,
        ".xml"
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
writeModuleXml path namespace name codepool = do
    xml <- moduleXml
        (lowercase namespace)
        (lowercase name)
        (lowercase codepool)
    writeFile path xml

writeConfigXml :: String -> String -> String -> IO ()
writeConfigXml path namespace name = do
    xml <- configXml
        (lowercase namespace)
        (lowercase name)
    writeFile path xml

createModuleXml :: String -> String -> String -> IO ()
createModuleXml namespace name codepool =
    let moduleXmlPath = composeModuleXmlPath namespace name
    in do
        createDirectoryIfMissing True (takeDirectory moduleXmlPath)
        writeModuleXml moduleXmlPath namespace name codepool

createConfigXml :: String -> String -> String -> IO ()
createConfigXml namespace name codepool =
    let configXmlPath = composeConfigXmlPath codepool namespace name
    in do
        createDirectoryIfMissing True (takeDirectory configXmlPath)
        writeConfigXml configXmlPath namespace name
