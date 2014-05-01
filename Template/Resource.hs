{-# LANGUAGE DeriveDataTypeable #-}

module Template.Resource (
    resourceXml,
    entityXml,
    modelConstructor,
    resourcePhp,
    collectionPhp
) where

import Template (render)
import Data.Data (Data, Typeable)

data ConfigXmlTemplate = ConfigXmlTemplate {
    classPrefix :: String,
    resourceName :: String
} deriving (Data, Typeable)

data ResourceTemplate = ResourceTemplate {
    moduleName :: String,
    className :: String,
    entityName :: String,
    tableName :: String
} deriving (Data, Typeable)

resourceXml :: String -> String -> IO String
resourceXml classPrefix resourceName =
    render
        "resource/resource.xml"
        (ConfigXmlTemplate classPrefix resourceName)

entityXml :: String -> String -> IO String
entityXml entityName tableName =
    render
        "resource/entity.xml"
        (ResourceTemplate "" "" entityName tableName)

modelConstructor :: String -> String -> IO String
modelConstructor moduleName entityName =
    render
        "resource/model_constructor.php"
        (ResourceTemplate moduleName "" entityName "")

resourcePhp :: String -> String -> String -> IO String
resourcePhp moduleName className entityName =
    render
        "resource/resource.php"
        (ResourceTemplate moduleName className entityName "")

collectionPhp :: String -> String -> String -> IO String
collectionPhp moduleName className entityName =
    render
        "resource/collection.php"
        (ResourceTemplate moduleName className entityName "")
