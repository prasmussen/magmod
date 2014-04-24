{-# LANGUAGE DeriveDataTypeable #-}

module Template.Controller (
    controllerXml,
    controllerPhp
) where

import Text.Hastache.Context (mkGenericContext)
import Template (content, render)
import Data.Data (Data, Typeable)

data ControllerXmlTemplate = ControllerXmlTemplate {
    moduleName :: String,
    fullModuleName :: String,
    router :: String
} deriving (Data, Typeable)

data ControllerPhpTemplate = ControllerPhpTemplate {
    className :: String,
    parentClassName :: String
} deriving (Data, Typeable)

controllerXml :: String -> String -> String -> IO String
controllerXml moduleName fullModuleName router = render
    (content "controller.xml")
    (mkGenericContext $ ControllerXmlTemplate moduleName fullModuleName router)

controllerPhp :: String -> String -> IO String
controllerPhp className parentClassName = render
    (content "controller.php")
    (mkGenericContext $ ControllerPhpTemplate className parentClassName)
