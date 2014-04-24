{-# LANGUAGE DeriveDataTypeable #-}

module Template.Controller (
    controllerXml,
    controllerPhp
) where

import Template (render)
import Data.Data (Data, Typeable)

data CtrlXmlTemplate = CtrlXmlTemplate {
    moduleName :: String,
    fullModuleName :: String,
    router :: String
} deriving (Data, Typeable)

data CtrlPhpTemplate = CtrlPhpTemplate {
    className :: String,
    parentClassName :: String
} deriving (Data, Typeable)

controllerXml :: String -> String -> String -> IO String
controllerXml moduleName fullModuleName router =
    render "controller.xml" (CtrlXmlTemplate moduleName fullModuleName router)

controllerPhp :: String -> String -> IO String
controllerPhp className parentClassName =
    render "controller.php" (CtrlPhpTemplate className parentClassName)
