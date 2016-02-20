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

controllerXml :: String -> String -> String -> String -> IO String
controllerXml moduleName fullModuleName router "frontend" =
    render "controller/frontend.xml" (CtrlXmlTemplate moduleName fullModuleName router)
controllerXml moduleName fullModuleName router "admin" =
    render "controller/admin.xml" (CtrlXmlTemplate moduleName fullModuleName router)

controllerPhp :: String -> String -> String -> IO String
controllerPhp className parentClassName "frontend" =
    render "controller/frontend.php" (CtrlPhpTemplate className parentClassName)
controllerPhp className parentClassName "admin" =
    render "controller/admin.php" (CtrlPhpTemplate className parentClassName)
