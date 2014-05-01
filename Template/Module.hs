{-# LANGUAGE DeriveDataTypeable #-}

module Template.Module (
    moduleXml,
    configXml
) where

import Template (render)
import Data.Data (Data, Typeable)

data ModuleTemplate = ModuleTemplate {
    codepool :: String,
    fullModuleName :: String
} deriving (Data, Typeable)

moduleXml :: String -> String -> IO String
moduleXml codepool fullModuleName =
    render "module/module.xml" (ModuleTemplate codepool fullModuleName)

configXml :: String -> IO String
configXml fullModuleName =
    render "module/config.xml" (ModuleTemplate "" fullModuleName)
