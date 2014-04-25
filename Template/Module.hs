{-# LANGUAGE DeriveDataTypeable #-}

module Template.Module (
    moduleXml
) where

import Template (render)
import Data.Data (Data, Typeable)

data ModuleTemplate = ModuleTemplate {
    codepool :: String,
    namespace :: String,
    name :: String
} deriving (Data, Typeable)

moduleXml :: String -> String -> String -> IO String
moduleXml codepool namespace name =
    render "module.xml" (ModuleTemplate codepool namespace name)

