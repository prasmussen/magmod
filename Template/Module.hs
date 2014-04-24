{-# LANGUAGE DeriveDataTypeable #-}

module Template.Module (
    moduleXml
) where

import Template (render)
import Data.Data (Data, Typeable)

data ModuleTemplate = ModuleTemplate {
    namespace :: String,
    name :: String,
    codepool :: String
} deriving (Data, Typeable)

moduleXml :: String -> String -> String -> IO String
moduleXml namespace name codepool =
    render "module.xml" (ModuleTemplate namespace name codepool)

