{-# LANGUAGE DeriveDataTypeable #-}

module Template.Module (
    moduleXml
) where

import Text.Hastache.Context (mkGenericContext)
import Template.Template (content, render)
import Data.Data (Data, Typeable)

data ModuleTemplate = ModuleTemplate {
    namespace :: String,
    name :: String,
    codepool :: String
} deriving (Data, Typeable)

moduleXml :: String -> String -> String -> IO String
moduleXml namespace name codepool = render
    (content "module.xml")
    (mkGenericContext $ ModuleTemplate namespace name codepool)

