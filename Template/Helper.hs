{-# LANGUAGE DeriveDataTypeable #-}

module Template.Helper (
    helperXml
) where

import Text.Hastache.Context (mkGenericContext)
import Template (content, render)
import Data.Data (Data, Typeable)

data HelperTemplate = HelperTemplate {
    name :: String,
    className :: String
} deriving (Data, Typeable)

helperXml :: String -> String -> IO String
helperXml name className = render
    (content "helper.xml")
    (mkGenericContext $ HelperTemplate name className)
