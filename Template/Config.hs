{-# LANGUAGE DeriveDataTypeable #-}

module Template.Config (
    configXml
) where

import Text.Hastache.Context (mkGenericContext)
import Template (content, render)
import Data.Data (Data, Typeable)

data ConfigTemplate = ConfigTemplate {
    namespace :: String,
    name :: String
} deriving (Data, Typeable)

configXml :: String -> String -> IO String
configXml namespace name = render
    (content "config.xml")
    (mkGenericContext $ ConfigTemplate namespace name)
