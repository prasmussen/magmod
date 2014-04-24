{-# LANGUAGE DeriveDataTypeable #-}

module Template.Config (
    configXml
) where

import Template (render)
import Data.Data (Data, Typeable)

data ConfigTemplate = ConfigTemplate {
    namespace :: String,
    name :: String
} deriving (Data, Typeable)

configXml :: String -> String -> IO String
configXml namespace name =
    render "config.xml" (ConfigTemplate namespace name)
