{-# LANGUAGE DeriveDataTypeable #-}

module Template.Model (
    modelXml,
    modelPhp
) where

import Text.Hastache.Context (mkGenericContext)
import Template (content, render)
import Data.Data (Data, Typeable)

data ModelXmlTemplate = ModelXmlTemplate {
    name :: String,
    classNamePrefix :: String
} deriving (Data, Typeable)

data ModelPhpTemplate = ModelPhpTemplate {
    className :: String
} deriving (Data, Typeable)

modelXml :: String -> String -> IO String
modelXml name classNamePrefix = render
    (content "model.xml")
    (mkGenericContext $ ModelXmlTemplate name classNamePrefix)

modelPhp :: String -> IO String
modelPhp className = render
    (content "model.php")
    (mkGenericContext $ ModelPhpTemplate className)
