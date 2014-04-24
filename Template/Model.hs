{-# LANGUAGE DeriveDataTypeable #-}

module Template.Model (
    modelXml,
    modelPhp
) where

import Template (render)
import Data.Data (Data, Typeable)

data ModelXmlTemplate = ModelXmlTemplate {
    name :: String,
    classNamePrefix :: String
} deriving (Data, Typeable)

data ModelPhpTemplate = ModelPhpTemplate {
    className :: String
} deriving (Data, Typeable)

modelXml :: String -> String -> IO String
modelXml name classNamePrefix =
    render "model.xml" (ModelXmlTemplate name classNamePrefix)

modelPhp :: String -> IO String
modelPhp className =
    render "model.php" (ModelPhpTemplate className)
