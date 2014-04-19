{-# LANGUAGE DeriveDataTypeable #-}

module Template.Helper (
    helperXml,
    helperPhp
) where

import Text.Hastache.Context (mkGenericContext)
import Template (content, render)
import Data.Data (Data, Typeable)

data HelperXmlTemplate = HelperXmlTemplate {
    name :: String,
    classNamePrefix :: String
} deriving (Data, Typeable)

data HelperPhpTemplate = HelperPhpTemplate {
    className :: String
} deriving (Data, Typeable)

helperXml :: String -> String -> IO String
helperXml name classNamePrefix = render
    (content "helper.xml")
    (mkGenericContext $ HelperXmlTemplate name classNamePrefix)

helperPhp :: String -> IO String
helperPhp className = render
    (content "helper.php")
    (mkGenericContext $ HelperPhpTemplate className)
