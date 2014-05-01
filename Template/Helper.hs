{-# LANGUAGE DeriveDataTypeable #-}

module Template.Helper (
    helperXml,
    helperPhp
) where

import Template (render)
import Data.Data (Data, Typeable)

data HelperXmlTemplate = HelperXmlTemplate {
    name :: String,
    classNamePrefix :: String
} deriving (Data, Typeable)

data HelperPhpTemplate = HelperPhpTemplate {
    className :: String
} deriving (Data, Typeable)

helperXml :: String -> String -> IO String
helperXml name classNamePrefix =
    render "helper/helper.xml" (HelperXmlTemplate name classNamePrefix)

helperPhp :: String -> IO String
helperPhp className =
    render "helper/helper.php" (HelperPhpTemplate className)
