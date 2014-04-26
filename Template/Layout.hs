{-# LANGUAGE DeriveDataTypeable #-}

module Template.Layout (
    layoutConfigXml,
    layoutXml
) where

import Template (render)
import Data.Data (Data, Typeable)

data LayoutXmlTemplate = LayoutXmlTemplate {
    moduleName :: String
} deriving (Data, Typeable)

layoutConfigXml :: String -> IO String
layoutConfigXml moduleName =
    render "layout_config.xml" (LayoutXmlTemplate moduleName)

layoutXml :: IO String
layoutXml =
    render "layout.xml" (LayoutXmlTemplate "")

