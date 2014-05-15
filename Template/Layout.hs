{-# LANGUAGE DeriveDataTypeable #-}

module Template.Layout (
    layoutConfigXml,
    layoutXml,
    genXml
) where

import Template (render)
import Data.Data (Data, Typeable)

data LayoutXmlTemplate = LayoutXmlTemplate {
    moduleName :: String
} deriving (Data, Typeable)

data Block = Block {
    block :: String,
    template :: String,
    name :: String
} deriving (Data, Typeable)

data File = File {
    path :: String
} deriving (Data, Typeable)

data Controller = Controller {
    handle :: String
} deriving (Data, Typeable)

data Layout = Layout {
    controllers :: [Controller],
    stylesheets :: [File],
    items :: [File],
    scripts :: [File],
    blocks :: [Block]
} deriving (Data, Typeable)

layoutConfigXml :: String -> IO String
layoutConfigXml moduleName =
    render "layout/config.xml" (LayoutXmlTemplate moduleName)

layoutXml :: IO String
layoutXml = render "layout/layout.xml" (LayoutXmlTemplate "")

genXml :: [String]
       -> [String]
       -> [String]
       -> [String]
       -> [(String, String, String)]
       -> IO String
genXml handles stylesheets items scripts blocks =
    render "layout/gen.xml" $ Layout
        (map Controller handles)
        (map File stylesheets)
        (map File items)
        (map File scripts)
        (map (\(block, template, name) -> Block block template name) blocks)
