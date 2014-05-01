{-# LANGUAGE DeriveDataTypeable #-}

module Template.Locale (
    localeXml
) where

import Template (render)
import Data.Data (Data, Typeable)

data LocaleXmlTemplate = LocaleXmlTemplate {
    fullModuleName :: String,
    filename :: String
} deriving (Data, Typeable)

localeXml :: String -> String -> IO String
localeXml fullModuleName filename =
    render "locale/locale.xml" (LocaleXmlTemplate fullModuleName filename)
