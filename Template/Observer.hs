{-# LANGUAGE DeriveDataTypeable #-}

module Template.Observer (
    observerXml,
    observerPhp
) where

import Text.Hastache.Context (mkGenericContext)
import Template (content, render)
import Data.Data (Data, Typeable)

data ObserverTemplate = ObserverTemplate {
    moduleName :: String,
    eventName :: String,
    methodName :: String
} deriving (Data, Typeable)

observerXml :: String -> String -> String -> IO String
observerXml moduleName eventName methodName = render
    (content "observer.xml")
    (mkGenericContext $ ObserverTemplate moduleName eventName methodName)

observerPhp :: String -> IO String
observerPhp methodName = render
    (content "observer.php")
    (mkGenericContext $ ObserverTemplate "" "" methodName)
