{-# LANGUAGE DeriveDataTypeable #-}

module Template.Observer (
    observerXml,
    observerPhp
) where

import Template (render)
import Data.Data (Data, Typeable)

data ObserverTemplate = ObserverTemplate {
    moduleName :: String,
    eventName :: String,
    methodName :: String
} deriving (Data, Typeable)

observerXml :: String -> String -> String -> IO String
observerXml moduleName eventName methodName =
    render
        "observer/observer.xml"
        (ObserverTemplate moduleName eventName methodName)

observerPhp :: String -> IO String
observerPhp methodName =
    render
        "observer/observer.php"
        (ObserverTemplate "" "" methodName)
