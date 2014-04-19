{-# LANGUAGE DeriveDataTypeable #-}

module Template.Block (
    blockXml,
    blockPhp
) where

import Text.Hastache.Context (mkGenericContext)
import Template (content, render)
import Data.Data (Data, Typeable)

data BlockXmlTemplate = BlockXmlTemplate {
    name :: String,
    classNamePrefix :: String
} deriving (Data, Typeable)

data BlockPhpTemplate = BlockPhpTemplate {
    className :: String
} deriving (Data, Typeable)

blockXml :: String -> String -> IO String
blockXml name classNamePrefix = render
    (content "block.xml")
    (mkGenericContext $ BlockXmlTemplate name classNamePrefix)

blockPhp :: String -> IO String
blockPhp className = render
    (content "block.php")
    (mkGenericContext $ BlockPhpTemplate className)
