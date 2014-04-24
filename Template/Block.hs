{-# LANGUAGE DeriveDataTypeable #-}

module Template.Block (
    blockXml,
    blockPhp
) where

import Template (render)
import Data.Data (Data, Typeable)

data BlockXmlTemplate = BlockXmlTemplate {
    name :: String,
    classNamePrefix :: String
} deriving (Data, Typeable)

data BlockPhpTemplate = BlockPhpTemplate {
    className :: String
} deriving (Data, Typeable)

blockXml :: String -> String -> IO String
blockXml name classNamePrefix =
    render "block.xml" (BlockXmlTemplate name classNamePrefix)

blockPhp :: String -> IO String
blockPhp className =
    render "block.php" (BlockPhpTemplate className)
