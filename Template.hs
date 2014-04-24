{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Template (
    render
) where

import Data.Data
import Data.FileEmbed (embedDir)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import Text.Hastache.Context (mkGenericContext)
import Text.Hastache (
    hastacheStr,
    defaultConfig,
    encodeStr,
    decodeStrLT)

files :: [(FilePath, ByteString)]
files = $(embedDir "templates")

content :: FilePath -> String
content path = toString . snd . head $ filter (\(p, _) -> p == path) files

render :: Data a => FilePath -> a -> IO String
render templatePath values = do
    res <- hastacheStr
        defaultConfig
        (encodeStr $ content templatePath)
        (mkGenericContext values)
    return $ decodeStrLT res
