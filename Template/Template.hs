{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Template.Template (
    content,
    render
) where

import Data.FileEmbed (embedDir)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import Text.Hastache (
    hastacheStr,
    defaultConfig,
    encodeStr,
    decodeStrLT,
    MuContext)
import Control.Monad.Trans (MonadIO)

files :: [(FilePath, ByteString)]
files = $(embedDir "templates")

content :: FilePath -> String
content path = toString . snd . head $ filter (\(p, _) -> p == path) files

render :: (MonadIO IO) => String -> (MuContext IO) -> IO String
render template ctx = do
    res <- hastacheStr defaultConfig (encodeStr template) ctx
    return $ decodeStrLT res
