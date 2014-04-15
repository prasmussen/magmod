module Util (
    capitalize,
    lowercase
) where

import Data.Char (toUpper, toLower)

lowercase :: String -> String
lowercase word = map toLower word

capitalize :: String -> String
capitalize word = case word of
    [] -> []
    (x:xs) -> toUpper x:map toLower xs
