module Magento.Module.Version (
    Version,
    showVersion,
    parseVersion,
    parseVersionRange,
    incrementMajor,
    incrementMinor,
    incrementPatch,
) where

import Data.String.Utils (join)
import Text.Regex.Posix ((=~))

type Version = VersionData
data VersionData = Version Int Int Int deriving (Show, Read, Eq, Ord)

showVersion :: Version -> String
showVersion (Version x y z) = join "." $ map show [x, y, z]

incrementMajor :: Version -> Version
incrementMajor (Version x _ _) = Version (x + 1) 0 0

incrementMinor :: Version -> Version
incrementMinor (Version x y _) = Version x (y + 1) 0

incrementPatch :: Version -> Version
incrementPatch (Version x y z) = Version x y (z + 1)

parseVersion :: String -> Version
parseVersion str = case (parse str) of
    [x, y, z] -> Version x y z
    _ -> Version 0 0 0

parseVersionRange :: String -> (Version, Version)
parseVersionRange str = case (parse str) of
    [x, y, z, x', y', z'] -> (Version x y z, Version x' y' z')
    _ -> (Version 0 0 0, Version 0 0 0)

parse :: String -> [Int]
parse str = map read $ concat (str =~ "[0-9]+")
