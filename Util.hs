module Util (
    capitalize,
    capitalizePath,
    lowercase,
    ensureSinglePath,
    tmpFname,
    backupFname,
    renameWithBackup
) where

import Data.Char (toUpper, toLower)
import System.Directory (renameFile, copyFile)
import System.FilePath.Posix (
    joinPath,
    splitPath,
    takeDirectory,
    takeBaseName)


lowercase :: String -> String
lowercase word = map toLower word

capitalize :: String -> String
capitalize word = case word of
    [] -> []
    (x:xs) -> toUpper x:map toLower xs

capitalizePath :: FilePath -> FilePath
capitalizePath path =
    joinPath $ map capitalize $ splitPath path

ensureSinglePath :: [FilePath] -> Maybe FilePath
ensureSinglePath paths =
    case paths of
        [x] -> Just x
        [] -> Nothing
        _ -> Nothing

renameWithBackup :: FilePath -> FilePath -> IO ()
renameWithBackup old new = do
    copyFile new (backupFname new)
    renameFile old new

tmpFname :: FilePath -> FilePath
tmpFname path =
    joinPath [
        takeDirectory path,
        "." ++ takeBaseName path ++ ".new"
    ]

backupFname :: FilePath -> FilePath
backupFname path =
    joinPath [
        takeDirectory path,
        "." ++ takeBaseName path ++ ".old"
    ]
