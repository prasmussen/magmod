module Util (
    capitalize,
    capitalizePath,
    lowercase,
    camelcase,
    ensureSinglePath,
    tmpFname,
    backupFname,
    renameWithBackup
) where

import Control.Monad (when)
import System.IO.Error (isDoesNotExistError, catchIOError)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import Control.Applicative ((<$>), (<*>))
import Data.String.Utils (join, split)
import Data.Char (toUpper, toLower)
import System.Directory (
    renameFile,
    copyFile,
    getModificationTime,
    doesFileExist)
import System.FilePath.Posix (
    joinPath,
    splitPath,
    takeDirectory,
    takeBaseName,
    takeExtension)


lowercase :: String -> String
lowercase word = map toLower word

capitalize :: String -> String
capitalize word = case word of
    [] -> []
    (x:xs) -> toUpper x:map toLower xs

capitalizePath :: FilePath -> FilePath
capitalizePath path =
    joinPath $ map capitalize $ splitPath path

camelcase :: String -> String
camelcase str =
    let xs = split "_" str in
    join "" $ lowercase (head xs) : (map capitalize $ tail xs)

ensureSinglePath :: [FilePath] -> Maybe FilePath
ensureSinglePath paths =
    case paths of
        [x] -> Just x
        [] -> Nothing
        _ -> Nothing

renameWithBackup :: FilePath -> FilePath -> IO ()
renameWithBackup old new = do
    -- Do not create backup of destination file
    -- if it has been modified withing the last 3 seconds
    recentlyModified <- fileRecentlyModified new 3
    when (not recentlyModified) $ copyFile new (backupFname new)
    renameFile old new

fileRecentlyModified :: FilePath -> NominalDiffTime -> IO Bool
fileRecentlyModified p s =
    (fmap (< s) $ diffUTCTime <$> getCurrentTime <*> getModificationTime p)
    `catchIOError` \e ->
        if isDoesNotExistError e
            then return False
            else ioError e

tmpFname :: FilePath -> FilePath
tmpFname path =
    joinPath [
        takeDirectory path,
        join "" [".", takeBaseName path, "_tmp", takeExtension path]
    ]

backupFname :: FilePath -> FilePath
backupFname path =
    joinPath [
        takeDirectory path,
        join "" [".", takeBaseName path, "_bk", takeExtension path]
    ]
