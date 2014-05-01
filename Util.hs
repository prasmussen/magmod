module Util (
    capitalize,
    capitalizePath,
    lowercase,
    camelcase,
    ensureSinglePath,
    tmpFname,
    backupFname,
    renameWithBackupAndPrint,
    writeFileAndPrint
) where

import Control.Monad as M
import System.IO.Error (isDoesNotExistError, catchIOError)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import Control.Applicative ((<$>), (<*>))
import Data.String.Utils as U
import Data.Char (toUpper, toLower)
import System.Directory (
    makeRelativeToCurrentDirectory,
    canonicalizePath,
    copyPermissions,
    renameFile,
    copyFile,
    getModificationTime)
import System.FilePath.Posix (
    joinPath,
    splitPath,
    takeDirectory,
    takeBaseName,
    takeExtension)

prettyPath :: FilePath -> (FilePath -> IO ()) -> IO ()
prettyPath path f = do
    pretty <- normalizePath path
    f pretty

writeFileAndPrint :: FilePath -> String -> IO ()
writeFileAndPrint path content = do
    writeFile path content
    prettyPath path (\x -> putStrLn $ "-- Created " ++ x)

normalizePath :: FilePath -> IO FilePath
normalizePath path =
    M.join $ makeRelativeToCurrentDirectory <$> canonicalizePath path

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
    let xs = U.split "_" str in
    U.join "" $ lowercase (head xs) : (map capitalize $ tail xs)

ensureSinglePath :: [FilePath] -> Maybe FilePath
ensureSinglePath paths =
    case paths of
        [x] -> Just x
        [] -> Nothing
        _ -> Nothing

renameWithBackupAndPrint :: FilePath -> FilePath -> IO ()
renameWithBackupAndPrint old new = do
    -- Do not create backup of destination file
    -- if it has been modified within the last 3 seconds
    recentlyModified <- fileRecentlyModified new 3
    M.when (not recentlyModified) $ copyFile new (backupFname new)
    renameFile old new
    copyPermissions (backupFname new) new
    prettyPath new (\x -> putStrLn $ "-- Updated " ++ x)

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
        U.join "" [".", takeBaseName path, "_tmp", takeExtension path]
    ]

backupFname :: FilePath -> FilePath
backupFname path =
    joinPath [
        takeDirectory path,
        U.join "" [".", takeBaseName path, "_bk", takeExtension path]
    ]
