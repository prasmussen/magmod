module Util.PHP (
    PhpClass,
    readClass,
    getClassName,
    getParentName,
    getPublicFunctions,
    insertPhpMethod
) where

import Control.Monad.State (when)
import Data.List (isInfixOf, isPrefixOf)
import System.IO (hPutStrLn, hClose, openTempFile, Handle)
import Util (renameWithBackupAndPrint)

type Parser = [(String -> Bool)]

data Status = NotFound | Found | Done deriving (Eq, Show)

type PhpClass = PhpClassData
data PhpClassData = PhpClass {
    getClassName :: String,
    getParentName :: String,
    getPublicFunctions :: [String]
}

readClass :: FilePath -> IO PhpClass
readClass path = do
    (name, parent) <- getClassAndParent path
    fns <- listPublicFunctions path
    return PhpClass {
        getClassName=name,
        getParentName=parent,
        getPublicFunctions=fns
    }

getClassAndParent :: FilePath -> IO (String, String)
getClassAndParent path = do
    contents <- readFile path
    return $ case (classesAndParents (lines contents) []) of
        [] -> ("", "")
        (x:_) -> x

listPublicFunctions :: FilePath -> IO [String]
listPublicFunctions path = do
    contents <- readFile path
    return $ publicFunctions (lines contents) []

classesAndParents :: [String] -> [(String, String)] -> [(String, String)]
classesAndParents [] xs = xs
classesAndParents (line:lines) xs =
    case (take 4 $ words line) of
        ["class", name, "extends", parent] ->
            classesAndParents lines ((name, parent):xs)
        _ ->
            classesAndParents lines xs

publicFunctions :: [String] -> [String] -> [String]
publicFunctions [] names = names
publicFunctions (line:lines) names =
    case (take 3 $ words line) of
        ["public", "function", name] -> publicFunctions lines (name:names)
        _ -> publicFunctions lines names

insertPhpMethod :: FilePath -> String -> IO ()
insertPhpMethod path payload = do
    contents <- readFile path
    (tmp, handle) <- openTempFile "." ".tmp"
    insert handle (lines contents) phpClassBodyStartParser payload
    renameWithBackupAndPrint tmp path

insert :: Handle -> [String] -> Parser -> String -> IO ()
insert dst [] _ _ = hClose dst
insert dst (line:xs) parser payload = do
    hPutStrLn dst line
    let (status, newParser) = parse parser line in do
        when (status == Found) $ hPutStrLn dst payload
        insert dst xs newParser payload

phpClassBodyStartParser :: Parser
phpClassBodyStartParser = [findClass, findLeftCurlyBracket]

findLeftCurlyBracket :: String -> Bool
findLeftCurlyBracket str = isInfixOf "{" str

findClass :: String -> Bool
findClass str = isPrefixOf "class" str

parse :: Parser -> String -> (Status, Parser)
parse [] _ = (Done, [])
parse [f] str = case f str of
    True -> (Found, [])
    False -> (NotFound, [f])
parse (f:fs) str = case f str of
    True -> parse fs str
    False -> (NotFound, f:fs)
