module Util.PHP (
    insertPhpMethod
) where

import Control.Monad.State (when)
import Data.List (isInfixOf, isPrefixOf)
import System.IO (hPutStrLn, hClose, openTempFile, Handle)
import Util (renameWithBackupAndPrint)

type Parser = [(String -> Bool)]

data Status = NotFound | Found | Done deriving (Eq, Show)

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
