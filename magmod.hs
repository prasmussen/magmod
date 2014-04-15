import System.Environment (getArgs)
import Magento.Module (createModuleXml, createConfigXml)

dispatch :: [String] -> IO ()
dispatch ["new", namespace, name, codepool] = new namespace name codepool
dispatch ["help"] = help
dispatch args = putStrLn "No match"


main = do
    args <- getArgs
    dispatch args

help :: IO ()
help = putStrLn "Such helpful"

new :: String -> String -> String -> IO ()
new namespace name codepool = do
    createModuleXml namespace name codepool
    createConfigXml namespace name codepool
    putStrLn "Done"
