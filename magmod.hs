import System.Environment (getArgs)
import Magento.Module (createModuleXml, createConfigXml, findConfigXml)
import Magento.Module.XML (readNamespaceAndName)
import Magento.Helper (insertHelperXmlIfMissing)
import System.Exit (exitFailure)

dispatch :: [String] -> IO ()
dispatch ["new", namespace, name, codepool] = new namespace name codepool
dispatch ["add", "helper", name] = addHelper name
dispatch ["help"] = help
dispatch args = putStrLn "No match"


main :: IO ()
main = do
    args <- getArgs
    dispatch args

help :: IO ()
help = putStrLn "Such helpful"

addHelper :: String -> IO ()
addHelper helperName = do
    isInsideModule (\path namespace moduleName -> do
        insertHelperXmlIfMissing path namespace moduleName)
    putStrLn "Done"

new :: String -> String -> String -> IO ()
new namespace name codepool = do
    createModuleXml namespace name codepool
    createConfigXml namespace name codepool
    putStrLn "Done"


isInsideModule :: (FilePath -> String -> String -> IO ()) -> IO ()
isInsideModule f = do
    maybePath <- findConfigXml
    case maybePath of
        Just path -> do
            (namespace, name) <- readNamespaceAndName path
            f path namespace name
        Nothing ->
            putStrLn "Found none or more than one config.xml"
