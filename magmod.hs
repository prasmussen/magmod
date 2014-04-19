import System.Environment (getArgs)
import Magento.Module (newModule, findConfigXml)
import Magento.Module.XML (readNamespaceAndName)
import Magento.Helper (addHelper)
import System.Exit (exitFailure)


main :: IO ()
main = do
    args <- getArgs
    dispatch args

dispatch :: [String] -> IO ()
dispatch args =
    case args of
        ["new", namespace, name, codepool] ->
            newModuleHandler namespace name codepool
        ["add", "helper", name] ->
            addHelperHandler name
        ["help"] ->
            helpHandler
        _ -> do
            putStrLn "No match"
            exitFailure

helpHandler :: IO ()
helpHandler = putStrLn "Such helpful"

addHelperHandler :: String -> IO ()
addHelperHandler helperName =
    isInsideModule (\configXmlPath namespace moduleName ->
        addHelper configXmlPath namespace moduleName helperName)

newModuleHandler :: String -> String -> String -> IO ()
newModuleHandler namespace name codepool =
    newModule namespace name codepool

isInsideModule :: (FilePath -> String -> String -> IO ()) -> IO ()
isInsideModule f = do
    maybePath <- findConfigXml
    case maybePath of
        Just path -> do
            (namespace, name) <- readNamespaceAndName path
            f path namespace name
        Nothing -> do
            putStrLn "Found none or more than one config.xml"
            exitFailure
