import System.Environment (getArgs, getProgName)
import Magento.Module (newModule, findConfigXml)
import Magento.Module.XML (readNamespaceAndName)
import Magento.Helper (addHelper)
import Magento.Model (addModel)
import Magento.Block (addBlock)
import Magento.Controller (addController)
import Magento.Observer (addObserver)
import System.Exit (exitFailure)


main :: IO ()
main = do
    args <- getArgs
    dispatch args

dispatch :: [String] -> IO ()
dispatch args = case args of
    ["new", namespace, name, codepool] ->
        newModuleHandler namespace name codepool
    ["add", "helper", name] ->
        addHelperHandler name
    ["add", "model", name] ->
        addModelHandler name
    ["add", "block", name] ->
        addBlockHandler name
    ["add", "controller", scope, name] ->
        addControllerHandler scope name
    ["add", "observer", scope, eventName] ->
        addObserverHandler scope eventName
    ["help"] ->
        helpHandler
    _ -> do
        putStrLn "Invalid arguments, use 'help' to see available commands"
        exitFailure

helpHandler :: IO ()
helpHandler = do
    name <- getProgName
    mapM_ (putStrLn . (name ++) . (" " ++)) [
        "new <namespace> <name> <codepool>",
        "add helper <name>",
        "add model <name>",
        "add block <name>",
        "add controller <scope> <controller_name>",
        "add observer <scope> <event_name>"]

newModuleHandler :: String -> String -> String -> IO ()
newModuleHandler namespace name codepool =
    newModule namespace name codepool

addHelperHandler :: String -> IO ()
addHelperHandler helperName =
    isInsideModule (\configXmlPath namespace moduleName ->
        addHelper configXmlPath namespace moduleName helperName)

addModelHandler :: String -> IO ()
addModelHandler modelName =
    isInsideModule (\configXmlPath namespace moduleName ->
        addModel configXmlPath namespace moduleName modelName)

addBlockHandler :: String -> IO ()
addBlockHandler blockName =
    isInsideModule (\configXmlPath namespace moduleName ->
        addBlock configXmlPath namespace moduleName blockName)

addControllerHandler :: String -> String -> IO ()
addControllerHandler controllerName scope =
    isInsideModule (\configXmlPath namespace moduleName ->
        addController configXmlPath namespace moduleName scope controllerName)

addObserverHandler :: String -> String -> IO ()
addObserverHandler scope eventName =
    isInsideModule (\configXmlPath namespace moduleName ->
        addObserver configXmlPath namespace moduleName scope eventName)

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
