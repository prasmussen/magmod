import System.Environment (getArgs, getProgName)
import Magento.Module (newModule, findConfigXml)
import Magento.Module.XML (readNamespaceAndName)
import Magento.Helper (addHelper)
import Magento.Model (addModel)
import Magento.Block (addBlock)
import Magento.Controller (addController)
import Magento.Observer (addObserver)
import Magento.Resource (addResource)
import Magento.Layout (addLayout)
import Magento.Locale (addLocale)
import System.Exit (exitFailure)


main :: IO ()
main = do
    args <- getArgs
    dispatch args

dispatch :: [String] -> IO ()
dispatch args = case args of
    ["new", "local", namespace, name] ->
        newModuleHandler "local" namespace name
    ["new", "community", namespace, name] ->
        newModuleHandler "community" namespace name
    ["add", "helper", name] ->
        addHelperHandler name
    ["add", "model", name] ->
        addModelHandler name
    ["add", "block", name] ->
        addBlockHandler name
    ["add", "controller", "frontend", name] ->
        addControllerHandler "frontend" name
    ["add", "controller", "admin", name] ->
        addControllerHandler "admin" name
    ["add", "observer", "frontend", eventName] ->
        addObserverHandler "frontend" eventName
    ["add", "observer", "admin", eventName] ->
        addObserverHandler "admin" eventName
    ["add", "observer", "global", eventName] ->
        addObserverHandler "global" eventName
    ["add", "resource", name] ->
        addResourceHandler name
    ["add", "layout", "frontend"] ->
        addLayoutHandler "frontend"
    ["add", "layout", "admin"] ->
        addLayoutHandler "admin"
    ["add", "locale", "frontend", name] ->
        addLocaleHandler "frontend" name
    ["add", "locale", "admin", name] ->
        addLocaleHandler "admin" name
    ["help"] -> helpHandler
    ["-h"] -> helpHandler
    ["--help"] -> helpHandler
    _ -> do
        putStrLn "Invalid arguments, use 'help' to see available commands"
        exitFailure

helpHandler :: IO ()
helpHandler = do
    name <- getProgName
    mapM_ (putStrLn . (name ++) . (" " ++)) [
        "new local <namespace> <name>",
        "new community <namespace> <name>",
        "add helper <name>",
        "add model <name>",
        "add block <name>",
        "add controller frontend <name>",
        "add controller admin <name>",
        "add resource <name>",
        "add observer frontend <event>",
        "add observer admin <event>",
        "add observer global <event>",
        "add layout frontend",
        "add layout admin",
        "add locale frontend <locale>",
        "add locale admin <locale>"]

newModuleHandler :: String -> String -> String -> IO ()
newModuleHandler codepool namespace name =
    newModule codepool namespace name

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
addControllerHandler scope controllerName =
    isInsideModule (\configXmlPath namespace moduleName ->
        addController configXmlPath namespace moduleName scope controllerName)

addObserverHandler :: String -> String -> IO ()
addObserverHandler scope eventName =
    isInsideModule (\configXmlPath namespace moduleName ->
        addObserver configXmlPath namespace moduleName scope eventName)

addResourceHandler :: String -> IO ()
addResourceHandler entityName =
    isInsideModule (\configXmlPath namespace moduleName ->
        addResource configXmlPath namespace moduleName entityName)

addLayoutHandler :: String -> IO ()
addLayoutHandler scope =
    isInsideModule (\configXmlPath _ moduleName ->
        addLayout configXmlPath moduleName scope)

addLocaleHandler :: String -> String -> IO ()
addLocaleHandler scope localeName =
    isInsideModule (\configXmlPath namespace moduleName ->
        addLocale configXmlPath namespace moduleName scope localeName)

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
