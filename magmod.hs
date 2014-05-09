import System.Environment (getArgs, getProgName)
import Magento.Module (newModule, findConfigXml, ModuleInfo)
import Magento.Module.XML (readModuleInfo)
import Magento.Helper (addHelper)
import Magento.Model (addModel)
import Magento.Block (addBlock)
import Magento.Controller (addController)
import Magento.Observer (addObserver)
import Magento.Resource (addResource)
import Magento.Layout (addLayout)
import Magento.Locale (addLocale)
import Magento.Setup (addInstall, addUpgrade)
import Util (removeTempFiles)
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
    ["add", "install"] -> addInstallHandler
    ["add", "upgrade"] -> addUpgradeHandler
    ["clean"] -> cleanHandler
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
        "add locale admin <locale>",
        "add install",
        "add upgrade",
        "clean"]

newModuleHandler :: String -> String -> String -> IO ()
newModuleHandler codepool namespace name =
    newModule codepool namespace name

addHelperHandler :: String -> IO ()
addHelperHandler helperName =
    withModuleInfo (\moduleInfo ->
        addHelper moduleInfo helperName)

addModelHandler :: String -> IO ()
addModelHandler modelName =
    withModuleInfo (\moduleInfo ->
        addModel moduleInfo modelName)

addBlockHandler :: String -> IO ()
addBlockHandler blockName =
    withModuleInfo (\moduleInfo ->
        addBlock moduleInfo blockName)

addControllerHandler :: String -> String -> IO ()
addControllerHandler scope controllerName =
    withModuleInfo (\moduleInfo ->
        addController moduleInfo scope controllerName)

addObserverHandler :: String -> String -> IO ()
addObserverHandler scope eventName =
    withModuleInfo (\moduleInfo ->
        addObserver moduleInfo scope eventName)

addResourceHandler :: String -> IO ()
addResourceHandler entityName =
    withModuleInfo (\moduleInfo ->
        addResource moduleInfo entityName)

addLayoutHandler :: String -> IO ()
addLayoutHandler scope =
    withModuleInfo (\moduleInfo ->
        addLayout moduleInfo scope)

addLocaleHandler :: String -> String -> IO ()
addLocaleHandler scope localeName =
    withModuleInfo (\moduleInfo ->
        addLocale moduleInfo scope localeName)

addInstallHandler :: IO ()
addInstallHandler =
    withModuleInfo (\moduleInfo ->
        addInstall moduleInfo)

addUpgradeHandler :: IO ()
addUpgradeHandler =
    withModuleInfo (\moduleInfo ->
        addUpgrade moduleInfo)

cleanHandler :: IO ()
cleanHandler = do
    files <- removeTempFiles
    mapM_ (putStrLn . ("Removed " ++)) files

withModuleInfo :: (ModuleInfo -> IO ()) -> IO ()
withModuleInfo f = do
    maybePath <- findConfigXml
    case maybePath of
        Just path -> do
            moduleInfo <- readModuleInfo path
            f moduleInfo
        Nothing -> do
            putStrLn "Found none or more than one config.xml"
            exitFailure
