{-# LANGUAGE DeriveDataTypeable #-}

module Template.Setup (
    setupXml,
    installPhp,
    upgradePhp
) where

import Template (render)
import Data.Data (Data, Typeable)

data Setup = Setup {
    setupIdentifier :: String,
    fullModuleName :: String
} deriving (Data, Typeable)

data Entity = Entity {
    name :: String
} deriving (Data, Typeable)

data Entities = Entities {
    moduleName :: String,
    entities :: [Entity]
} deriving (Data, Typeable)

setupXml :: String -> String -> IO String
setupXml setupIdentifier fullModuleName =
    render "setup/setup.xml" (Setup setupIdentifier fullModuleName)

installPhp :: String -> [String] -> IO String
installPhp moduleName names  =
    render "setup/install.php" (Entities moduleName $ map Entity names)

upgradePhp :: String -> [String] -> IO String
upgradePhp moduleName names  =
    render "setup/upgrade.php" (Entities moduleName $ map Entity names)
