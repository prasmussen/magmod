module Magento.Module.Path (
    basePath,
    jsBasePath,
    skinBasePath,
    templateBasePath,
    controllersBasePath,
    blockBasePath,
    modelBasePath,
    helperBasePath,
    layoutBasePath,
    localeBasePath,
    resourceBasePath,
    setupBasePath,
    codeRootPath
) where

import Magento.Module (ModuleInfo, getConfigXml)
import System.FilePath.Posix (joinPath, takeDirectory)

codeRootPath :: ModuleInfo -> FilePath
codeRootPath info =
    joinPath [takeDirectory $ getConfigXml info, ".."]

basePath :: ModuleInfo -> FilePath
basePath info =
    joinPath $ (takeDirectory $ getConfigXml info):(replicate 6 "..")

jsBasePath :: ModuleInfo -> FilePath
jsBasePath info = joinPath [basePath info, "js"]

skinBasePath :: ModuleInfo -> String -> FilePath
skinBasePath info "frontend" = skinBasePath' info "frontend"
skinBasePath info "adminhtml" = skinBasePath' info "adminhtml"

skinBasePath' :: ModuleInfo -> String -> FilePath
skinBasePath' info scope =
    joinPath [
        basePath info,
        "skin",
        scope,
        "base",
        "default"
    ]

templateBasePath :: ModuleInfo -> String -> FilePath
templateBasePath info "frontend" = templateBasePath' info "frontend"
templateBasePath info "adminhtml" = templateBasePath' info "adminhtml"

templateBasePath' :: ModuleInfo -> String -> FilePath
templateBasePath' info scope =
    joinPath [
        basePath info,
        "app",
        "design",
        scope,
        "base",
        "default",
        "template"
    ]

blockBasePath :: ModuleInfo -> FilePath
blockBasePath info =
    joinPath [
        codeRootPath info,
        "Block"
    ]

helperBasePath :: ModuleInfo -> FilePath
helperBasePath info =
    joinPath [
        codeRootPath info,
        "Helper"
    ]

modelBasePath :: ModuleInfo -> String
modelBasePath info =
    joinPath [
        codeRootPath info,
        "Model"
    ]

resourceBasePath :: ModuleInfo -> String
resourceBasePath info =
    joinPath [
        modelBasePath info,
        "Resource"
    ]

setupBasePath :: ModuleInfo -> String
setupBasePath info =
    joinPath [
        codeRootPath info,
        "sql"
    ]

controllersBasePath :: ModuleInfo -> FilePath
controllersBasePath info =
    joinPath [
        codeRootPath info,
        "controllers"
    ]

layoutBasePath :: ModuleInfo -> String -> FilePath
layoutBasePath info "frontend" = layoutBasePath' info "frontend"
layoutBasePath info "adminhtml" = layoutBasePath' info "adminhtml"

layoutBasePath' :: ModuleInfo -> String -> FilePath
layoutBasePath' info scope =
    joinPath [
        basePath info,
        "app",
        "design",
        scope,
        "base",
        "default",
        "layout"
    ]

localeBasePath :: ModuleInfo -> String
localeBasePath info =
    joinPath [
        basePath info,
        "app",
        "locale"
    ]

