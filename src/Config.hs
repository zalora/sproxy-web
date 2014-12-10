{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Config where

import Control.Applicative
import Control.Exception
import Data.ByteString (ByteString)
import Data.Configurator as C
import HFlags
import System.Directory
import System.FilePath
import System.IO

import Paths_sproxy_web

defineFlag "c:config" ("sproxy-web.config" :: String) "config file"

data Config = Config {
    dbConnectionString :: ByteString,
    port :: Int,
    staticDir :: FilePath
  }
    deriving (Show, Eq)

-- | Get the connection string and the port
--   from the config file
getConfig :: FilePath -> IO Config
getConfig configFile = do
    conf    <- C.load [C.Required configFile]
    Config <$>
        C.require conf "db_connection_string" <*>
        C.require conf "port" <*>
        getStaticDir

getStaticDir :: IO FilePath
getStaticDir = do
    currentDir <- getCurrentDirectory
    wwwExists <- doesDirectoryExist (currentDir </> "static")
    if wwwExists then do
        hPutStrLn stderr ("Serving static files from " ++ currentDir ++
                          " -- This is bad since it probably allows to publicly access source code files.")
        return currentDir
    else do
        cabalDataDir <- getDataDir
        cabalDataDirExists <- doesDirectoryExist cabalDataDir
        if cabalDataDirExists
            then return cabalDataDir
            else throwIO (ErrorCall "directory for static files not found.")
