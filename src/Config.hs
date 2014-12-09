{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Config where

import Data.ByteString (ByteString)
import Data.Configurator as C
import HFlags
import System.IO.Unsafe (unsafePerformIO)
import System.Environment

defineFlag "c:config" ("sproxy-web.config" :: String) "config file"

-- | Get the connection string and the port
--   from the config file
getConfig :: FilePath -> IO (ByteString, Int)
getConfig configFile = do
    conf    <- C.load [C.Required configFile]
    cs <- C.require conf "db_connection_string"
    p  <- C.require conf "port"
    return (cs, p)

dbConnectionString :: ByteString
port :: Int

(dbConnectionString, port) =
    unsafePerformIO (getConfig flags_config)
