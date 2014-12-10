{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Config where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Configurator as C
import HFlags

defineFlag "c:config" ("sproxy-web.config" :: String) "config file"

data Config = Config {
    dbConnectionString :: ByteString,
    port :: Int
  }
    deriving (Show, Eq)

-- | Get the connection string and the port
--   from the config file
getConfig :: FilePath -> IO Config
getConfig configFile = do
    conf    <- C.load [C.Required configFile]
    Config <$>
        C.require conf "db_connection_string" <*>
        C.require conf "port"
