{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module SproxyError where

import qualified Data.Text.Lazy as T

import Control.Exception
import Data.String
import Data.Typeable
import Database.PostgreSQL.Simple
import Web.Scotty.Trans

data SproxyError = SqlErr !SqlError
                 | NotFound
                 | Other  !T.Text
    deriving Typeable

instance Exception SproxyError where

instance Show SproxyError where
    show (SqlErr e) = "PGSQL error: " ++ show e
    show (NotFound) = "This page doesn't exist, sorry."
    show (Other e)  = "Unknown error: " ++ T.unpack e

instance ScottyError SproxyError where
    stringError = Other . T.pack
    showError   = fromString . show