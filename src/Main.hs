{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Config
import DB
import Handlers
import Paths_sproxy_web
import SproxyError

import Control.Monad.Trans
import Data.Pool
import Database.PostgreSQL.Simple
import HFlags
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Web.Scotty.Trans

main :: IO ()
main = do
    _    <- $initHFlags "sproxy-web - Web interface to the sproxy permissions database"
    pool <- createDBPool
    scottyT port id id (sproxyWeb pool)

sproxyWeb :: Pool Connection -> ScottyT SproxyError IO ()
sproxyWeb pool = do
    dataDir <- liftIO $ getDataFileName ""
    -- let's log the requests
    middleware logStdoutDev
    -- serve static files, using the generated Path_sproxy_web module
    -- to make it easily deployable
    middleware (staticPolicy $ addBase dataDir)

    -- error page for uncaught exceptions
    defaultHandler handleEx

    get "/"       $ homepage pool

    -- groups
    get "/groups" $ groupList pool           -- this is the group listing page
    get "/groups.json" $ jsonGroupList pool  -- json endpoint returning an array of group names
    post "/groups" $ jsonUpdateGroup pool    -- endpoint where we POST requests for modifications of groups

    -- group
    get "/group/:group/members" $ memberList pool       -- list of members for a group
    post "/group/:group/members" $ jsonPostMembers pool -- endpoint for POSTing updates to the member list

    -- domains
    get "/domains" $ domainList pool        -- list of domains handled by sproxy
    post "/domains" $ jsonUpdateDomain pool -- endpoint for POSTing updates

    -- privileges for a given domain
    get "/domain/:domain/privileges" $ domainPrivileges pool          -- listing of privileges available on a domain
    get "/domain/:domain/privileges.json" $ jsonDomainPrivileges pool -- json endpoint, array of privilege names
    post "/domain/:domain/privileges" $ jsonPostDomainPrivileges pool -- endpoint for POSTing updates

    -- rules for a given privilege on a given domain
    get "/domain/:domain/privilege/:privilege/rules" $ privilegeRules pool -- listing of paths/methods associated to a privilege
    post "/domain/:domain/privilege/:privilege/rules" $ jsonPostRule pool  -- endpoint for POSTing updates about these

    -- add/remove group privileges
    post "/domain/:domain/group_privileges" $ handleGPs pool -- endpoint for POSTing privilege granting/removal for groups

    notFound $ raise NotFound