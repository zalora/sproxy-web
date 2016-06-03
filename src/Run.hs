{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Run where

import Config
import DB
import Handlers
import SproxyError

import Data.Default.Class
import Data.Maybe (fromJust)
import Data.Pool
import Data.Version (showVersion)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Paths_sproxy_web (version) -- from cabal
import System.Environment (getArgs)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Text.RawString.QQ (r)
import Web.Scotty.Trans
import qualified System.Console.Docopt.NoTH as O

usage :: String
usage =  "sproxy-web " ++ showVersion version ++
  " web interface to the sproxy database" ++ [r|

Usage:
  sproxy-web [options]

Options:
  -c, --config=FILE        Configuration file [default: sproxy-web.conf]
  -h, --help               Show this message

|]

run :: IO ()
run = do
  doco <- O.parseUsageOrExit usage
  args <- O.parseArgsOrExit doco =<< getArgs
  if args `O.isPresent` O.longOption "help"
  then putStrLn $ O.usage doco
  else do
    config <- getConfig . fromJust . O.getArg args $ O.longOption "config"
    pool <- createDBPool (dbConnectionString config)
    let
      warpSettings =
            setPort (port config) $
            setBeforeMainLoop (do
                hPutStrLn stderr ("Serving static files from " ++ staticDir config)
                hPutStrLn stderr ("Listening on port " ++ show (port config))) $
            -- see https://hackage.haskell.org/package/scotty-0.9.0/docs/Web-Scotty-Trans.html#t:Options
            setFdCacheDuration 0 $
            defaultSettings

    runSettings warpSettings =<< scottyAppT id (sproxyWeb (staticDir config) pool)

sproxyWeb :: FilePath -> Pool Connection -> ScottyT SproxyError IO ()
sproxyWeb staticDirectory pool = do
    -- let's log the requests
    middleware . unsafePerformIO $ mkRequestLogger def{ destination = Handle stderr }

    middleware (staticPolicy (hasPrefix "static" >-> addBase staticDirectory))

    -- error page for uncaught exceptions
    defaultHandler handleEx

    get "/"       $ homepage pool

    post "/search" $ searchUserH pool
    post "/delete-user" $ deleteUserH pool
    post "/rename-user" $ renameUserH pool

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

