{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Application (
  app
) where

import Control.Exception
import Control.Monad (when)
import Data.Default.Class
import Data.Int (Int64)
import Data.Monoid
import Data.Pool (Pool)
import Data.Text.Lazy as Text
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types.Status
import Network.Wai (Application, Middleware)
import Network.Wai.Middleware.RequestLogger (Destination(Handle),
  mkRequestLogger, RequestLoggerSettings(destination, outputFormat),
  OutputFormat(CustomOutputFormat))
import Network.Wai.Middleware.Static
import System.IO
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html)
import Views.DomainList (domainListT)
import Views.DomainPrivileges (domainPrivilegesT)
import Views.ErrorPage (errorPageT)
import Views.GroupList (groupListT)
import Views.Homepage (homepageT)
import Views.MemberList (memberListT)
import Views.PrivilegeRules (privilegeRulesT)
import Views.Search (searchResultsT)
import Web.Scotty.Trans

import DB
import Entities
import LogFormat (logFormat)
import SproxyError

app :: Pool Connection -> FilePath -> IO Application
app c p = do
  logger <- mkRequestLogger def{ destination = Handle stderr
                               , outputFormat = CustomOutputFormat logFormat }
  scottyAppT id (sproxyWeb c p logger)

sproxyWeb :: Pool Connection -> FilePath -> Middleware -> ScottyT SproxyError IO ()
sproxyWeb pool dataDirectory logger = do
  middleware logger

  middleware (staticPolicy (hasPrefix "static" >-> addBase dataDirectory))

  -- error page for uncaught exceptions
  defaultHandler handleEx

  get "/" $ homepage pool

  post "/search"      $ searchUserH pool
  post "/delete-user" $ deleteUserH pool
  post "/rename-user" $ renameUserH pool

  -- groups
  get "/groups"      $ groupList pool        -- this is the group listing page
  get "/groups.json" $ jsonGroupList pool    -- json endpoint returning an array of group names
  post "/groups"     $ jsonUpdateGroup pool  -- endpoint where we POST requests for modifications of groups

  -- group
  get "/group/:group/members"  $ memberList pool       -- list of members for a group
  post "/group/:group/members" $ jsonPostMembers pool  -- endpoint for POSTing updates to the member list

  -- domains
  get "/domains"  $ domainList pool        -- list of domains handled by sproxy
  post "/domains" $ jsonUpdateDomain pool  -- endpoint for POSTing updates

  -- privileges for a given domain
  get "/domain/:domain/privileges"      $ domainPrivileges pool          -- listing of privileges available on a domain
  get "/domain/:domain/privileges.json" $ jsonDomainPrivileges pool      -- json endpoint, array of privilege names
  post "/domain/:domain/privileges"     $ jsonPostDomainPrivileges pool  -- endpoint for POSTing updates

  -- rules for a given privilege on a given domain
  get "/domain/:domain/privilege/:privilege/rules"  $ privilegeRules pool  -- listing of paths/methods associated to a privilege
  post "/domain/:domain/privilege/:privilege/rules" $ jsonPostRule pool    -- endpoint for POSTing updates about these

  -- add/remove group privileges
  post "/domain/:domain/group_privileges" $ handleGPs pool  -- endpoint for POSTing privilege granting/removal for groups


blaze :: Html -> ActionT SproxyError IO ()
blaze = Web.Scotty.Trans.html . renderHtml

handleEx :: SproxyError -> ActionT SproxyError IO ()
handleEx = errorPage

errorPage :: SproxyError -> ActionT SproxyError IO ()
errorPage err = blaze (errorPageT err)

homepage :: Pool Connection -> ActionT SproxyError IO ()
homepage _ = blaze homepageT

------------------------------------------
-- handlers related to the group list page
------------------------------------------

-- POST /groups
jsonUpdateGroup :: Pool Connection -> ActionT SproxyError IO ()
jsonUpdateGroup pool = do
    (operation :: Text) <- param "operation"
    (t, n) <- case operation of
             "add" -> do
                g <- param "group"
                checked pool (addGroup g)

             "del" -> do
                g <- param "group"
                checked pool (removeGroup g)

             "upd" -> do
                old <- param "old"
                new <- param "new"
                checked pool (updateGroup old new)

             _ -> status badRequest400 >> return ("incorrect operation", -1)

    outputFor t n

-- GET /groups
groupList :: Pool Connection -> ActionT SproxyError IO ()
groupList pool = do
    groups <- Prelude.map Prelude.head `fmap` withDB pool getGroups
    blaze (groupListT groups)

-- GET /groups.json
jsonGroupList :: Pool Connection -> ActionT SproxyError IO ()
jsonGroupList pool = do
    groups <- Prelude.map Prelude.head `fmap` withDB pool getGroups

    json groups

---------------------------------------------------
--  handlers related to the group members list page
---------------------------------------------------

-- GET /group/:group
memberList :: Pool Connection -> ActionT SproxyError IO ()
memberList pool = do
    groupName <- param "group"
    members   <- Prelude.map Prelude.head `fmap` withDB pool (getMembersFor groupName)
    blaze (memberListT members groupName)

-- POST /group/:group/members
jsonPostMembers :: Pool Connection -> ActionT SproxyError IO ()
jsonPostMembers pool = do
    groupName <- param "group"

    (operation :: Text) <- param "operation"

    (t, n) <- case operation of
        "add" -> do
            m <- param "member"
            checked pool (addMemberTo m groupName)

        "del" -> do
            m <- param "member"
            checked pool (removeMemberOf m groupName)

        "upd" -> do
            old <- param "old"
            new <- param "new"
            checked pool (updateMemberOf old new groupName)

        _     -> status badRequest400 >> return ("incorrect operation", -1)

    outputFor t n

--------------------------------------
-- handlers related to the domain list
--------------------------------------

-- GET /domains
domainList :: Pool Connection -> ActionT SproxyError IO ()
domainList pool = do
    domains <- Prelude.map Prelude.head `fmap` withDB pool getDomains
    blaze (domainListT domains)

-- POST /domains
jsonUpdateDomain :: Pool Connection -> ActionT SproxyError IO ()
jsonUpdateDomain pool = do
    (operation :: Text) <- param "operation"
    (t, n) <- case operation of
             "add" -> do
                d <- param "domain"
                checked pool (addDomain d)

             "del" -> do
                d <- param "domain"
                checked pool (removeDomain d)

             "upd" -> do
                old <- param "old"
                new <- param "new"
                checked pool (updateDomain old new)

             _ -> status badRequest400 >> return ("incorrect operation", -1)

    outputFor t n

-------------------------------------------------------------------------
-- handlers related to the list of possible privileges for a given domain
-------------------------------------------------------------------------

-- GET /domain/:domain/privileges
domainPrivileges :: Pool Connection -> ActionT SproxyError IO ()
domainPrivileges pool = do
    domain     <- param "domain"
    privileges <- Prelude.map Prelude.head `fmap` withDB pool (getDomainPrivileges domain)
    groups     <- Prelude.map Prelude.head `fmap` withDB pool getGroups
    groupPrivs <- withDB pool (getGroupPrivsFor domain)

    blaze (domainPrivilegesT domain privileges groups groupPrivs)

-- GET /domain/:domain/privileges.json
jsonDomainPrivileges :: Pool Connection -> ActionT SproxyError IO ()
jsonDomainPrivileges pool = do
    domain     <- param "domain"
    privileges <- Prelude.map Prelude.head `fmap` withDB pool (getDomainPrivileges domain)

    json privileges

-- POST /domain/:domain/group_privileges
handleGPs :: Pool Connection -> ActionT SproxyError IO ()
handleGPs pool = do
    domain <- param "domain"

    (operation :: Text) <- param "operation"

    (t, n) <- case operation of
        "add" -> do
            grp <- param "group"
            priv <- param "privilege"
            checked pool (addGPFor domain grp priv)

        "del" -> do
            grp <- param "group"
            priv <- param "privilege"
            checked pool (deleteGPOf domain grp priv)

        _ -> status badRequest400 >> return ("incorrect operation", -1)

    outputFor t n

-- POST /domain/:domain/privileges
jsonPostDomainPrivileges :: Pool Connection -> ActionT SproxyError IO ()
jsonPostDomainPrivileges pool = do
    domain <- param "domain"

    (operation :: Text) <- param "operation"

    (t, n) <- case operation of
        "add" -> do
            p <- param "privilege"
            checked pool (addPrivilegeToDomain p domain)

        "del" -> do
            p <- param "privilege"
            checked pool (removePrivilegeOfDomain p domain)

        "upd" -> do
            old <- param "old"
            new <- param "new"
            checked pool (updatePrivilegeOfDomain old new domain)

        _     -> status badRequest400 >> return ("incorrect operation", -1)

    outputFor t n

-------------------------------------------------------------------------------
-- handlers related to the rules associated to a given privilege on some domain
-------------------------------------------------------------------------------

-- GET /domain/:domain/privilege/:privilege/rules
privilegeRules :: Pool Connection -> ActionT SproxyError IO ()
privilegeRules pool = do
    -- TODO: check that the domain and privilege exist
    domain    <- param "domain"
    privilege <- param "privilege"
    rules     <- withDB pool (getRules domain privilege)

    blaze (privilegeRulesT domain privilege rules)

-- POST /domain/:domain/privilege/:privilege/rules
jsonPostRule :: Pool Connection -> ActionT SproxyError IO ()
jsonPostRule pool = do
    -- TODO: check that the domain and privilege exist
    domain    <- param "domain"
    privilege <- param "privilege"
    operation <- param "operation"
    case operation :: Text of
        "add" -> addRule domain privilege
        "del" -> delRule domain privilege
        "upd" -> updRule domain privilege
        _     -> status badRequest400 >> text "bad operation"

    where
      addRule domain privilege = do
          path   <- param "path"
          method <- param "method"

          (t, n) <- checked pool
                            (addRuleToPrivilege domain privilege path method)

          outputFor t n

      delRule domain privilege = do
          path   <- param "path"
          method <- param "method"

          (t, n) <- checked pool
                            (deleteRuleFromPrivilege domain privilege path method)

          outputFor t n

      updRule domain privilege = do
          what <- param "what"

          when (what /= "path" && what /= "method") $ text "invalid 'what'"

          let updFunc = if what == "path" then updatePathFor else updateMethodFor

          old <- param ("old" <> what)
          new <- param ("new" <> what)
          otherField <- param $ if what == "path" then "method" else "path"

          (t, n) <- checked pool
                            (updFunc domain privilege new old otherField)

          outputFor t n

-- | POST /search, search string in "search_query"
searchUserH :: Pool Connection -> ActionT SproxyError IO ()
searchUserH pool = do
  searchStr <- param "search_query"

  matchingEmails <- withDB pool (searchUser searchStr)

  blaze (searchResultsT searchStr matchingEmails)

-- | POST /delete-user, email to delete in "user_email"
deleteUserH :: Pool Connection -> ActionT SproxyError IO ()
deleteUserH pool = do
  userEmail <- param "user_email"
  (t, n) <- checked pool (removeUser userEmail)
  outputFor t n

-- | POST /rename-user:
--  - old email in "old_email"
--  - new email in "new_email"
renameUserH :: Pool Connection -> ActionT SproxyError IO ()
renameUserH pool = do
  oldEmail   <- param "old_email"
  newEmail <- param "new_email"
  (resp, n) <- checked pool (renameUser oldEmail newEmail)
  outputFor resp n

-- utility functions

outputFor :: Text -> Int64 -> ActionT SproxyError IO ()
outputFor t 0    = status badRequest400 >> text ("no: " <> t)
outputFor t (-1) = status badRequest400 >> text ("error: " <> t)
outputFor t _    = text t

checked :: Pool Connection
        -> (Connection -> IO Int64) -- request
        -> ActionT SproxyError IO (Text, Int64)
checked pool req =
    withDB pool req'

    where req' c = handle (\(e :: SomeException) -> return (Text.pack (show e), -1))
                              ( ("",) <$> req c )

