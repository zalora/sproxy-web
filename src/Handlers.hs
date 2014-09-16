{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}

module Handlers where

import DB
import Entities
import SproxyError

import Control.Applicative
import Control.Exception
import Control.Monad (when)
import Data.Int (Int64)
import Data.Monoid
import Data.Text.Lazy as Text
import Network.HTTP.Types.Status
import Web.Scotty.Trans

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

blaze :: Html -> ActionT SproxyError IO ()
blaze = Web.Scotty.Trans.html . renderHtml

handleEx :: SproxyError -> ActionT SproxyError IO ()
handleEx = errorPage

errorPage :: SproxyError -> ActionT SproxyError IO ()
errorPage err = blaze (errorPageT err)

homepage :: DBPool -> ActionT SproxyError IO ()
homepage _ = blaze homepageT

------------------------------------------
-- handlers related to the group list page
------------------------------------------

-- POST /groups
jsonUpdateGroup :: DBPool -> ActionT SproxyError IO ()
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

             _ -> status badRequest400 >> return ("incorrect operation", (-1))

    outputFor t n

-- GET /groups
groupList :: DBPool -> ActionT SproxyError IO ()
groupList pool = do
    groups <- Prelude.map Prelude.head `fmap` withDB pool getGroups 
    blaze (groupListT groups)

-- GET /groups.json
jsonGroupList :: DBPool -> ActionT SproxyError IO ()
jsonGroupList pool = do
    groups <- Prelude.map Prelude.head `fmap` withDB pool getGroups

    json groups

---------------------------------------------------
--  handlers related to the group members list page
---------------------------------------------------

-- GET /group/:group
memberList :: DBPool -> ActionT SproxyError IO ()
memberList pool = do
    groupName <- param "group"
    members   <- Prelude.map Prelude.head `fmap` withDB pool (getMembersFor groupName)
    blaze (memberListT members groupName)

-- POST /group/:group/members
jsonPostMembers :: DBPool -> ActionT SproxyError IO ()
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

        _     -> status badRequest400 >> return ("incorrect operation", (-1))

    outputFor t n

--------------------------------------
-- handlers related to the domain list
--------------------------------------

-- GET /domains
domainList :: DBPool -> ActionT SproxyError IO ()
domainList pool = do
    domains <- Prelude.map Prelude.head `fmap` withDB pool getDomains
    blaze (domainListT domains)

-- POST /domains
jsonUpdateDomain :: DBPool -> ActionT SproxyError IO ()
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

             _ -> status badRequest400 >> return ("incorrect operation", (-1))

    outputFor t n

-------------------------------------------------------------------------
-- handlers related to the list of possible privileges for a given domain
-------------------------------------------------------------------------

-- GET /domain/:domain/privileges
domainPrivileges :: DBPool -> ActionT SproxyError IO ()
domainPrivileges pool = do
    domain     <- param "domain"
    privileges <- Prelude.map Prelude.head `fmap` withDB pool (getDomainPrivileges domain)
    groups     <- Prelude.map Prelude.head `fmap` withDB pool getGroups
    groupPrivs <- withDB pool (getGroupPrivsFor domain)

    blaze (domainPrivilegesT domain privileges groups groupPrivs)

-- GET /domain/:domain/privileges.json
jsonDomainPrivileges :: DBPool -> ActionT SproxyError IO ()
jsonDomainPrivileges pool = do
    domain     <- param "domain"
    privileges <- Prelude.map Prelude.head `fmap` withDB pool (getDomainPrivileges domain)

    json privileges

-- POST /domain/:domain/group_privileges
handleGPs :: DBPool -> ActionT SproxyError IO ()
handleGPs pool = do
    domain <- param "domain"

    (operation :: Text) <- param "operation"

    (t, n) <- case operation of
        "add" -> do
            grp <- param "group"
            priv  <- param "privilege"
            checked pool (addGPFor domain grp priv)

        "del" -> do
            grp <- param "group"
            priv  <- param "privilege"
            checked pool (deleteGPOf domain grp priv)

    outputFor t n

-- POST /domain/:domain/privileges
jsonPostDomainPrivileges :: DBPool -> ActionT SproxyError IO ()
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

        _     -> status badRequest400 >> return ("incorrect operation", (-1))

    outputFor t n

-------------------------------------------------------------------------------
-- handlers related to the rules associated to a given privilege on some domain
-------------------------------------------------------------------------------

-- GET /domain/:domain/privilege/:privilege/rules
privilegeRules :: DBPool -> ActionT SproxyError IO ()
privilegeRules pool = do
    -- TODO: check that the domain and privilege exist
    domain    <- param "domain"
    privilege <- param "privilege"
    rules     <- withDB pool (getRules domain privilege)

    blaze (privilegeRulesT domain privilege rules)

-- POST /domain/:domain/privilege/:privilege/rules
jsonPostRule :: DBPool -> ActionT SproxyError IO ()
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
searchUserH :: DBPool -> ActionT SproxyError IO ()
searchUserH pool = do
  searchStr <- param "search_query"

  matchingEmails <- withDB pool (searchUser searchStr)

  blaze (searchResultsT searchStr matchingEmails)

-- | POST /delete-user, email to delete in "user_email"
deleteUserH :: DBPool -> ActionT SproxyError IO ()
deleteUserH pool = do
  userEmail <- param "user_email"
  (t, n) <- checked pool (removeUser userEmail)
  outputFor t n

-- | POST /rename-user:
--  - old email in "old_email"
--  - new email in "new_email"
renameUserH :: DBPool -> ActionT SproxyError IO ()
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

checked :: DBPool
        -> (Connection -> IO Int64) -- request
        -> ActionT SproxyError IO (Text, Int64)
checked pool req = 
    withDB pool req'

    where req' c = flip catch (\(e :: SomeException) -> return (Text.pack (show e), -1))
                              ( ("",) <$> req c )
