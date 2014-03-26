{-# LANGUAGE OverloadedStrings #-}

module Entities where

import Data.Int
import Data.Text.Lazy (Text)
import Database.PostgreSQL.Simple

-- get the list of groups
getGroups :: Connection -> IO [[Text]]
getGroups conn =
    query_ conn "select * from \"group\" order by \"group\" asc"

-- change a group's name
updateGroup :: Text -> Text -> Connection -> IO Int64
updateGroup old new conn =
    execute conn "UPDATE \"group\" \
                 \ SET \"group\" = ? \
                 \ WHERE \"group\" = ?"
                 (new, old)

-- add a new group
addGroup :: Text -> Connection -> IO Int64
addGroup gname conn =
    execute conn "INSERT INTO \"group\" \
                 \ VALUES (?)"
                 (Only gname)

-- remove a group
removeGroup :: Text -> Connection -> IO Int64
removeGroup gname conn =
    execute conn "DELETE FROM \"group\" \
                 \ WHERE \"group\" = ?"
                 (Only gname)

-- get members of a group
getMembersFor :: Text -> Connection -> IO [[Text]]
getMembersFor gname conn =
    query conn "SELECT email FROM group_member \
               \ WHERE \"group\" = ? \
               \ ORDER BY email ASC"
               (Only gname)

-- add a member to a group
addMemberTo :: Text -> Text -> Connection -> IO Int64
addMemberTo new groupName conn =
    execute conn "INSERT INTO group_member (\"group\", email) \
               \ VALUES (?, ?)"
               (groupName, new)

-- remove a user from a group
removeMemberOf :: Text -> Text -> Connection -> IO Int64
removeMemberOf member groupName conn =
    execute conn "DELETE FROM group_member \
                 \ WHERE \"group\" = ? AND email = ?"
                 (groupName, member)

-- update a (meta-)user's email
updateMemberOf :: Text -> Text -> Text -> Connection -> IO Int64
updateMemberOf old new groupName conn =
    execute conn "UPDATE group_member \ 
                 \ SET email = ? \
                 \ WHERE \"group\" = ? AND email = ?"
                 (new, groupName, old)

-- list the different domains
getDomains :: Connection -> IO [[Text]]
getDomains conn = 
    query_ conn "SELECT domain \ 
                \ FROM domain \ 
                \ ORDER BY domain ASC"

-- add a domain
addDomain :: Text -> Connection -> IO Int64
addDomain d conn =
    execute conn "INSERT INTO domain (domain) \
                 \ VALUES (?)"
                 (Only d)

-- update a domain's "name"
updateDomain :: Text -> Text -> Connection -> IO Int64
updateDomain old new conn =
    execute conn "UPDATE domain \ 
                 \ SET domain = ? \
                 \ WHERE domain = ?"
                 (new, old)

-- remove a domain
removeDomain :: Text -> Connection -> IO Int64
removeDomain d conn =
    execute conn "DELETE FROM domain \ 
                 \ WHERE domain = ?"
                 (Only d)

-- get the privileges associated to a domain
getDomainPrivileges :: Text -> Connection -> IO [[Text]]
getDomainPrivileges domain conn =
    query conn "SELECT privilege \ 
               \ FROM privilege \ 
               \ WHERE domain = ? \ 
               \ ORDER BY privilege ASC"
               (Only domain)

-- update the name of a privilege associated to a domain
updatePrivilegeOfDomain :: Text -> Text -> Text -> Connection -> IO Int64
updatePrivilegeOfDomain old new domain conn =
    execute conn "UPDATE privilege \ 
                 \ SET privilege = ? \ 
                 \ WHERE domain = ? AND privilege = ?"
                 (new, domain, old)

-- add a privilege for the given domain
addPrivilegeToDomain :: Text -> Text -> Connection -> IO Int64
addPrivilegeToDomain new domain conn =
    execute conn "INSERT INTO privilege (domain, privilege) \ 
                 \ VALUES (?, ?)"
                 (domain, new)

-- remove a privilege for the given domain
removePrivilegeOfDomain :: Text -> Text -> Connection -> IO Int64                 
removePrivilegeOfDomain old domain conn =
    execute conn "DELETE FROM privilege \ 
                 \ WHERE domain = ? AND privilege = ?"
                 (domain, old)

-- list the path/method rules for a given privilege on a given domain
getRules :: Text -> Text -> Connection -> IO [(Text, Text)]
getRules domain privilege conn =
    query conn "SELECT path, method \ 
               \ FROM privilege_rule \ 
               \ WHERE domain = ? \
               \   AND privilege = ? \ 
               \ ORDER BY path, method ASC"
               (domain, privilege)

-- add a path/method rule for a given privilege on a given domain
addRuleToPrivilege :: Text -> Text -> Text -> Text -> Connection -> IO Int64
addRuleToPrivilege domain privilege path method conn =
    execute conn "INSERT INTO privilege_rule \
                 \   (domain, privilege, path, method) \
                 \ VALUES \
                 \   (?, ?, ?, ?)"
                 (domain, privilege, path, method)

-- delete a privilege rule
deleteRuleFromPrivilege :: Text -> Text -> Text -> Text -> Connection -> IO Int64
deleteRuleFromPrivilege domain privilege path method conn =
    execute conn "DELETE FROM privilege_rule \ 
                 \ WHERE domain = ? \
                 \   AND privilege = ? \
                 \   AND path = ? \
                 \   AND method = ?"
                 (domain, privilege, path, method)

-- update just the path for a given rule
updatePathFor :: Text -> Text -> Text -> Text -> Text -> Connection -> IO Int64
updatePathFor domain privilege newpath oldpath method conn =
    execute conn "UPDATE privilege_rule \ 
                 \ SET path = ? \ 
                 \ WHERE domain = ? \
                 \   AND privilege = ? \ 
                 \   AND path = ? \ 
                 \   AND method = ?"
                 (newpath, domain, privilege, oldpath, method)

-- update just the method for a given rule
updateMethodFor :: Text -> Text -> Text -> Text -> Text -> Connection -> IO Int64
updateMethodFor domain privilege newmethod oldmethod path conn = 
    execute conn "UPDATE privilege_rule \ 
                 \ SET method = ? \ 
                 \ WHERE domain = ? \ 
                 \   AND privilege = ? \ 
                 \   AND path = ? \ 
                 \   AND method = ?"
                 (newmethod, domain, privilege, path, oldmethod)

-- get the list of privileges granted to groups for a given domain
getGroupPrivsFor :: Text -> Connection -> IO [(Text, Text)]
getGroupPrivsFor domain conn =
    query conn "SELECT \"group\", privilege \
               \ FROM group_privilege \
               \ WHERE domain = ? \ 
               \ ORDER BY \"group\", privilege ASC"
               (Only domain)

-- grant a privilege to a group on a domain
addGPFor :: Text -> Text -> Text -> Connection -> IO Int64
addGPFor domain gr priv conn = 
    execute conn "INSERT INTO group_privilege \
                 \  (\"group\", domain, privilege) \
                 \ VALUES (?, ?, ?)"
                 (gr, domain, priv)

-- remove a privilege for a group on a domain
deleteGPOf :: Text -> Text -> Text -> Connection -> IO Int64
deleteGPOf domain gr priv conn =
    execute conn "DELETE FROM group_privilege \ 
                 \ WHERE domain = ? \ 
                 \   AND \"group\" = ? \
                 \   AND privilege = ?"
                 (domain, gr, priv)
