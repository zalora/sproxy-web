{-# LANGUAGE OverloadedStrings #-}

module Views.DomainPrivileges (domainPrivilegesT) where

import Data.Text.Lazy (Text)
import Data.Monoid

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import Views.Common

domainPrivilegesT :: Text -> [Text] -> [Text] -> [(Text, Text)] -> Html
domainPrivilegesT domain privileges groups groupPrivs =
    pageT ("Privileges for domain '" <> domain <> "'") $ do

        H.script ! A.type_ "text/javascript" $ do
            "var domain = '"
            toHtml domain
            "';"

        H.div ! A.class_ "row" $ do

            H.div ! A.class_ "col-md-6" $ do

                H.p ! A.class_ "lead text-center" $ do
                    "Add, edit or delete privileges for "
                    H.b $ toHtml domain

                H.div ! A.class_ "text-center" ! A.style "margin: 10px auto;" $
                    H.table ! A.id "edittable" 
                            ! A.class_ "table table-condensed" $ do
                        H.thead $ do
                            H.tr $ do
                                H.th ! A.width "50%" ! A.class_ "text-center" $ 
                                    "Privilege"
                                H.th ! A.width "30%" $ mempty
                                H.th ! A.width "20%" $ mempty
                        H.tbody $ do
                            mapM_ privToHtml privileges
                        H.tfoot $ do
                            H.tr $ do
                                H.td $ inp "privilege" "Privilege"
                                H.td $ H.button ! A.class_ "btn btn-success btn-xs add-btn"
                                                $ "Add a privilege"
                                H.td $ mempty

                H.div ! A.class_ "alert alert-success" ! A.id "updatesuccess" $
                    "Successfully updated."

                H.div ! A.class_ "alert alert-error" ! A.id "updateerror" $
                    "An error occured when trying to update the privileges in the DB."

                H.div ! A.class_ "alert alert-error" ! A.id "fieldempty" $
                    "You left one of the fields empty."

            H.div ! A.class_ "col-md-6" $ do

                H.p ! A.class_ "lead text-center" $ do
                    "Grant or remove group privileges on "
                    H.b $ toHtml domain

                H.div ! A.class_ "text-center" ! A.style "margin: 10px auto;" $
                    H.table ! A.id "edittable2" 
                            ! A.class_ "table table-condensed" $ do
                        H.thead $ do
                            H.tr $ do
                                H.th ! A.width "40%" ! A.class_ "text-center" $ 
                                    "Group"
                                H.th ! A.width "40%" ! A.class_ "text-center" $
                                    "Privilege"
                                H.th ! A.width "20%" $ mempty
                        H.tbody $ do
                            mapM_ groupPrivToHtml groupPrivs
                        H.tfoot $ do
                            H.tr $ do
                                H.td $ sel "groupSel" groups
                                H.td $ sel "privSel"  privileges
                                H.td $ H.button ! A.class_ "btn btn-success btn-xs grant-btn"
                                                $ "Grant access"

                H.div ! A.class_ "alert alert-success" ! A.id "gpsuccess" $
                    "Successfully updated."

                H.div ! A.class_ "alert alert-error" ! A.id "gperror" $
                    "An error occured when trying to update the privileges in the DB."

        H.script ! A.type_ "text/javascript"
                 ! A.src "/static/js/domainprivileges.js" $ mempty

    where privToHtml p = do
              tr $ do
                td ! A.class_ "edit privilege-edit" $ toHtml p
                td $ 
                    a ! A.class_ "delete-btn btn btn-danger btn-xs" $ "Delete"
                td $
                    a ! href "#"
                      ! class_ "rule-btn btn btn-link btn-xs"
                      $ "Rules"

          groupPrivToHtml (group, priv) = do
              tr $ do
                td $ toHtml group
                td $ toHtml priv
                td $ 
                    a ! A.class_ "delete-gp-btn btn btn-danger btn-xs" $ "Delete"