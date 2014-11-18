{-# LANGUAGE OverloadedStrings #-}

module Views.PrivilegeRules (privilegeRulesT) where

import           Data.Text.Lazy (Text)
import           Data.Monoid

import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           Views.Common

privilegeRulesT :: Text -> Text -> [(Text, Text)] -> Html
privilegeRulesT domain privilege rules =
    pageT ("Rules for privilege '" <> privilege <> "' on domain '" <> domain <> "'") $ do

        H.script ! A.type_ "text/javascript" $ do
            "var domain = '"
            toHtml domain
            "';"

            "var privilege = '"
            toHtml privilege
            "';"

        H.p ! A.class_ "lead text-center" $ do
            "Add, edit or delete rules for "
            H.b $ toHtml privilege
            " on "
            H.b $ toHtml domain

        H.div ! A.class_ "text-center" ! A.style "margin: 10px auto;" $ do
            H.table ! A.id "edittable" 
                    ! A.class_ "table table-condensed" $ do
                H.thead $ do
                    H.tr $ do
                        H.th ! A.width "50%" ! A.class_ "text-center" $ 
                            "Path"
                        H.th ! A.width "30%" ! A.class_ "text-center" $
                            "Method"
                        H.th ! A.width "20%" $ mempty
                H.tbody $ do
                    mapM_ ruleToHtml rules
                H.tfoot $ do
                    H.tr $ do
                        H.td $ inp "path" "Path"
                        H.td $ inp "method" "Method"
                        H.td $ H.button ! A.class_ "btn btn-success btn-xs add-btn"
                                        $ "Add a rule"

            H.div ! A.class_ "alert alert-success" ! A.id "updatesuccess" $
                "Operation successful."

            H.div ! A.class_ "alert alert-error" ! A.id "updateerror" $
                "An error occured when trying to update the database."

            H.div ! A.class_ "alert alert-error" ! A.id "fieldempty" $
                "You left one of the fields empty."

            H.div ! A.class_ "alert alert-error" ! A.id "updateexists" $
                "This value already exists"

        H.script ! A.type_ "text/javascript"
                 ! A.src "/static/js/rules.js" $ mempty

    where ruleToHtml (path, method) = do
              tr $ do
                td ! A.class_ "path-edit edit" $ toHtml path
                td ! A.class_ "method-edit edit" $ toHtml method
                td $ a ! A.class_ "delete-btn btn btn-danger btn-xs" $ "Delete"
