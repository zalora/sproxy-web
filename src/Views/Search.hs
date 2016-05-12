{-# LANGUAGE OverloadedStrings #-}

module Views.Search where

import           Data.Text.Lazy (Text)

import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text.Lazy as T
import           Views.Common

searchResultsT :: Text -> [(Text, [Text])] -> Html
searchResultsT searchStr matchingMails = 
    pageT ("Search results") $ do
        H.p ! A.class_ "lead text-center" $ do
            "Emails matching "
            H.i $ "\"" >> toHtml searchStr >> "\""

        H.p ! A.class_ "text-center" $ H.i $
          "(Click on an email to \"rename\" the user in the sproxy database.)"

        H.div ! A.class_ "text-center" ! A.style "margin: 10px auto;" $ 
          H.table ! A.id "searchtable" ! A.class_ "table table-condensed" $ do
            H.thead $ do
              H.tr $ do
                H.th ! A.width "30%" ! A.class_ "text-center" $ 
                  "Email address"
                H.th ! A.width "40%" ! A.class_ "text-center" $
                  "Groups"
                H.th ! A.width "30%" $ mempty
              H.tbody $
                emailsHtml

        H.div ! A.class_ "alert alert-success" ! A.id "updatesuccess" $
            "Successfully updated."

        H.div ! A.class_ "alert alert-error" ! A.id "updateexists" $
           "This is already used."

        H.div ! A.class_ "alert alert-error" ! A.id "updateerror" $
            "An error occured when trying to update the email in the DB."
    
        H.script ! A.type_ "text/javascript"
                    ! A.src "/static/js/delete-user.js" $ mempty

        H.script ! A.type_ "text/javascript"
                    ! A.src "/static/js/rename-user.js" $ mempty

  where emailsHtml = mapM_ emailToHtml matchingMails
        emailToHtml (mail, groups) = 
          tr $ do
            td ! A.class_ "edit email-edit" $ toHtml mail
            td $ i $ toHtml (T.intercalate ", " groups)
            td $ a ! A.class_ "delete-btn btn btn-danger btn-xs" $ "Delete from all groups"
