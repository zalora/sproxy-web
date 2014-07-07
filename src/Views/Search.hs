{-# LANGUAGE OverloadedStrings #-}

module Views.Search where

import Data.Text.Lazy (Text)
import Data.Monoid

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import Views.Common

searchResultsT :: Text -> [Text] -> Html
searchResultsT searchStr emails = 
    pageT ("Search results") $ do
        H.p ! A.class_ "lead text-center" $ do
            "Emails matching "
            H.i $ "\"" >> toHtml searchStr >> "\""
            

        H.div ! A.class_ "text-center" ! A.style "margin: 10px auto;" $ 
          H.table ! A.id "searchtable" ! A.class_ "table table-condensed" $ do
            H.thead $ do
              H.tr $ do
                H.th ! A.width "40%" ! A.class_ "text-center" $ 
                  "Email address"
                H.th ! A.width "60%" $ mempty
              H.tbody $
                emailsHtml

        H.div ! A.class_ "alert alert-success" ! A.id "updatesuccess" $
            "Successfully updated."

        H.div ! A.class_ "alert alert-error" ! A.id "updateerror" $
            "An error occured when trying to update the groups in the DB."
    
        H.script ! A.type_ "text/javascript"
                 ! A.src "/static/js/delete-user.js" $ mempty

  where emailsHtml = mapM_ emailToHtml emails
        emailToHtml mail = 
          tr $ do
            td $ toHtml mail
            td $ a ! A.class_ "delete-btn btn btn-danger btn-xs" $ "Delete from all groups"
