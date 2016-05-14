{-# LANGUAGE OverloadedStrings #-}

module Views.DomainList (domainListT) where

import           Data.Text.Lazy (Text)

import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import           Views.Common

domainListT :: [Text] -> Html
domainListT gs = 
    pageT "Manage domains" $ do
        H.p ! A.class_ "lead text-center" $
            "Add, edit or delete domains."

        H.div ! A.class_ "text-center" ! A.style "margin: 10px auto;" $
            H.table ! A.id "edittable" 
                    ! A.class_ "table table-condensed" $ do
                H.thead $ do
                    H.tr $ do
                        H.th ! A.width "50%" ! A.class_ "text-center" $ 
                            "Domain name"
                        H.th ! A.width "30%" $ mempty
                        H.th ! A.width "20%" $ mempty
                H.tbody $ do
                    mapM_ domainToHtml gs
                H.tfoot $ do
                    H.tr $ do
                        H.td $ inp "domain" "example.org"
                        H.td $ H.button ! A.class_ "btn btn-success btn-xs add-btn"
                                        $ "Add a domain"
                        H.td $ mempty

        H.div ! A.class_ "alert alert-success" ! A.id "updatesuccess" $
            "Successfully updated."

        H.div ! A.class_ "alert alert-error" ! A.id "updateerror" $
            "An error occured when trying to update the domains in the DB."

        H.div ! A.class_ "alert alert-error" ! A.id "fieldempty" $
            "You left one of the fields empty."

        H.script ! A.type_ "text/javascript"
                 ! A.src "/static/js/domainlist.js" $ mempty

    where domainToHtml d = do
              tr $ do
                td ! A.class_ "edit domain-edit" $ toHtml d
                td $
                    a ! A.class_ "delete-btn btn btn-xs btn-danger" $ "Delete"
                td $ 
                    a ! href "#"
                      ! A.class_ "privileges-btn btn btn-link btn-xs"
                      $ "Privileges"
