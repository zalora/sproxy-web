{-# LANGUAGE OverloadedStrings #-}

module Views.GroupList (groupListT) where

import           Data.Text.Lazy (Text)
import           Data.Monoid

import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import           Views.Common

groupListT :: [Text] -> Html
groupListT gs = 
    pageT "Manage groups" $ do
        H.p ! A.class_ "lead text-center" $
            "Add, edit or delete user groups."

        H.div ! A.class_ "text-center" ! A.style "margin: 10px auto;" $
            H.table ! A.id "edittable" 
                    ! A.class_ "table table-condensed" $ do
                H.thead $ do
                    H.tr $ do
                        H.th ! A.width "50%" ! A.class_ "text-center" $ 
                            "Group name"
                        H.th ! A.width "30%" $ mempty
                        H.th ! A.width "20%" $ mempty
                H.tbody $ do
                    mapM_ groupToHtml gs
                H.tfoot $ do
                    H.tr $ do
                        H.td $ inp "gname" "Group"
                        H.td $ H.button ! A.class_ "btn btn-success btn-xs add-btn"
                                        $ "Add a group"
                        H.td $ mempty

        H.div ! A.class_ "alert alert-success" ! A.id "updatesuccess" $
            "Successfully updated."

        H.div ! A.class_ "alert alert-error" ! A.id "updateerror" $
            "An error occured when trying to update the groups in the DB."
    
        H.div ! A.class_ "alert alert-error" ! A.id "fieldempty" $
            "You left one of the fields empty."
    
        H.script ! A.type_ "text/javascript"
                 ! A.src "/static/js/grouplist.js" $ mempty

    where groupToHtml gname = do
              tr $ do
                td ! A.class_ "edit group-edit" $ toHtml gname
                td $
                    a ! A.class_ "delete-btn btn btn-danger btn-xs" $ "Delete"
                td $ 
                    a ! href "#"
                      ! A.class_ "member-btn btn btn-link btn-xs"
                      $ "Members"
