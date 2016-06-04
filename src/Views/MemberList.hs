{-# LANGUAGE OverloadedStrings #-}

module Views.MemberList (memberListT) where

import           Data.Text.Lazy (Text)
import           Data.Monoid

import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           Views.Common

memberListT :: [Text] -> Text -> Html
memberListT members groupName =
    pageT ("Members of the group '" <> groupName <> "'") $ do

        H.script ! A.type_ "text/javascript" $ do
            "var groupName = '"
            toHtml groupName
            "';"

        H.p ! A.class_ "lead text-center" $ do
            "Add, edit or delete members of "
            H.b $ toHtml groupName

        H.div ! A.class_ "text-center" ! A.style "margin: 10px auto;" $
            H.table ! A.id "edittable" 
                    ! A.class_ "table table-condensed" $ do
                H.thead $
                    H.tr $ do
                        H.th ! A.width "70%" ! A.class_ "text-center" $ 
                            "Member"
                        H.th ! A.width "30%" $ mempty
                H.tbody $
                    mapM_ memberToHtml members
                H.tfoot $
                    H.tr $ do
                        H.td $ inp "member" "%@example.com"
                        H.td $ H.button ! A.class_ "btn btn-success btn-xs add-btn"
                                        $ "Add a member"

        H.div ! A.class_ "alert alert-success" ! A.id "updatesuccess" $
            "Successfully updated."

        H.div ! A.class_ "alert alert-error" ! A.id "updateerror" $
            "An error occured when trying to update the members in the DB."

        H.div ! A.class_ "alert alert-error" ! A.id "fieldempty" $
            "You left one of the fields empty."

        H.script ! A.type_ "text/javascript"
                 ! A.src "/static/js/memberlist.js" $ mempty

    where memberToHtml m =
              tr $ do
                td ! A.class_ "edit member-edit" $ toHtml m
                td $ a ! A.class_ "delete-btn btn btn-danger btn-xs" $ "Delete"

