{-# LANGUAGE OverloadedStrings #-}

module Views.Common (pageT, inp, sel) where

import           Data.Monoid
import           Data.Text.Lazy (Text)
import           Text.Blaze.Html5 hiding (option)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes hiding (name)
import qualified Text.Blaze.Html5.Attributes as A

inp :: Text -> Text -> Html 
inp name text = 
    H.input ! A.type_ "text" ! A.name (toValue name)
            ! A.id (toValue name) ! A.value (toValue text)

sel :: Text -> [Text] -> Html
sel name options = 
    H.select ! A.name (toValue name) ! A.id (toValue name) $
        mapM_ optToHtml options

    where optToHtml option = 
              H.option ! A.value (toValue option) $ toHtml option

pageT :: Text -> Html -> Html
pageT t cont = do
    docTypeHtml ! lang "en" $ do
        H.head $ do
            H.title (toHtml t)
            meta ! A.name "viewport" ! content "width=device-width, initial-scale=1"
            --  Latest compiled and minified CSS 
            link ! rel "stylesheet" ! href "/static/css/bootstrap.min.css"
            --  Optional theme 
            link ! rel "stylesheet" ! href "/static/css/bootstrap-theme.min.css"
            --  My own CSS file 
            link ! rel "stylesheet" ! href "/static/css/style.css"
            
            -- required js files
            script ! A.type_ "text/javascript" ! src "/static/js/jquery.min.js" $ mempty
            script ! A.type_ "text/javascript" ! src "/static/js/jquery.color.min.js" $ mempty
            script ! A.type_ "text/javascript" ! src "/static/js/bootstrap.min.js" $ mempty
            script ! A.type_ "text/javascript" ! src "/static/js/longtable.js" $ mempty
            script ! A.type_ "text/javascript" ! src "/static/js/jquery.jeditable.min.js" $ mempty
            script ! A.type_ "text/javascript" ! src "/static/js/utils.js" $ mempty

        body $ do

            H.div ! A.class_ "navbar navbar-inverse navbar-fixed-top" $ do
              H.div ! A.class_ "container" $ do

                -- header for the navbar
                H.div ! A.class_ "navbar-header" $ do
                  H.button ! A.type_ "button" ! A.class_  "navbar-toggle" 
                           ! dataAttribute "toggle" "collapse" 
                           ! dataAttribute "target" ".navbar-collapse" $ do
                    H.span ! A.class_ "sr-only" $ "Toggle navigation"
                    H.span ! A.class_ "icon-bar" $ mempty
                    H.span ! A.class_ "icon-bar" $ mempty
                    H.span ! A.class_ "icon-bar" $ mempty
                  H.a ! A.class_ "navbar-brand" ! A.href "/" $ "sproxy-web"

                H.div ! A.class_ "collapse navbar-collapse" $ do
                  H.ul ! A.class_ "nav navbar-nav" $ do
                    H.li ! class_ {- "active" -} "" $ H.a ! A.href "/" $ "Home"
                    H.li ! A.id "animat-gr" $ H.a ! A.href "/groups" $ "Groups and users"
                    H.li ! A.id "animat-do" $ H.a ! A.href "/domains" $ "Domains and privileges"

                  H.form ! A.id "searchform" ! A.class_ "navbar-form navbar-right"
                         ! A.method "POST"   ! A.action "/search" $
                    H.input ! A.type_ "text"
                            ! A.name  "search_query"
                            ! A.class_ "form-control"
                            ! A.placeholder "Search an email"

            H.div ! class_ "container content" $ do
                H.div ! A.class_ "page-header text-center" $ 
                    H.h1 $ toHtml t
                cont
 
            --  Latest compiled and minified JavaScript
            -- script ! A.type_ "text/javascript" ! src "/static/js/jquery-1.10.2.min.js" $ mempty
