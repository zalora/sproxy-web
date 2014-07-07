{-# LANGUAGE OverloadedStrings #-}

module Views.Homepage (homepageT) where

import Data.Monoid (mempty)
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Views.Common (pageT)

homepageT :: Html
homepageT = 
    pageT "Welcome" $ do
        H.p ! A.class_ "lead" $ do
            "This is the web interface to the sproxy service. "
            "You can directly add, edit and remove groups, domains, privileges, users and much more! "
            "Just use the "
            H.i "Groups and users"
            " and "
            H.i "Domains and privileges"
            " links, "
            H.a ! A.href "#" ! A.class_ "up-there" $ "up there"
            "."
        H.p ! A.class_ "lead text-center" $ do
            "Subscribe to "
            H.i $ "sproxy-web"
            " for only " 
            H.b $ "$5/month"
            "!"

        H.script ! A.type_ "text/javascript" ! A.src "/static/js/up-there.js" $ mempty