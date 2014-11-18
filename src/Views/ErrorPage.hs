{-# LANGUAGE OverloadedStrings #-}

module Views.ErrorPage (errorPageT) where

import           Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes

import           SproxyError
import           Views.Common

errorPageT :: SproxyError -> Html
errorPageT err = do
    pageT "Error!" $ do
            H.div ! class_ "alert alert-error text-center" $ do
                p $ toHtml (show err)
                p $ a ! href "##" ! onclick "history.go(-1); return false;" $ "Go back"
