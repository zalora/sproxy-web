{-# LANGUAGE OverloadedStrings #-}

module MainSpec where

import Test.Hspec
import Network.Wai
import Test.Hspec.Wai
import Web.Scotty.Trans (scottyAppT)

import Config
import DB
import Run

app :: IO Application
app = do
    pool <- createDBPool "something"
    sDir <- getStaticDir
    scottyAppT id id $ sproxyWeb sDir pool

spec :: Spec
spec = with app $ do
  describe "sproxyWeb" $ do
    it "doesn't serve source files" $ do
      get "/sproxy-web.cabal" `shouldRespondWith` 404
    it "serve static file" $ do
      get "/static/loading.gif" `shouldRespondWith` 200
    it "returns 404 for non-existing routes" $ do
      get "/foo" `shouldRespondWith` 404
