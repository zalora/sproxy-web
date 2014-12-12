{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module ConfigSpec where


import           Control.Exception
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           HFlags
import           System.Directory
import           System.IO.Temp
import           Test.Hspec

import           Config


spec :: Spec
spec = do
  describe "getConfig" $ do
    it "parses the example config file" $ do
      config <- getConfig "example.config"
      (dbConnectionString config, port config) `shouldBe` ("hostaddr=127.0.0.1 user=alp dbname=alp", 8001)


inTempDirectory :: IO a -> IO a
inTempDirectory action = withSystemTempDirectory "sproxy-web-test" $ \ tempDir ->
  bracket (start tempDir) stop (const action)
 where
  start tempDir = do
    outerDir <- getCurrentDirectory
    setCurrentDirectory tempDir
    return outerDir
  stop outerDir = do
    setCurrentDirectory outerDir
