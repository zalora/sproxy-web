{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module ConfigSpec where


import           Control.Exception
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           HFlags
import           System.Directory
import           System.Environment
import           System.IO.Temp
import           Test.Hspec

import           Config


spec :: Spec
spec = do
  describe "getConfig" $ do
    it "uses 'sproxy-web.config' as default config file" $ inTempDirectory $ do
      writeFile "sproxy-web.config" $ unindent [i|
          db_connection_string = "foo"
          port = 42
        |]
      _ <- $initHFlags "sproxy-web - Web interface to the sproxy permissions database"
      getConfig flags_config `shouldReturn` Config "foo" 42

    it "parses the example config file" $ do
      getConfig "example.config" `shouldReturn`
        Config "hostaddr=127.0.0.1 user=alp dbname=alp" 8001


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
