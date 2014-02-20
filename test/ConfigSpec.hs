{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ConfigSpec where

import Control.Applicative ((<$>))

import qualified Data.Aeson as AE
import qualified Data.Text as T

import Config
import SpecHelper

instance Default AojConf where
  def = AojConf {
    user = "aoj_user",
    pass = "aoj_pass"
    }

instance Arbitrary AojConf where
  arbitrary = do
    user' <- arbitrary
    pass' <- arbitrary
    return $ AojConf { user = user', pass = pass' }

instance Default ProxyConf where
  def = ProxyConf {
    proxyHost = "localhost",
    proxyPort = 8080
    }

instance Arbitrary ProxyConf where
  arbitrary = do
    host <- T.pack <$> arbitrary
    port <- arbitrary
    return $ ProxyConf { proxyHost = host, proxyPort = port }

spec :: Spec
spec = do
  describe "AoConf" $ do
    let jsonOfDef = "{\"user\":\"aoj_user\",\"pass\":\"aoj_pass\"}"
    it "should be encoded to JSON" $ do
      AE.encode (def :: AojConf) `shouldBe` jsonOfDef

    it "should be decoded from JSON" $ do
      AE.decode jsonOfDef `shouldBe` Just (def :: AojConf)

    prop "decode reverses encode" $ \(j :: AojConf) ->
      AE.decode (AE.encode j) `shouldBe` Just j

  describe "ProxyConf" $ do
    let jsonOfDef = "{\"host\":\"localhost\",\"port\":8080}"
    it "should be encoded to JSON" $ do
      AE.encode (def :: ProxyConf) `shouldBe` jsonOfDef

    it "should be decoded from JSON" $ do
      AE.decode jsonOfDef `shouldBe` Just (def :: ProxyConf)

    prop "decode reverses encode" $ \(j :: ProxyConf) ->
      AE.decode (AE.encode j) `shouldBe` Just j
