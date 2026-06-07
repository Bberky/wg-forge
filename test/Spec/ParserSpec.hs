{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.ParserSpec (spec) where

import Data.Aeson (Result (..), fromJSON, object, (.=))
import Data.IP (AddrRange, IPv4, makeAddrRange, toIPv4)
import Data.Text (Text)
import Test.Hspec

import WgForge.Spec (NetworkSpec (..))
import WgForge.Spec.Parser (parseCidr)

ipAddr1 :: IPv4
ipAddr1 = toIPv4 [10, 0, 0, 0]

ipAddr2 :: IPv4
ipAddr2 = toIPv4 [192, 168, 1, 0]

ipRange1 :: AddrRange IPv4
ipRange1 = makeAddrRange ipAddr1 24

ipRangeStr1 :: String
ipRangeStr1 = "10.0.0.0/24"

ipRange2 :: AddrRange IPv4
ipRange2 = makeAddrRange ipAddr2 16

ipRangeStr2 :: String
ipRangeStr2 = "192.168.1.0/16"

spec :: Spec
spec =
  describe "ParserSpec expecations" $ do
    it "should parse a valid CIDR notation" $ do
      let input = ipRangeStr1
      let expected = Right ipRange1
      parseCidr input `shouldBe` expected
    it "should parse another valid CIDR notation" $ do
      let input = ipRangeStr2
      let expected = Right ipRange2
      parseCidr input `shouldBe` expected
    it "should fail to parse an invalid CIDR notation" $ do
      let input = "invalid_cidr"
      let expected = Left "Invalid CIDR notation: invalid_cidr"
      parseCidr input `shouldBe` expected
    it "should parse valid NetworkSpec JSON" $ do
      let val = object ["name" .= ("Test Network" :: Text), "cidr" .= ipRangeStr1]
      let expected = NetworkSpec (Just "Test Network") ipRange1
      fromJSON val `shouldBe` Success expected
    it "should parse NetworkSpec JSON with missing optional name" $ do
      let val = object ["cidr" .= ipRangeStr1]
      let expected = NetworkSpec Nothing ipRange1
      fromJSON val `shouldBe` Success expected
    it "should fail to parse NetworkSpec JSON with invalid CIDR" $ do
      let val = object ["name" .= ("Test Network" :: Text), "cidr" .= ("invalid_cidr" :: String)]
      (fromJSON val :: Result NetworkSpec) `shouldSatisfy` \case
        Error err -> err == "Invalid CIDR notation: invalid_cidr"
        _ -> False
