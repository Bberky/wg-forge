{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.ParserSpec (spec) where

import Data.Aeson (Result (..), Value (String), fromJSON, object, (.=))
import Data.IP (AddrRange, IPv4, makeAddrRange, toIPv4)
import Data.Text (Text)
import Test.Hspec

import WgForge.Spec (
  AllowedIpsMode (..),
  Endpoint (..),
  HostOrIp (..),
  NetworkSpec (..),
  PeerName (..),
  Port (..),
  SegmentSpec (..),
 )
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
    it "should parse valid AllowedIpsMode" $ do
      let mode1 = "peers"
      let mode2 = "subnet"
      let mode3 = "internet"
      (fromJSON mode1 :: Result AllowedIpsMode) `shouldBe` Success Peers
      (fromJSON mode2 :: Result AllowedIpsMode) `shouldBe` Success Subnet
      (fromJSON mode3 :: Result AllowedIpsMode) `shouldBe` Success Internet
    it "should not parse invalid AllowedIpsMode" $ do
      let mode = "something"
      (fromJSON mode :: Result AllowedIpsMode) `shouldSatisfy` \case
        Error _ -> True
        _ -> False
    it "should parse endpoint with IPv4 host" $ do
      let val = String "10.0.0.1:51820"
      let expected = Endpoint (HostIp (toIPv4 [10, 0, 0, 1])) (Port 51820)
      fromJSON val `shouldBe` Success expected
    it "should parse endpoint with hostname" $ do
      let val = String "vpn.example.com:51820"
      let expected = Endpoint (HostName "vpn.example.com") (Port 51820)
      fromJSON val `shouldBe` Success expected
    it "should fail to parse endpoint without a port" $ do
      let val = String "vpn.example.com"
      (fromJSON val :: Result Endpoint) `shouldSatisfy` \case
        Error _ -> True
        _ -> False
    it "should fail to parse endpoint with non-numeric port" $ do
      let val = String "vpn.example.com:notaport"
      (fromJSON val :: Result Endpoint) `shouldSatisfy` \case
        Error _ -> True
        _ -> False
    it "should fail to parse endpoint with out-of-range port" $ do
      let val = String "vpn.example.com:99999"
      (fromJSON val :: Result Endpoint) `shouldSatisfy` \case
        Error _ -> True
        _ -> False
    it "should fail to parse endpoint with negative port" $ do
      let val = String "vpn.example.com:-1"
      (fromJSON val :: Result Endpoint) `shouldSatisfy` \case
        Error _ -> True
        _ -> False
    it "should parse full-mesh segment" $ do
      let val = object ["topology" .= ("full-mesh" :: Text), "peers" .= (["alice", "bob"] :: [Text])]
      let expected = FullMesh [PeerName "alice", PeerName "bob"]
      fromJSON val `shouldBe` Success expected
    it "should parse hub-and-spoke segment with allowedIps" $ do
      let val =
            object
              [ "topology" .= ("hub-and-spoke" :: Text),
                "hubs" .= (["hub1"] :: [Text]),
                "spokes" .= (["spoke1", "spoke2"] :: [Text]),
                "allowedIps" .= ("subnet" :: Text)
              ]
      let expected = HubSpoke [PeerName "hub1"] [PeerName "spoke1", PeerName "spoke2"] Subnet
      fromJSON val `shouldBe` Success expected
    it "should default to Peers when hub-and-spoke segment has no allowedIps" $ do
      let val =
            object
              [ "topology" .= ("hub-and-spoke" :: Text),
                "hubs" .= (["hub1"] :: [Text]),
                "spokes" .= (["spoke1"] :: [Text])
              ]
      let expected = HubSpoke [PeerName "hub1"] [PeerName "spoke1"] Peers
      fromJSON val `shouldBe` Success expected
    it "should parse relay segment with allowedIps" $ do
      let val =
            object
              [ "topology" .= ("relay" :: Text),
                "relays" .= (["relay1"] :: [Text]),
                "client" .= (["leaf1"] :: [Text]),
                "allowedIps" .= ("internet" :: Text)
              ]
      let expected = Relay [PeerName "relay1"] [PeerName "leaf1"] Internet
      fromJSON val `shouldBe` Success expected
    it "should fail to parse segment with invalid topology" $ do
      let val = object ["topology" .= ("ring" :: Text)]
      (fromJSON val :: Result SegmentSpec) `shouldSatisfy` \case
        Error err -> err == "Invalid topology: ring"
        _ -> False
    it "should fail to parse segment with missing topology" $ do
      let val = object ["peers" .= (["alice"] :: [Text])]
      (fromJSON val :: Result SegmentSpec) `shouldSatisfy` \case
        Error _ -> True
        _ -> False
    it "should fail to parse full-mesh segment with missing peers" $ do
      let val = object ["topology" .= ("full-mesh" :: Text)]
      (fromJSON val :: Result SegmentSpec) `shouldSatisfy` \case
        Error _ -> True
        _ -> False
