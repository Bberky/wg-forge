{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.ParserSpec (spec) where

import Data.Aeson (Result (..), Value (String), fromJSON, object, (.=))
import qualified Data.ByteString.Char8 as BS8
import Data.IP (toIPv4)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

import Spec.Fixtures
import WgForge.Error
import WgForge.Spec
import WgForge.Spec.Parser

spec :: Spec
spec = do
  describe "parseCidr" $ do
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

  describe "NetworkSpec" $ do
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

  describe "AllowedIpsMode" $ do
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

  describe "Endpoint" $ do
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

  describe "SegmentSpec" $ do
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

  describe "PeerSpec" $ do
    it "should parse full PeerSpec" $ do
      let val =
            object
              [ "endpoint" .= ("vpn.example.com:51820" :: Text),
                "listenPort" .= (51820 :: Int),
                "persistentKeepalive" .= (25 :: Int),
                "address" .= ("10.0.0.5" :: Text),
                "tags" .= (["server", "eu"] :: [Text])
              ]
      let expected =
            PeerSpec
              (Just (Endpoint (HostName "vpn.example.com") (Port 51820)))
              (Just (Port 51820))
              (Just 25)
              (Just (toIPv4 [10, 0, 0, 5]))
              ["server", "eu"]
      fromJSON val `shouldBe` Success expected
    it "should parse empty PeerSpec with all defaults" $ do
      let val = object []
      let expected = PeerSpec Nothing Nothing Nothing Nothing []
      fromJSON val `shouldBe` Success expected
    it "should fail to parse PeerSpec with invalid address" $ do
      let val = object ["address" .= ("not-an-ip" :: Text)]
      (fromJSON val :: Result PeerSpec) `shouldSatisfy` \case
        Error err -> err == "Invalid IPv4 address: not-an-ip"
        _ -> False

  describe "Network" $ do
    it "should parse full Network document" $ do
      let val =
            object
              [ "network" .= object ["name" .= ("Test Network" :: Text), "cidr" .= ipRangeStr1],
                "peers"
                  .= object
                    [ "alice" .= object ["address" .= ("10.0.0.1" :: Text)],
                      "bob" .= object []
                    ],
                "segments"
                  .= object
                    [ "main" .= object ["topology" .= ("full-mesh" :: Text), "peers" .= (["alice", "bob"] :: [Text])]
                    ]
              ]
      let expected =
            Network
              (NetworkSpec (Just "Test Network") ipRange1)
              ( Map.fromList
                  [ (PeerName "alice", PeerSpec Nothing Nothing Nothing (Just (toIPv4 [10, 0, 0, 1])) []),
                    (PeerName "bob", PeerSpec Nothing Nothing Nothing Nothing [])
                  ]
              )
              (Map.fromList [(SegmentName "main", FullMesh [PeerName "alice", PeerName "bob"])])
      fromJSON val `shouldBe` Success expected
    it "should parse Network document with missing peers and segments" $ do
      let val = object ["network" .= object ["cidr" .= ipRangeStr1]]
      let expected = Network (NetworkSpec Nothing ipRange1) Map.empty Map.empty
      fromJSON val `shouldBe` Success expected
    it "should fail to parse Network document without network section" $ do
      let val = object ["peers" .= object []]
      (fromJSON val :: Result Network) `shouldSatisfy` \case
        Error _ -> True
        _ -> False

  describe "parseNetwork" $ do
    it "should parse a full YAML document" $ do
      let yaml =
            BS8.pack $
              unlines
                [ "network:",
                  "  name: Test Network",
                  "  cidr: 10.0.0.0/24",
                  "peers:",
                  "  alice:",
                  "    address: 10.0.0.1",
                  "  bob: {}",
                  "segments:",
                  "  main:",
                  "    topology: full-mesh",
                  "    peers: [alice, bob]"
                ]
      let expected =
            Network
              (NetworkSpec (Just "Test Network") ipRange1)
              ( Map.fromList
                  [ (PeerName "alice", PeerSpec Nothing Nothing Nothing (Just (toIPv4 [10, 0, 0, 1])) []),
                    (PeerName "bob", PeerSpec Nothing Nothing Nothing Nothing [])
                  ]
              )
              (Map.fromList [(SegmentName "main", FullMesh [PeerName "alice", PeerName "bob"])])
      parseNetwork yaml `shouldBe` Right expected
    it "should report malformed YAML as a syntax error" $ do
      let yaml = BS8.pack "network: [unclosed"
      parseNetwork yaml `shouldSatisfy` \case
        Left (YamlSyntaxError _) -> True
        _ -> False
    it "should report schema violations as parse errors" $ do
      let yaml = BS8.pack $ unlines ["network:", "  cidr: invalid_cidr"]
      parseNetwork yaml `shouldSatisfy` \case
        Left (SpecParseError msg) -> "Invalid CIDR notation" `isInfixOf` msg
        _ -> False

  describe "unknown key rejection" $ do
    it "should reject an unknown key in network section" $ do
      let yaml =
            BS8.pack $
              unlines
                [ "network:",
                  "  cidr: 10.0.0.0/24",
                  "  typo: oops"
                ]
      parseNetwork yaml `shouldSatisfy` \case
        Left (SpecParseError msg) -> "Unknown field" `isInfixOf` msg
        _ -> False
    it "should reject an unknown key in peer section" $ do
      let yaml =
            BS8.pack $
              unlines
                [ "network:",
                  "  cidr: 10.0.0.0/24",
                  "peers:",
                  "  alice:",
                  "    unknownField: foo"
                ]
      parseNetwork yaml `shouldSatisfy` \case
        Left (SpecParseError msg) -> "Unknown field" `isInfixOf` msg
        _ -> False
    it "should reject an unknown key in segment section" $ do
      let yaml =
            BS8.pack $
              unlines
                [ "network:",
                  "  cidr: 10.0.0.0/24",
                  "peers:",
                  "  alice: {}",
                  "  bob: {}",
                  "segments:",
                  "  main:",
                  "    topology: full-mesh",
                  "    peers: [alice, bob]",
                  "    extra: forbidden"
                ]
      parseNetwork yaml `shouldSatisfy` \case
        Left (SpecParseError msg) -> "Unknown field" `isInfixOf` msg
        _ -> False
    it "should reject an unknown key at the top level" $ do
      let yaml =
            BS8.pack $
              unlines
                [ "network:",
                  "  cidr: 10.0.0.0/24",
                  "bogus: value"
                ]
      parseNetwork yaml `shouldSatisfy` \case
        Left (SpecParseError msg) -> "Unknown field" `isInfixOf` msg
        _ -> False

  describe "parseNetworkFile" $ do
    it "should parse a YAML spec file" $ do
      let expected =
            Network
              (NetworkSpec (Just "Fixture Network") ipRange1)
              ( Map.fromList
                  [ ( PeerName "alice",
                      PeerSpec
                        (Just (Endpoint (HostName "vpn.example.com") (Port 51820)))
                        (Just (Port 51820))
                        Nothing
                        (Just (toIPv4 [10, 0, 0, 1]))
                        []
                    ),
                    (PeerName "bob", PeerSpec Nothing Nothing (Just 25) Nothing ["laptop"])
                  ]
              )
              (Map.fromList [(SegmentName "main", FullMesh [PeerName "alice", PeerName "bob"])])
      result <- parseNetworkFile "test/fixtures/network.yaml"
      result `shouldBe` Right expected
    it "should report a missing spec file as an IO error" $ do
      result <- parseNetworkFile "test/fixtures/does-not-exist.yaml"
      result `shouldSatisfy` \case
        Left (SpecIoError _) -> True
        _ -> False
