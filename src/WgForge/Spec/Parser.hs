{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WgForge.Spec.Parser (
  parseNetwork,
  parseNetworkFile,
  parseCidr,
) where

import Control.Exception (IOException, displayException, try)
import Data.Aeson (
  FromJSON (..),
  Key,
  Object,
  Value,
  withObject,
  withText,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Aeson.KeyMap (keys)
import Data.Aeson.Types (Parser)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IP (AddrRange, IPv4)
import Data.Text (pack, unpack)
import Data.Word (Word16)
import Data.Yaml (ParseException (..), decodeEither', prettyPrintParseException)
import Text.Read (readMaybe)

import WgForge.Error (SpecError (..))
import WgForge.Spec

instance FromJSON NetworkSpec where
  parseJSON = withObjectStrict "NetworkSpec" ["name", "cidr"] $ \v ->
    NetworkSpec
      <$> v .:? "name"
      <*> (v .: "cidr" >>= either fail pure . parseCidr)

-- | Parse a CIDR notation string into an 'AddrRange IPv4'.
-- The input should be in the form "x.x.x.x/y", where x.x.x.x is an IPv4 address and y is the prefix length.
parseCidr :: String -> Either String (AddrRange IPv4)
parseCidr s =
  case readMaybe s of
    Just ip -> Right ip
    Nothing -> Left $ "Invalid CIDR notation: " ++ s

instance FromJSON AllowedIpsMode where
  parseJSON = withText "AllowedIpsMode" $ \t ->
    case t of
      "peers" -> pure Peers
      "subnet" -> pure Subnet
      "internet" -> pure Internet
      _ -> fail $ "Invalid allowedIps: " ++ unpack t

-- | Parse an IPv4 address from a string.
-- The input should be in the form "x.x.x.x", where x are decimal octets.
parseIPv4 :: String -> Either String IPv4
parseIPv4 s = case readMaybe s of
  Just ip -> Right ip
  Nothing -> Left $ "Invalid IPv4 address: " ++ s

-- | Parse a TCP/UDP port number from a string.
-- The input should be a valid 16 bit unsigned integer.
parsePort :: String -> Either String Word16
parsePort s = case readMaybe s :: Maybe Integer of
  Just n | n >= 0, n <= 65535 -> Right (fromInteger n)
  _ -> Left $ "Invalid port number: " ++ s

-- | Parse a host from string by first trying to parse as IPv4, then falls back to hostname.
parseHost :: String -> HostOrIp
parseHost s = either (const $ HostName (pack s)) HostIp (parseIPv4 s)

instance FromJSON Endpoint where
  parseJSON = withText "Endpoint" $ \t ->
    case break (== ':') (unpack t) of
      (hostStr, ':' : portStr) -> do
        p <- either fail pure (parsePort portStr)
        pure $ Endpoint (parseHost hostStr) (Port p)
      _ -> fail $ "Invalid endpoint: " ++ unpack t

instance FromJSON SegmentSpec where
  parseJSON = withObjectStrict
    "SegmentSpec"
    ["topology", "peers", "hubs", "spokes", "relays", "client", "allowedIps"]
    $ \o -> do
      topology <- o .: "topology" :: Parser String
      case topology of
        "full-mesh" -> FullMesh <$> o .: "peers"
        "hub-and-spoke" -> HubSpoke <$> o .: "hubs" <*> o .: "spokes" <*> allowedIpsOrDefault o
        "relay" -> Relay <$> o .: "relays" <*> o .: "client" <*> allowedIpsOrDefault o
        _ -> fail $ "Invalid topology: " ++ topology
   where
    allowedIpsOrDefault :: Object -> Parser AllowedIpsMode
    allowedIpsOrDefault o = o .:? "allowedIps" .!= Peers

instance FromJSON PeerSpec where
  parseJSON = withObjectStrict "PeerSpec" ["endpoint", "listenPort", "persistentKeepalive", "address", "tags"] $ \o ->
    PeerSpec
      <$> o .:? "endpoint"
      <*> o .:? "listenPort"
      <*> o .:? "persistentKeepalive"
      <*> (o .:? "address" >>= traverse (either fail pure . parseIPv4))
      <*> o .:? "tags" .!= []

-- | Parse a YAML document into a 'Network'.
parseNetwork :: ByteString -> Either SpecError Network
parseNetwork = first fromParseException . decodeEither'

-- | Read and parse a YAML spec file into a 'Network'.
parseNetworkFile :: FilePath -> IO (Either SpecError Network)
parseNetworkFile path = do
  contents <- try (BS.readFile path)
  pure $ case contents of
    Left e -> Left $ SpecIoError (displayException (e :: IOException))
    Right bytes -> parseNetwork bytes

fromParseException :: ParseException -> SpecError
fromParseException (AesonException msg) = SpecParseError msg
fromParseException e = YamlSyntaxError (prettyPrintParseException e)

instance FromJSON Network where
  parseJSON = withObjectStrict "Network" ["network", "peers", "segments"] $ \o ->
    Network
      <$> o .: "network"
      <*> o .:? "peers" .!= mempty
      <*> o .:? "segments" .!= mempty

-- | A helper function to parse an object while ensuring that no unknown fields are present.
withObjectStrict :: String -> [Key] -> (Object -> Parser a) -> Value -> Parser a
withObjectStrict typeName allowed f = withObject typeName $ \o ->
  case filter (`notElem` allowed) (keys o) of
    [] -> f o
    extra -> fail $ "Unknown field(s) in " ++ typeName ++ ": " ++ show extra
