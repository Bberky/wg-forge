{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WgForge.Spec.Parser (parseCidr) where

import Data.Aeson (FromJSON (..), withObject, withText, (.:), (.:?))
import Data.IP (AddrRange, IPv4)
import Data.Text (unpack)
import Text.Read

import WgForge.Spec

instance FromJSON NetworkSpec where
  parseJSON = withObject "NetworkSpec" $ \v ->
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
