{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WgForge.Spec.Parser (parseCidr) where

import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.IP (AddrRange, IPv4)
import Text.Read (readMaybe)

import WgForge.Spec

instance FromJSON NetworkSpec where
  parseJSON = withObject "NetworkSpec" $ \v ->
    NetworkSpec
      <$> v .:? "name"
      <*> (v .: "cidr" >>= either fail pure . parseCidr)

parseCidr :: String -> Either String (AddrRange IPv4)
parseCidr s =
  case readMaybe s of
    Just ip -> Right ip
    Nothing -> Left $ "Invalid CIDR notation: " ++ s
