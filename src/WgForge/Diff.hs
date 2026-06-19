{-# LANGUAGE OverloadedStrings #-}

module WgForge.Diff (
  PeerDiff (..),
  diffConfigs,
  normalize,
  isDirty,
  renderReport,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import WgForge.Spec (PeerName (..))

-- | The result of comparing the goal peer's config against the on-disk config.
data PeerDiff = Added | Removed | Modified | Unchanged
  deriving (Eq, Show, Enum, Bounded)

-- | Classify every peer by comparing the desired configuration against the on-disk configuration.
diffConfigs :: Map PeerName Text -> Map PeerName Text -> Map PeerName PeerDiff
diffConfigs desired disk =
  Map.fromList
    [ (p, classify (Map.lookup p nd) (Map.lookup p nk))
    | p <- Map.keys (Map.union nd nk)
    ]
 where
  nd = Map.map normalize desired
  nk = Map.map normalize disk
  classify (Just _) Nothing = Added
  classify Nothing (Just _) = Removed
  classify (Just a) (Just b) = if a == b then Unchanged else Modified
  classify Nothing Nothing = Unchanged -- unreachable: keys come from the union

-- | Drop all lines ignored during comparison, e.g. private/public keys.
normalize :: Text -> Text
normalize = T.unlines . filter keep . T.lines
 where
  keep line = not ("PrivateKey" `T.isPrefixOf` line || "PublicKey" `T.isPrefixOf` line)

-- | Whether any peer is not @Unchanged@.
isDirty :: Map PeerName PeerDiff -> Bool
isDirty = any (/= Unchanged)

-- | A human-readable report: one symbol-prefixed line per peer (sorted by name)
--   followed by a one-line summary of the counts.
renderReport :: Map PeerName PeerDiff -> Text
renderReport report =
  T.unlines (map line (Map.toAscList report) ++ ["", summary])
 where
  line (PeerName name, d) = T.unwords [symbol, name, label]
   where
    (symbol, label) = symbolLabel d
  summary =
    T.intercalate
      ", "
      [count d <> " " <> label | d <- [Added .. Unchanged], let (_, label) = symbolLabel d]
  count d = T.pack (show (length (filter (== d) (Map.elems report))))

symbolLabel :: PeerDiff -> (Text, Text)
symbolLabel Added = ("+", "added")
symbolLabel Removed = ("-", "removed")
symbolLabel Modified = ("~", "modified")
symbolLabel Unchanged = (" ", "unchanged")
