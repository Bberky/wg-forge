{-# LANGUAGE OverloadedStrings #-}

module WgForge.QR (
  encodeToQr,
  renderQrToAnsii,
) where

import Codec.QRCode (
  ErrorLevel (M),
  QRImage,
  TextEncoding (Utf8WithECI),
  defaultQRCodeOptions,
  encodeText,
  toMatrix,
 )
import Data.Text (Text)
import qualified Data.Text as T

data ColorMode = FG | BG
data Color = White | Black

-- | Encode UTF-8 text into a QR code image using medium (15%) error
-- correction. Returns 'Nothing' if the text is too long to fit any QR version.
encodeToQr :: Text -> Maybe QRImage
encodeToQr = encodeText (defaultQRCodeOptions M) Utf8WithECI

-- | Render a QR code for terminal display.
renderQrToAnsii :: QRImage -> Text
renderQrToAnsii = T.unlines . map renderRowPair . pairRows . addQuietZone . toMatrix White Black

addQuietZone :: [[Color]] -> [[Color]]
addQuietZone m = padRows ++ map (\row -> replicate 4 White ++ row ++ replicate 4 White) m ++ padRows
 where
  width = case m of
    (r : _) -> length r
    [] -> 0
  padRows = replicate 4 (replicate (width + 8) White)

pairRows :: [[Color]] -> [([Color], [Color])]
pairRows (t : b : rest) = (t, b) : pairRows rest
pairRows [t] = [(t, replicate (length t) White)]
pairRows [] = []

renderRowPair :: ([Color], [Color]) -> Text
renderRowPair (top, bottom) = T.concat $ zipWith cell top bottom

cell :: Color -> Color -> Text
cell White White = withColor BG (ansiColor White) " "
cell White Black = withColor BG (ansiColor White) $ withColor FG (ansiColor Black) "▄"
cell Black White = withColor BG (ansiColor White) $ withColor FG (ansiColor Black) "▀"
cell Black Black = withColor BG (ansiColor Black) " "

reset :: Text
reset = "\ESC[0m"

withColor :: ColorMode -> Text -> Text -> Text
withColor FG color content = "\ESC[38;5;" <> color <> "m" <> content <> reset
withColor BG color content = "\ESC[48;5;" <> color <> "m" <> content <> reset

ansiColor :: Color -> Text
ansiColor White = "15"
ansiColor Black = "0"
