import Test.Hspec

import qualified Spec.ParserSpec as ParserSpec

main :: IO ()
main = hspec $ do
  ParserSpec.spec
