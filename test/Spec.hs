import Test.Hspec

import qualified Spec.ParserSpec as ParserSpec
import qualified Spec.ValidatorSpec as ValidatorSpec

main :: IO ()
main = hspec $ do
  ParserSpec.spec
  ValidatorSpec.spec
