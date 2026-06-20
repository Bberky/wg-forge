import Test.Hspec

import qualified Spec.AllocatorSpec as AllocatorSpec
import qualified Spec.CLISpec as CLISpec
import qualified Spec.DiffSpec as DiffSpec
import qualified Spec.KeySpec as KeySpec
import qualified Spec.KeystoreSpec as KeystoreSpec
import qualified Spec.OutputSpec as OutputSpec
import qualified Spec.ParserSpec as ParserSpec
import qualified Spec.ReportSpec as ReportSpec
import qualified Spec.ValidatorSpec as ValidatorSpec

main :: IO ()
main = hspec $ do
  ParserSpec.spec
  ValidatorSpec.spec
  AllocatorSpec.spec
  KeySpec.spec
  KeystoreSpec.spec
  OutputSpec.spec
  CLISpec.spec
  DiffSpec.spec
  ReportSpec.spec
