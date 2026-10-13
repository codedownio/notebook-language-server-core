
import Test.Sandwich

import qualified Test.CodeLens
import qualified Test.WorkspaceEdit


spec :: TopSpec
spec = do
  Test.CodeLens.spec
  Test.WorkspaceEdit.spec

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
