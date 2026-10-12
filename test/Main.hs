
import Test.Sandwich

import qualified Test.WorkspaceEdit


spec :: TopSpec
spec = do
  Test.WorkspaceEdit.spec

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
