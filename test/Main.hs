
import Test.Sandwich

import qualified Test.Formatting
import qualified Test.Hover

import qualified Test.Transformer.HeadTail
import qualified Test.Transformer.StripDirective


spec :: TopSpec
spec = do
  Test.Formatting.spec
  Test.Hover.spec

  Test.Transformer.HeadTail.spec
  Test.Transformer.StripDirective.spec

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
