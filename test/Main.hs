
import Test.Sandwich

import qualified Test.Hover


spec :: TopSpec
spec = do
  Test.Hover.spec

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
