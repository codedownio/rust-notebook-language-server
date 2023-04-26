{-# LANGUAGE OverloadedLists #-}

module Test.Transformer.HeadTail where

-- import Language.LSP.Transformer
-- import Language.LSP.Types hiding (line)
import Test.Sandwich


spec :: TopSpec
spec = describe "HeadTailTransformer" $ do
  it "works" $ do
    pending
    -- let (ls, ed@(StripDirective affectedLines)) = project SDParams (listToDoc ["foo = 42", ":t foo"])
    -- ls `shouldBe` (listToDoc ["foo = 42", ""])
    -- affectedLines `shouldBe` [1]

    -- transformPosition SDParams ed (Position 1 3) `shouldBe` (Just (Position 1 0))
    -- untransformPosition SDParams ed (Position 1 0) `shouldBe` (Just (Position 1 0))


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
