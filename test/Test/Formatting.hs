{-# LANGUAGE OverloadedLists #-}

module Test.Formatting where

import qualified Data.Text as T
import Data.Text.Rope (Rope)
import qualified Data.Text.Rope as Rope
import qualified Data.UUID as UUID
import Language.LSP.Notebook
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Test.Sandwich
import Transform.ServerRsp.Formatting
import Transform.ServerRsp.Hover (mkDocRegex)
import Transform.Util


spec :: TopSpec
spec = describe "Formatting" $ do
  describe "untransformFormattingEdits" $ do
    it "passes edits through for a plain .rs file" $ do
      let ds = documentState "/tmp/test.rs" "fn main(){}"
      let edits = [TextEdit (Range (Position 0 9) (Position 0 9)) " "]
      untransformFormattingEdits ds edits `shouldBe` edits

    it "unwraps fn main and undoes the indentation it caused" $ do
      let cell = "let x   =   1+2;\nprintln!(\"{}\",x);"
      let ds = documentState "/tmp/main.ipynb" cell
      let formatted = "fn main() {\n    let x = 1 + 2;\n    println!(\"{}\", x);\n}\n"

      untransformFormattingEdits ds [replaceAll ds formatted]
        `shouldBe` [TextEdit (wholeOf cell) "let x = 1 + 2;\nprintln!(\"{}\", x);"]

    it "keeps indentation that was in the cell to begin with" $ do
      let cell = "if x > 0 {\nprintln!(\"hi\");\n}"
      let ds = documentState "/tmp/main.ipynb" cell
      let formatted = "fn main() {\n    if x > 0 {\n        println!(\"hi\");\n    }\n}\n"

      untransformFormattingEdits ds [replaceAll ds formatted]
        `shouldBe` [TextEdit (wholeOf cell) "if x > 0 {\n    println!(\"hi\");\n}"]

    it "applies several edits before unwrapping" $ do
      let cell = "let x=1;\nlet y=2;"
      let ds = documentState "/tmp/main.ipynb" cell
      -- Out of document order on purpose: the edits have to be applied last-first.
      let edits = [
            TextEdit (Range (Position 1 5) (Position 1 6)) " = "
            , TextEdit (Range (Position 1 0) (Position 1 0)) "    "
            , TextEdit (Range (Position 2 5) (Position 2 6)) " = "
            , TextEdit (Range (Position 2 0) (Position 2 0)) "    "
            ]

      untransformFormattingEdits ds edits
        `shouldBe` [TextEdit (wholeOf cell) "let x = 1;\nlet y = 2;"]

    it "leaves a cell containing directives alone" $ do
      let cell = ":dep rand = \"0.8\"\nlet x=1;"
      let ds = documentState "/tmp/main.ipynb" cell
      let formatted = "fn main() {\n    let x = 1;\n}\n"

      untransformFormattingEdits ds [replaceAll ds formatted] `shouldBe` []

    it "returns nothing when the server returned nothing" $ do
      let ds = documentState "/tmp/main.ipynb" "let x=1;"
      untransformFormattingEdits ds [] `shouldBe` []

  describe "dedent" $ do
    it "removes the shared prefix" $
      dedent ["    a", "        b", "    c"] `shouldBe` ["a", "    b", "c"]

    it "ignores blank lines when working out the prefix" $
      dedent ["    a", "", "    b"] `shouldBe` ["a", "", "b"]

    it "does nothing when a line starts at column zero" $
      dedent ["    a", "b"] `shouldBe` ["    a", "b"]

    it "does nothing to an empty document" $
      dedent [] `shouldBe` []


-- | A 'DocumentState' as 'Transform.ClientNot' would have built it for this document.
documentState :: FilePath -> T.Text -> DocumentState
documentState name text = DocumentState {
  transformer = tx
  , curLines = ls
  , curLines' = ls'
  , origUri = uri
  , newUri = uri
  , newPath = name
  , referenceRegex = mkDocRegex (T.pack name)
  , documentUuid = UUID.nil
  , debouncedDidChange = return ()
  }
  where
    uri = filePathToUri name
    ls = Rope.fromText text
    (ls', tx) = project (if isNotebook uri then transformerParams else idTransformerParams) ls

-- | A single edit replacing the whole shadow document, which is what a formatter that doesn't
-- bother to diff would send back.
replaceAll :: DocumentState -> T.Text -> TextEdit
replaceAll (DocumentState {curLines'}) = TextEdit (wholeRange curLines')

wholeOf :: T.Text -> Range
wholeOf = wholeRange . Rope.fromText

wholeRange :: Rope -> Range
wholeRange doc = Range (Position 0 0) (Position (fromIntegral l) (fromIntegral c))
  where Rope.Position l c = Rope.lengthAsPosition doc

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
