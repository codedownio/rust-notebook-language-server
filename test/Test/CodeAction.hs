{-# LANGUAGE OverloadedLists #-}

module Test.CodeAction where

import Control.Lens hiding (List)
import Control.Monad.IO.Class
import GHC.Stack
import qualified Data.Map as M
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.Rope as Rope
import qualified Data.UUID as UUID
import Language.LSP.Notebook
import Language.LSP.Protocol.Lens as Lens hiding (edit, range, to)
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Test.Sandwich
import Transform.Common
import Transform.ServerRsp.Hover (mkDocRegex)
import Transform.Util


cell :: T.Text
cell = "let nums = vec![1, 2, 3];\nprintln!(\"{:?}\", nums);"

spec :: TopSpec
spec = describe "CodeAction" $ do
  describe "untransformWorkspaceEdit" $ do
    it "rewrites documentChanges back to the notebook's uri and lines" $ do
      let ds = notebookDocumentState
      shadowPoint <- shadowPointFor ds (Position 0 8)

      let given = workspaceEditWithDocumentChanges (shadowUri ds) (Just 16) shadowPoint ": Vec<i32>"
      untransformWorkspaceEdit ds given
        `shouldBe` Just (workspaceEditWithDocumentChanges (notebookUri ds) Nothing (pointAt 0 8) ": Vec<i32>")

    it "rewrites a changes map the same way" $ do
      let ds = notebookDocumentState
      shadowPoint <- shadowPointFor ds (Position 0 8)

      let given = emptyEdit & set changes (Just (M.singleton (shadowUri ds) [TextEdit shadowPoint ": Vec<i32>"]))
      untransformWorkspaceEdit ds given
        `shouldBe` Just (emptyEdit & set changes (Just (M.singleton (notebookUri ds) [TextEdit (pointAt 0 8) ": Vec<i32>"])))

    it "maps the second line of the cell, not just the first" $ do
      let ds = notebookDocumentState
      shadowPoint <- shadowPointFor ds (Position 1 0)

      let given = workspaceEditWithDocumentChanges (shadowUri ds) Nothing shadowPoint "// "
      untransformWorkspaceEdit ds given
        `shouldBe` Just (workspaceEditWithDocumentChanges (notebookUri ds) Nothing (pointAt 1 0) "// ")

    it "drops an edit on a line the projection introduced" $ do
      let ds = notebookDocumentState
      let given = workspaceEditWithDocumentChanges (shadowUri ds) Nothing (pointAt 0 0) "#[allow(unused)]\n"
      untransformWorkspaceEdit ds given `shouldBe` Nothing

    it "drops an edit naming a document that isn't this notebook's shadow file" $ do
      let ds = notebookDocumentState
      let given = workspaceEditWithDocumentChanges (filePathToUri "/tmp/shadow/src/other.rs") Nothing (pointAt 0 0) "x"
      untransformWorkspaceEdit ds given `shouldBe` Nothing

    it "drops an action that creates a file" $ do
      let ds = notebookDocumentState
      let given = emptyEdit & set documentChanges (Just [
            InR (InL (CreateFile Nothing AString (filePathToUri "/tmp/shadow/src/new.rs") Nothing))
            ])
      untransformWorkspaceEdit ds given `shouldBe` Nothing

    it "passes a plain .rs file through untouched" $ do
      let ds = plainDocumentState
      let given = workspaceEditWithDocumentChanges (notebookUri ds) Nothing (pointAt 0 8) ": Vec<i32>"
      untransformWorkspaceEdit ds given
        `shouldBe` Just (given & set (documentChanges . _Just . traverse . _L . textDocument . version) (InR Null))

  describe "untransformCodeAction" $ do
    it "leaves an action with no edit alone, for codeAction/resolve to fill in later" $ do
      let action = CodeAction "Insert explicit type" Nothing Nothing Nothing Nothing Nothing Nothing (Just "token")
      untransformCodeAction notebookDocumentState action `shouldBe` Just action

    it "drops an action whose edit doesn't map back" $ do
      let ds = notebookDocumentState
      let given = workspaceEditWithDocumentChanges (shadowUri ds) Nothing (pointAt 0 0) "x"
      let action = CodeAction "Bad" Nothing Nothing Nothing Nothing (Just given) Nothing Nothing
      untransformCodeAction ds action `shouldBe` Nothing

  describe "workspaceEditUris" $ do
    it "finds the uri in documentChanges" $ do
      let u = filePathToUri "/tmp/shadow/src/doc.rs"
      workspaceEditUris (workspaceEditWithDocumentChanges u Nothing (pointAt 0 0) "x") `shouldBe` [u]

    it "finds the uri in a changes map" $ do
      let u = filePathToUri "/tmp/shadow/src/doc.rs"
      workspaceEditUris (emptyEdit & set changes (Just (M.singleton u [TextEdit (pointAt 0 0) "x"]))) `shouldBe` [u]


shadowPointFor :: (HasCallStack, MonadIO m) => DocumentState -> Position -> m Range
shadowPointFor (DocumentState {transformer=tx}) p =
  case transformPosition (getParams tx) tx p of
    Just p' -> pure (Range p' p')
    Nothing -> expectationFailure [i|Fixture position #{p} isn't inside the cell|]

pointAt :: UInt -> UInt -> Range
pointAt l c = Range (Position l c) (Position l c)

emptyEdit :: WorkspaceEdit
emptyEdit = WorkspaceEdit Nothing Nothing Nothing

workspaceEditWithDocumentChanges :: Uri -> Maybe Int32 -> Range -> T.Text -> WorkspaceEdit
workspaceEditWithDocumentChanges u v r newText = emptyEdit & set documentChanges (Just [
  InL (TextDocumentEdit
         (OptionalVersionedTextDocumentIdentifier u (maybe (InR Null) InL v))
         [InL (TextEdit r newText)])
  ])

notebookUri :: DocumentState -> Uri
notebookUri (DocumentState {origUri}) = origUri

shadowUri :: DocumentState -> Uri
shadowUri (DocumentState {newUri}) = newUri

notebookDocumentState :: DocumentState
notebookDocumentState = documentState "/home/main.ipynb" "/tmp/shadow/src/doc.rs" cell

plainDocumentState :: DocumentState
plainDocumentState = documentState "/tmp/test.rs" "/tmp/test.rs" cell

documentState :: FilePath -> FilePath -> T.Text -> DocumentState
documentState origName newName text = DocumentState {
  transformer = tx
  , curLines = ls
  , curLines' = ls'
  , origUri = origUri'
  , newUri = filePathToUri newName
  , newPath = newName
  , referenceRegex = mkDocRegex (T.pack newName)
  , documentUuid = UUID.nil
  , debouncedDidChange = return ()
  }
  where
    origUri' = filePathToUri origName
    ls = Rope.fromText text
    (ls', tx) = project (if isNotebook origUri' then transformerParams else idTransformerParams) ls

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
