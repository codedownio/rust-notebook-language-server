{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | The code action response path, run through the real 'RustNotebookTransformer' rather than a
-- stand-in. rust-analyzer answers in the shadow document's coordinates, and the shadow document
-- is the notebook's lines wrapped in @fn main() {@, so everything it says is a line too low.
module Test.CodeAction (tests, main) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Text (Text)
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Test.Sandwich
import TestLib.Transformers
import Transform.Config


tests :: TopSpec
tests = describe "CodeAction" $ do
  describe "textDocument/codeAction" $ do
    -- "Insert explicit type `f64`" on `let pi = 3.14;` came back with the edit on the line
    -- below, so the text landed on the following line of the cell.
    it "puts an inline edit back on the notebook's line" $ do
      let answered = [InR (action (Just (documentChangesEdit notebookShadowUri [InL (TextEdit (lineRange 8) ": f64")])))]
      runCodeAction (askedAbout notebookUri 7) (InL answered)
        >>= (`shouldBe` InL [InR (action (Just (documentChangesEdit notebookUri [InL (TextEdit (lineRange 7) ": f64")])))])

    it "does the same for an edit given as a changes map" $ do
      let answered = [InR (action (Just (changesEdit notebookShadowUri [TextEdit (lineRange 8) ": f64"])))]
      runCodeAction (askedAbout notebookUri 7) (InL answered)
        >>= (`shouldBe` InL [InR (action (Just (changesEdit notebookUri [TextEdit (lineRange 7) ": f64"])))])

    -- Shadow lines 8 and 9 are the last two lines of the cell. Several edits at once, on more
    -- than one line, so a transformer that mapped everything to one line wouldn't pass.
    it "maps every edit in the action, each to its own line" $ do
      let given = [InL (TextEdit (lineRange 8) ": f64"), InL (TextEdit (lineRange 9) "// ")]
      let expected = [InL (TextEdit (lineRange 7) ": f64"), InL (TextEdit (lineRange 8) "// ")]
      runCodeAction (askedAbout notebookUri 7) (InL [InR (action (Just (documentChangesEdit notebookShadowUri given)))])
        >>= (`shouldBe` InL [InR (action (Just (documentChangesEdit notebookUri expected)))])

    it "leaves an action with no edit for resolve to fill in" $ do
      let answered = [InR (action Nothing)]
      runCodeAction (askedAbout notebookUri 7) (InL answered) >>= (`shouldBe` InL answered)

    it "passes a plain command through" $ do
      let answered = [InL (Command "Run" "rust-analyzer.run" Nothing)]
      runCodeAction (askedAbout notebookUri 7) (InL answered) >>= (`shouldBe` InL answered)

    it "leaves a null answer alone" $
      runCodeAction (askedAbout notebookUri 7) (InR Null) >>= (`shouldBe` InR Null)

    -- Shadow line 0 is the `fn main() {` we added, which is in no cell.
    it "drops an action whose edit lands on a wrapper line" $ do
      let answered = [InR (action (Just (documentChangesEdit notebookShadowUri [InL (TextEdit (lineRange 0) "// hi")])))]
      runCodeAction (askedAbout notebookUri 7) (InL answered) >>= (`shouldBe` InL [])

    -- Another module in the shadow crate. There's no cell it could be pointed at.
    it "drops an action whose edit names a file other than this notebook's shadow document" $ do
      let other = filePathToUri (shadowDir <> "/src/other.rs")
      let answered = [InR (action (Just (documentChangesEdit other [InL (TextEdit (lineRange 8) ": f64")])))]
      runCodeAction (askedAbout notebookUri 7) (InL answered) >>= (`shouldBe` InL [])

    it "drops an action that creates a file" $ do
      let answered = [InR (action (Just (createFileEdit (filePathToUri (shadowDir <> "/src/new.rs")))))]
      runCodeAction (askedAbout notebookUri 7) (InL answered) >>= (`shouldBe` InL [])

  describe "codeAction/resolve" $ do
    -- No text document comes along with a resolve, so the notebook has to be found from the
    -- shadow uri the edit names.
    it "puts the resolved edit back on the notebook's line" $ do
      let answered = action (Just (documentChangesEdit notebookShadowUri [InL (TextEdit (lineRange 8) ": f64")]))
      runResolve answered
        >>= (`shouldBe` action (Just (documentChangesEdit notebookUri [InL (TextEdit (lineRange 7) ": f64")])))

    -- The uri is looked for in documentChanges first and a changes map second, so an edit that
    -- only has the latter has to find the notebook too.
    it "finds the notebook from a changes map as well" $ do
      let answered = action (Just (changesEdit notebookShadowUri [TextEdit (lineRange 8) ": f64"]))
      runResolve answered
        >>= (`shouldBe` action (Just (changesEdit notebookUri [TextEdit (lineRange 7) ": f64"])))

    it "leaves an action with no edit alone" $
      runResolve (action Nothing) >>= (`shouldBe` action Nothing)

    it "drops an edit that lands on a wrapper line" $ do
      let answered = action (Just (documentChangesEdit notebookShadowUri [InL (TextEdit (lineRange 0) "// hi")]))
      runResolve answered >>= (`shouldBe` action Nothing)

  describe "a plain .rs file" $ do
    -- Not a notebook, so it takes idTransformerParams and there's no wrapper to account for.
    -- Getting a shift wrong here would be invisible otherwise.
    it "shifts an edit by nothing at all" $ do
      let answered = [InR (action (Just (documentChangesEdit plainShadowUri [InL (TextEdit (lineRange 6) ": f64")])))]
      runCodeAction (askedAbout plainUri 6) (InL answered)
        >>= (`shouldBe` InL [InR (action (Just (documentChangesEdit plainUri [InL (TextEdit (lineRange 6) ": f64")])))])

runCodeAction :: MonadIO m
  => MessageParams Method_TextDocumentCodeAction
  -> MessageResult Method_TextDocumentCodeAction
  -> m (MessageResult Method_TextDocumentCodeAction)
runCodeAction = runTransform SMethod_TextDocumentCodeAction

runResolve :: MonadIO m => CodeAction -> m CodeAction
runResolve answered = runTransform SMethod_CodeActionResolve answered answered

runTransform :: MonadIO m => ServerRspMethod meth -> MessageParams meth -> MessageResult meth -> m (MessageResult meth)
runTransform meth params result = liftIO $ do
  state <- transformerStateWith [notebookDocumentState cellLines, plainDocumentState plainFileLines]
  runNoLoggingT $ runReaderT (transformServerRsp' (rustNotebookConfig shadowDir 0) meth params result) state

askedAbout :: Uri -> UInt -> MessageParams Method_TextDocumentCodeAction
askedAbout u line = CodeActionParams Nothing Nothing (TextDocumentIdentifier u) (lineRange line)
                      (CodeActionContext [] Nothing Nothing)

action :: Maybe WorkspaceEdit -> CodeAction
action e = CodeAction "Insert explicit type `f64`" (Just CodeActionKind_QuickFix) Nothing Nothing Nothing
             e Nothing Nothing

lineRange :: UInt -> Range
lineRange l = Range (Position l 0) (Position l 0)

documentChangesEdit :: Uri -> [TextEdit |? AnnotatedTextEdit] -> WorkspaceEdit
documentChangesEdit u edits = WorkspaceEdit Nothing
  (Just [InL (TextDocumentEdit (OptionalVersionedTextDocumentIdentifier u (InR Null)) edits)]) Nothing

changesEdit :: Uri -> [TextEdit] -> WorkspaceEdit
changesEdit u edits = WorkspaceEdit (Just (M.singleton u edits)) Nothing Nothing

createFileEdit :: Uri -> WorkspaceEdit
createFileEdit u = WorkspaceEdit Nothing
  (Just [InR (InL (CreateFile Nothing AString u Nothing))]) Nothing

-- | @let pi = 3.14;@ is notebook line 7, so shadow line 8. The @:dep@ line is there so the
-- composed @StripDirective :> HeadTailTransformer@ is the thing under test, not just the half
-- of it that shifts lines.
cellLines :: [Text]
cellLines = [
  ":dep rand = \"0.8\""
  , ""
  , "fn area(r: f64) -> f64 {"
  , "    3.14159 * r * r"
  , "}"
  , ""
  , "let r = 2.0;"
  , "let pi = 3.14;"
  , "println!(\"{} {}\", area(r), pi);"
  ]

-- | @let pi = 3.14;@ is line 6 here, and stays line 6 in the shadow document.
plainFileLines :: [Text]
plainFileLines = [
  "fn area(r: f64) -> f64 {"
  , "    3.14159 * r * r"
  , "}"
  , ""
  , "fn main() {"
  , "    let r = 2.0;"
  , "    let pi = 3.14;"
  , "    println!(\"{} {}\", area(r), pi);"
  , "}"
  ]

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
