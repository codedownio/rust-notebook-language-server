{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Expand-selection, run through the real 'RustNotebookTransformer'. A selection range is a
-- chain of ranges, each naming the wider one that contains it, and the widest links always reach
-- the @fn main() {@ that the shadow document wraps the cells in. Requiring the whole chain to map
-- back meant every chain was thrown away and the answer was always empty.
module Test.SelectionRange (tests, main) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text (Text)
import Language.LSP.NotebookLanguageServer.Transform.ClientReq
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Test.Sandwich
import TestLib.Transformers
import Transform.Config


tests :: TopSpec
tests = describe "SelectionRange" $ do
  describe "the request" $ do
    -- A selectionRange request carries a list of positions rather than one, and each moves on
    -- its own.
    it "moves every position into the shadow document" $ do
      runRequest (askedAbout notebookUri [Position 7 4, Position 8 10])
        >>= (`shouldBe` askedAbout notebookShadowUri [Position 8 4, Position 9 10])

    it "leaves a document we aren't shadowing alone" $ do
      let elsewhere = askedAbout (Uri "file:///home/other.rs") [Position 7 4]
      runRequest elsewhere >>= (`shouldBe` elsewhere)

    -- A plain .rs file takes idTransformerParams, so there's no wrapper and nothing to shift.
    it "shifts a plain .rs file's positions by nothing at all" $ do
      runRequest (askedAbout plainUri [Position 6 8])
        >>= (`shouldBe` askedAbout plainShadowUri [Position 6 8])

  describe "the response" $ do
    -- The regression: expand-selection did nothing at all, because every chain reaches the
    -- wrapper eventually and so every chain was dropped.
    it "answers at all, rather than coming back empty" $ do
      answered <- runResponse notebookUri (InL [piChain])
      null (chainsOf answered) `shouldBe` False

    -- The innermost range on the cell's line, and the chain cut before the two links that start
    -- on the `fn main() {` we added.
    it "cuts the chain where it leaves the cell" $ do
      answered <- runResponse notebookUri (InL [piChain])
      chainsOf answered
        `shouldBe` [[Range (Position 7 4) (Position 7 6), Range (Position 7 0) (Position 7 14)]]

    -- Shadow line 0 is in no cell, so there's no innermost range left to answer with.
    it "drops a chain whose innermost range isn't in a cell" $ do
      runResponse notebookUri (InL [chain [(0, 10, 11), (0, 0, 1)]]) >>= (`shouldBe` InL [])

    it "leaves a null answer alone" $
      runResponse notebookUri (InR Null) >>= (`shouldBe` InR Null)

    it "shifts a plain .rs file's chain by nothing at all" $ do
      let answered = chain [(6, 8, 10), (6, 4, 18)]
      runResponse plainUri (InL [answered]) >>= (`shouldBe` InL [answered])

runRequest :: MonadIO m => SelectionRangeParams -> m SelectionRangeParams
runRequest params = liftIO $ do
  state <- transformerStateWith [notebookDocumentState cellLines, plainDocumentState plainFileLines]
  runNoLoggingT $ runReaderT
    (transformClientReq' (rustNotebookConfig shadowDir 0) SMethod_TextDocumentSelectionRange params) state

runResponse :: MonadIO m
  => Uri
  -> MessageResult Method_TextDocumentSelectionRange
  -> m (MessageResult Method_TextDocumentSelectionRange)
runResponse u result = liftIO $ do
  state <- transformerStateWith [notebookDocumentState cellLines, plainDocumentState plainFileLines]
  runNoLoggingT $ runReaderT
    (transformServerRsp' (rustNotebookConfig shadowDir 0) SMethod_TextDocumentSelectionRange
       (askedAbout u [Position 7 4]) result) state

askedAbout :: Uri -> [Position] -> SelectionRangeParams
askedAbout u = SelectionRangeParams Nothing Nothing (TextDocumentIdentifier u)

-- | Roughly what rust-analyzer answers for the cursor inside @pi@ on shadow line 8: the
-- identifier, the statement it belongs to, then the block and the function, both of which begin
-- on the @fn main() {@ line the transformer added.
piChain :: SelectionRange
piChain = chain [(8, 4, 6), (8, 0, 14), (0, 10, 11), (0, 0, 1)]

-- | Innermost first, each one's parent being the next, given as (line, start column, end column).
chain :: [(UInt, UInt, UInt)] -> SelectionRange
chain [] = error "a selection range is at least one range"
chain ((l, s, e):rest) = SelectionRange (Range (Position l s) (Position l e)) $ case rest of
  [] -> Nothing
  _ -> Just (chain rest)

-- | Each answered chain flattened by walking @parent@, innermost first.
chainsOf :: MessageResult Method_TextDocumentSelectionRange -> [[Range]]
chainsOf (InL chains) = fmap rangesOf chains
  where rangesOf (SelectionRange r parent) = r : maybe [] rangesOf parent
chainsOf (InR _) = []

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
