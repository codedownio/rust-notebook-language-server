{-# LANGUAGE TypeOperators #-}

module Language.LSP.Notebook (
  RustNotebookTransformer
  , transformerParams
  , idTransformerParams
  ) where

import Language.LSP.Notebook.HeadTailTransformer
import Language.LSP.Notebook.StripDirective
import Language.LSP.Transformer


type RustNotebookTransformer =
  StripDirective
  :> HeadTailTransformer -- Wrap the whole doc in a function

transformerParams :: Params RustNotebookTransformer
transformerParams =
  SDParams True
  :> (["fn main() {"], ["}"])

idTransformerParams :: Params RustNotebookTransformer
idTransformerParams =
  SDParams False
  :> ([], [])
