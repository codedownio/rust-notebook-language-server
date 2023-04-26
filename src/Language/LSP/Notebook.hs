{-# LANGUAGE TypeOperators #-}

module Language.LSP.Notebook (
  RustNotebookTransformer
  , transformerParams
  ) where

import Language.LSP.Notebook.HeadTailTransformer
import Language.LSP.Transformer


type RustNotebookTransformer =
  HeadTailTransformer -- Wrap the whole doc in a function

transformerParams :: Params RustNotebookTransformer
transformerParams =
  (["fn main() {"], ["}"])
