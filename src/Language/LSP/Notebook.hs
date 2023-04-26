{-# LANGUAGE TypeOperators #-}

module Language.LSP.Notebook (
  module Language.LSP.Notebook.ExpressionToDeclaration

  , HaskellNotebookTransformer
  , expressionToDeclarationParams
  , transformerParams
  ) where

import Language.LSP.Notebook.ExpressionToDeclaration
import Language.LSP.Transformer


type HaskellNotebookTransformer =
  ExpressionToDeclaration -- Convert naked expressions to declarations

expressionToDeclarationParams :: Params ExpressionToDeclaration
expressionToDeclarationParams = EDParams 10

transformerParams :: Params HaskellNotebookTransformer
transformerParams =
  expressionToDeclarationParams
