{-# LANGUAGE MultiParamTypeClasses #-}

module Transform.Common where

import Control.Lens hiding (List)
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens


-- * Transform

transformRange :: RustNotebookTransformer -> Range -> Maybe Range
transformRange = transformRanged

transformRanged :: (HasRange a Range) => RustNotebookTransformer -> a -> Maybe a
transformRanged tx x = x
  & traverseOf (range . start) (transformPosition transformerParams tx)
  >>= traverseOf (range . end) (transformPosition transformerParams tx)

-- * Untransform

untransformRange :: RustNotebookTransformer -> Range -> Maybe Range
untransformRange = untransformRanged

untransformRanged :: (HasRange a Range) => RustNotebookTransformer -> a -> Maybe a
untransformRanged tx x = x
  & traverseOf (range . start) (untransformPosition transformerParams tx)
  >>= traverseOf (range . end) (untransformPosition transformerParams tx)

untransformRangedMaybe :: (HasRange a (Maybe Range)) => RustNotebookTransformer -> a -> Maybe a
untransformRangedMaybe tx x = x
  & traverseOf (range . _Just . start) (untransformPosition transformerParams tx)
  >>= traverseOf (range . _Just . end) (untransformPosition transformerParams tx)

-- * Orphan (wish this was in lsp-types)

instance HasRange Range Range where
  range = Prelude.id
