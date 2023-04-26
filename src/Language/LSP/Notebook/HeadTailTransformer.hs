{-# LANGUAGE TypeFamilies #-}

module Language.LSP.Notebook.HeadTailTransformer where

import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Rope as Rope
import Language.LSP.Transformer
import Language.LSP.Types


data HeadTailTransformer = HeadTailTransformer UInt UInt UInt
  deriving (Show)

instance Transformer HeadTailTransformer where
  type Params HeadTailTransformer = ([Text], [Text])

  project (initialLines, finalLines) ls = (beginning <> ls <> ending, tx)
    where
      beginning = case initialLines of
        [] -> ""
        xs -> joinLines xs <> "\n"

      ending = case finalLines of
        [] -> ""
        xs -> "\n" <> joinLines xs

      joinLines = Rope.fromText . T.intercalate "\n"

      tx = HeadTailTransformer (fromIntegral (L.length initialLines))
                               (fromIntegral (lengthInLines ls))
                               (fromIntegral (L.length finalLines))

  transformPosition _ (HeadTailTransformer s _d _e) (Position l c) = Just $ Position (l + s) c

  untransformPosition _ (HeadTailTransformer s d _e) (Position l c)
    | l >= s + d = Nothing
    | otherwise = Just $ Position (l - s) c


lengthInLines :: Rope.Rope -> Word
lengthInLines rp | Rope.null rp = 0
lengthInLines rp = (+ 1) $ Rope.posLine $ Rope.lengthAsPosition rp
