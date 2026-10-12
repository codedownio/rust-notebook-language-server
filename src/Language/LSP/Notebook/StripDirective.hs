{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.LSP.Notebook.StripDirective where

import Data.Foldable
import Data.Sequence hiding (zip)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Protocol.Types
import Language.LSP.Transformer


data StripDirective = StripDirective Bool (Set.Set UInt)
  deriving (Show)

data SDParams = SDParams { sdParamsEnable :: Bool }
  deriving (Show)

instance Transformer StripDirective where
  type Params StripDirective = SDParams

  -- Directive lines are blanked, and a formatter is free to delete blank lines, so once the
  -- document has been rewritten there's no telling where they went. Say so instead of
  -- handing back a cell with the user's :dep lines missing.
  unproject :: StripDirective -> Doc -> Maybe Doc
  unproject (StripDirective _ affectedLines) doc
    | Set.null affectedLines = Just doc
    | otherwise = Nothing

  transformPosition :: StripDirective -> Position -> Maybe Position
  transformPosition (StripDirective _ affectedLines) (Position l c)
    | l `Set.member` affectedLines = Just $ Position l 0
    | otherwise = Just $ Position l c

  untransformPosition :: StripDirective -> Position -> Maybe Position
  untransformPosition (StripDirective _ affectedLines) (Position l c)
    | l `Set.member` affectedLines = Just $ Position l 0
    | otherwise = Just $ Position l c


isDirectiveLine :: Text -> Bool
isDirectiveLine t = ":" `T.isPrefixOf` t

instance Monad m => ProjectIn m StripDirective where
  project :: Params StripDirective -> Doc -> m (Doc, StripDirective, Either Text ())
  project (SDParams False) doc = pure (doc, StripDirective False mempty, Right ())
  project (SDParams True) (docToList -> ls) = pure (go mempty mempty (zip ls [0..]))
    where
      go affectedLines processedLines [] = (listToDoc (toList processedLines), StripDirective True affectedLines, Right ())
      go affectedLines processedLines ((line, n):rest)
        | isDirectiveLine line = go (Set.insert n affectedLines) (processedLines |> "") rest
        | otherwise = go affectedLines (processedLines |> line) rest
