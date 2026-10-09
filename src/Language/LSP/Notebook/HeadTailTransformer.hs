{-# LANGUAGE TypeFamilies #-}

module Language.LSP.Notebook.HeadTailTransformer where

import Data.Char (isSpace)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Rope as Rope
import Language.LSP.Protocol.Types
import Language.LSP.Transformer


data HeadTailTransformer = HeadTailTransformer [Text] [Text] UInt UInt UInt
  deriving (Show, Eq)

instance Transformer HeadTailTransformer where
  type Params HeadTailTransformer = ([Text], [Text])

  getParams (HeadTailTransformer initialLines finalLines _ _ _) = (initialLines, finalLines)

  project (initialLines, finalLines) ls = (beginning <> ls <> ending, tx)
    where
      beginning = case initialLines of
        [] -> ""
        xs -> joinLines xs <> "\n"

      ending = case finalLines of
        [] -> ""
        xs -> "\n" <> joinLines xs

      joinLines = Rope.fromText . T.intercalate "\n"

      tx = HeadTailTransformer initialLines
                               finalLines
                               (fromIntegral (L.length initialLines))
                               (fromIntegral (lengthInLines ls))
                               (fromIntegral (L.length finalLines))

  unproject (initialLines, finalLines) _ doc
    | L.null initialLines && L.null finalLines = Just doc
    | otherwise = Just $ listToDoc $ dedent $ dropEnd (L.length finalLines) $ L.drop (L.length initialLines) core
    where
      -- A formatter will have put a newline on the end, which splitOn leaves as a final empty
      -- element. Drop it before counting from the end, or we'd peel off that instead of the
      -- closing line.
      core = case unsnoc (docToList doc) of
        Just (rest, "") -> rest
        _ -> docToList doc

  transformPosition _ (HeadTailTransformer _ _ s _d _e) (Position l c) = Just $ Position (l + s) c

  untransformPosition _ (HeadTailTransformer _ _ s d _e) (Position l c)
    | l < s = Nothing
    | l >= s + d = Nothing
    | otherwise = Just $ Position (l - s) c


lengthInLines :: Rope.Rope -> Word
lengthInLines rp | Rope.null rp = 0
lengthInLines rp = (+ 1) $ Rope.posLine $ Rope.lengthAsPosition rp

-- | Take out the indentation that wrapping the lines in a block introduced.
--
-- The first non-blank line sits at exactly one level -- it starts a statement directly inside
-- the block we added -- so its leading whitespace is the unit to strip. Measuring it rather
-- than assuming four spaces means a formatter configured for tabs or a different width still
-- comes out right, and lines that don't carry it (the interior of a multi-line string, which
-- formatters leave at column 0) are left alone.
dedent :: [Text] -> [Text]
dedent ls = case L.find (not . T.null . T.strip) ls of
  Nothing -> ls
  Just firstLine -> case T.takeWhile isSpace firstLine of
    "" -> ls
    indent -> fmap (\l -> fromMaybe l (T.stripPrefix indent l)) ls

dropEnd :: Int -> [a] -> [a]
dropEnd n xs = L.take (L.length xs - n) xs

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = Just (L.init xs, L.last xs)
