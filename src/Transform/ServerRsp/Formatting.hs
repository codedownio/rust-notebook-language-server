{-# LANGUAGE TypeOperators #-}

module Transform.ServerRsp.Formatting (
  untransformFormattingEdits

  -- For testing
  , applyTextEdits
  , dedent
  ) where

import Control.Lens hiding ((:>), List)
import Data.Function
import qualified Data.List as L
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Rope as Rope
import Language.LSP.Notebook
import Language.LSP.Notebook.StripDirective
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Transform.Util


-- | Map a @textDocument/formatting@ response back onto the original document.
--
-- Unlike the other responses we rewrite, these edits can't be mapped one at a time. The shadow
-- document wraps a notebook's code in @fn main() { ... }@, so rustfmt indents every line of it
-- by one level, and the edits doing that indenting are interleaved with the ones we actually
-- want. Instead we apply the whole edit list to our copy of the shadow document, peel the
-- wrapper back off, undo the indentation it caused, and return a single edit replacing the
-- document.
untransformFormattingEdits :: DocumentState -> [TextEdit] -> [TextEdit]
untransformFormattingEdits _ [] = []
untransformFormattingEdits (DocumentState {transformer=tx, curLines, curLines'}) edits =
  case getParams tx of
    -- No wrapper, so this isn't a notebook and the shadow document is a copy of the original:
    -- the edits already line up.
    _ :> ([], []) -> edits

    -- Directive lines (:dep and friends) are blanked out in the shadow document, so rustfmt is
    -- free to delete them and we have no way to put them back in the right place. Leave the
    -- cell alone rather than eat them.
    _ | hasDirectiveLines tx -> []

    _ :> (initialLines, finalLines) -> [TextEdit wholeDocument newText]
      where
        body = Rope.toText (applyTextEdits edits curLines')
          & T.lines
          & L.drop (L.length initialLines)
          & dropEnd (L.length finalLines)
          & dedent

        -- T.lines drops the trailing newline rustfmt adds, so put one back only if the
        -- document we're replacing had one.
        trailingNewline = if "\n" `T.isSuffixOf` Rope.toText curLines then "\n" else ""

        newText = T.intercalate "\n" body <> trailingNewline

        wholeDocument = Range (Position 0 0) (Position (fromIntegral endLine) (fromIntegral endColumn))
          where Rope.Position endLine endColumn = Rope.lengthAsPosition curLines

hasDirectiveLines :: RustNotebookTransformer -> Bool
hasDirectiveLines (StripDirective _ affectedLines :> _) = not (Set.null affectedLines)

-- | Apply LSP text edits to a document. They're non-overlapping and expressed against the
-- document as it stands, so applying them last-first keeps the earlier positions valid.
applyTextEdits :: [TextEdit] -> Doc -> Doc
applyTextEdits edits doc = applyChanges (fmap toChangeEvent sorted) doc
  where
    sorted = L.sortOn (Down . (^. (range . start))) edits
    toChangeEvent (TextEdit r t) =
      TextDocumentContentChangeEvent $ InL $ TextDocumentContentChangePartial r Nothing t

-- | Undo the indentation that wrapping the code in a function added, by removing the longest
-- whitespace prefix every non-blank line shares.
dedent :: [Text] -> [Text]
dedent ls = case mapMaybe indentOf ls of
  [] -> ls
  indents -> case L.foldr1 commonPrefix indents of
    "" -> ls
    prefix -> fmap (\l -> fromMaybe l (T.stripPrefix prefix l)) ls
  where
    indentOf l
      | T.null (T.strip l) = Nothing
      | otherwise = Just (T.takeWhile (`elem` (" \t" :: String)) l)

    commonPrefix a b = maybe "" (^. _1) (T.commonPrefixes a b)

dropEnd :: Int -> [a] -> [a]
dropEnd n xs = L.take (L.length xs - n) xs
