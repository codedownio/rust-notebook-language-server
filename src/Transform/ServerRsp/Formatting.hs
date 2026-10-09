module Transform.ServerRsp.Formatting (
  untransformFormattingEdits

  -- For testing
  , applyTextEdits
  ) where

import Control.Lens hiding (List)
import qualified Data.List as L
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.Rope as Rope
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import Transform.Util


-- | Map a @textDocument/formatting@ response back onto the original document.
--
-- Unlike the other responses we rewrite, these edits can't be taken one at a time: a formatter
-- reflows the whole document, and the edits only mean anything applied together. So we apply
-- them to our copy of the shadow document, hand that to 'unproject' to get back to the
-- original's coordinates, and return a single edit replacing the document.
--
-- 'unproject' answers 'Nothing' when the projection can't be undone for this document, which
-- today means a cell with directive lines in it. Returning no edits leaves the cell alone,
-- which beats handing back one with the user's @:dep@ lines dropped.
untransformFormattingEdits :: DocumentState -> [TextEdit] -> [TextEdit]
untransformFormattingEdits _ [] = []
untransformFormattingEdits (DocumentState {transformer=tx, curLines, curLines'}) edits =
  case unproject (getParams tx) tx formatted of
    Nothing -> []
    Just unprojected
      -- Nothing was undone, so this isn't a notebook and the shadow document is a copy of the
      -- original. The server's own edits already line up, and they're smaller than replacing
      -- the whole document.
      | unprojected == formatted -> edits
      | otherwise -> [TextEdit wholeDocument (Rope.toText unprojected <> trailingNewline)]
  where
    formatted = applyTextEdits edits curLines'

    -- unproject drops the newline the formatter added along with the wrapper, so put one back
    -- only if the document we're replacing had one.
    trailingNewline = if "\n" `T.isSuffixOf` Rope.toText curLines then "\n" else ""

    wholeDocument = Range (Position 0 0) (Position (fromIntegral endLine) (fromIntegral endColumn))
      where Rope.Position endLine endColumn = Rope.lengthAsPosition curLines

-- | Apply LSP text edits to a document. They're non-overlapping and expressed against the
-- document as it stands, so applying them last-first keeps the earlier positions valid.
applyTextEdits :: [TextEdit] -> Doc -> Doc
applyTextEdits edits doc = applyChanges (fmap toChangeEvent sorted) doc
  where
    sorted = L.sortOn (Down . (^. (range . start))) edits
    toChangeEvent (TextEdit r t) =
      TextDocumentContentChangeEvent $ InL $ TextDocumentContentChangePartial r Nothing t
