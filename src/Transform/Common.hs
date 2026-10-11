{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Transform.Common where

import Control.Lens hiding (List)
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Map as M
import Data.Maybe
import qualified Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Lens as Lens
import System.FilePath
import Transform.Util
import UnliftIO.Directory
import UnliftIO.MVar


-- * Transform

transformRange :: RustNotebookTransformer -> Range -> Maybe Range
transformRange = transformRanged

transformRanged :: (HasRange a Range) => RustNotebookTransformer -> a -> Maybe a
transformRanged tx x = x
  & traverseOf (range . start) (transformPosition (getParams tx) tx)
  >>= traverseOf (range . end) (transformPosition (getParams tx) tx)

-- * Untransform

untransformRange :: RustNotebookTransformer -> Range -> Maybe Range
untransformRange = untransformRanged

untransformRanged :: (HasRange a Range) => RustNotebookTransformer -> a -> Maybe a
untransformRanged tx x = x
  & traverseOf (range . start) (untransformPosition (getParams tx) tx)
  >>= traverseOf (range . end) (untransformPosition (getParams tx) tx)

untransformRangedMaybe :: (HasRange a (Maybe Range)) => RustNotebookTransformer -> a -> Maybe a
untransformRangedMaybe tx x = x
  & traverseOf (range . _Just . start) (untransformPosition (getParams tx) tx)
  >>= traverseOf (range . _Just . end) (untransformPosition (getParams tx) tx)

-- | An inlay hint sits at a position rather than over a range, and carries its own edits for
-- when the user accepts it. Drop the hint if its position isn't in the cell; drop individual
-- edits that aren't.
untransformInlayHint :: RustNotebookTransformer -> InlayHint -> Maybe InlayHint
untransformInlayHint tx hint =
  hint
    & traverseOf position (untransformPosition (getParams tx) tx)
    & fmap (over (textEdits . _Just) (Data.Maybe.mapMaybe (untransformRanged tx)))

untransformWorkspaceEdit :: DocumentState -> WorkspaceEdit -> Maybe WorkspaceEdit
untransformWorkspaceEdit (DocumentState {transformer=tx, origUri=notebookUri, newUri=shadowUri}) wsEdit = do
  changes' <- traverse untransformChanges (wsEdit ^. changes)
  documentChanges' <- traverse (traverse untransformDocumentChange) (wsEdit ^. documentChanges)
  pure $ wsEdit
    & set changes changes'
    & set documentChanges documentChanges'
  where
    untransformChanges m = M.fromList <$> traverse untransformEntry (M.toList m)

    untransformEntry (u, es)
      | u == shadowUri = (,) notebookUri <$> traverse (untransformRanged tx) es
      | otherwise = Nothing

    untransformDocumentChange (InL textDocumentEdit) = InL <$> untransformTextDocumentEdit textDocumentEdit
    untransformDocumentChange (InR _createRenameOrDelete) = Nothing

    untransformTextDocumentEdit tde
      | tde ^. textDocument . uri /= shadowUri = Nothing
      | otherwise = tde
          & set (textDocument . uri) notebookUri
          & set (textDocument . version) (InR Null)
          & traverseOf (edits . traverse) untransformEdit

    untransformEdit (InL textEdit) = InL <$> untransformRanged tx textEdit
    untransformEdit (InR annotatedTextEdit) = InR <$> untransformRanged tx annotatedTextEdit

untransformCodeAction :: DocumentState -> CodeAction -> Maybe CodeAction
untransformCodeAction ds codeAction = case codeAction ^. edit of
  Nothing -> Just codeAction
  Just wsEdit -> (\wsEdit' -> codeAction & set edit (Just wsEdit')) <$> untransformWorkspaceEdit ds wsEdit

workspaceEditUris :: WorkspaceEdit -> [Uri]
workspaceEditUris wsEdit =
  M.keys (fromMaybe mempty (wsEdit ^. changes))
  <> [tde ^. textDocument . uri | InL tde <- fromMaybe [] (wsEdit ^. documentChanges)]

-- * Orphan (wish this was in lsp-types)

instance HasRange Range Range where
  range = Prelude.id

-- * Update the shadow lib.rs file

updateLibRs :: TransformerMonad m => m ()
updateLibRs = do
  TransformerState {..} <- ask
  docs <- readMVar transformerDocuments
  let identifiers = [dropExtension (takeFileName newPath) | DocumentState {..} <- fmap snd (M.toList docs)]
  let libRsContent = T.intercalate "\n" [T.pack ("mod " <> x <> ";") | x <- identifiers]
  logInfoN [i|libRsContent: #{libRsContent}|]
  liftIO $ createDirectoryIfMissing True (transformerShadowDir </> "src")
  liftIO $ T.writeFile (transformerShadowDir </> "src" </> "lib.rs") libRsContent
