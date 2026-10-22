{-# LANGUAGE TypeOperators #-}

-- | 'DocumentState's holding the real 'RustNotebookTransformer', for the transforms that reach
-- the transformer through 'Control.Monad.Reader.MonadReader' rather than taking it as an
-- argument.
module TestLib.Transformers (
  notebookDocumentState
  , plainDocumentState
  , transformerStateWith

  , shadowDir
  , notebookUri
  , notebookShadowUri
  , plainUri
  , plainShadowUri
  ) where

import Data.Functor.Identity
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Language.LSP.Notebook
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp.Hover (mkDocRegex)
import Language.LSP.NotebookLanguageServer.Transform.Util
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import UnliftIO.MVar


-- | A notebook's cells, wrapped in @fn main() {@ the way 'transformerParams' says, so every
-- line of the notebook sits one lower in the shadow document.
notebookDocumentState :: [Text] -> DocumentState RustNotebookTransformer
notebookDocumentState = documentStateFor transformerParams notebookUri notebookShadowPath

-- | A plain @.rs@ file, which takes 'idTransformerParams' and so has no offset at all.
plainDocumentState :: [Text] -> DocumentState RustNotebookTransformer
plainDocumentState = documentStateFor idTransformerParams plainUri plainShadowPath

documentStateFor :: Params RustNotebookTransformer -> Uri -> FilePath -> [Text] -> DocumentState RustNotebookTransformer
documentStateFor params origUri newPath ls = DocumentState {
  transformer = tx
  , curLines = docLines
  , curLines' = projected
  , origUri = origUri
  , newUri = filePathToUri newPath
  , newPath = newPath
  , referenceRegex = mkDocRegex (T.pack newPath)
  , documentUuid = UUID.nil
  , debouncedDidChange = return ()
  }
  where
    docLines = listToDoc ls
    (projected, tx, _) = runIdentity $ project params docLines

-- | A state holding the given open documents, keyed the way the server keys them.
transformerStateWith :: [DocumentState tx] -> IO (TransformerState tx)
transformerStateWith dss = TransformerState
  <$> newMVar (M.fromList [(getUri (origUri ds), ds) | ds <- dss])
  <*> newMVar Nothing
  <*> newMVar Nothing

shadowDir :: FilePath
shadowDir = "/home/shadow"

notebookUri :: Uri
notebookUri = Uri "file:///home/main.ipynb"

notebookShadowUri :: Uri
notebookShadowUri = filePathToUri notebookShadowPath

notebookShadowPath :: FilePath
notebookShadowPath = shadowDir <> "/src/notebook.rs"

plainUri :: Uri
plainUri = Uri "file:///home/plain.rs"

plainShadowUri :: Uri
plainShadowUri = filePathToUri plainShadowPath

plainShadowPath :: FilePath
plainShadowPath = shadowDir <> "/src/plain.rs"
