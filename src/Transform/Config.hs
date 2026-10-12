{-# LANGUAGE RankNTypes #-}

module Transform.Config (rustNotebookConfig) where

import Control.Lens hiding ((<.>))
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Map as M
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Rope as Rope
import Language.LSP.Notebook
import Language.LSP.NotebookLanguageServer.Config
import Language.LSP.NotebookLanguageServer.Transform.ServerRsp.Hover (mkDocRegex)
import Language.LSP.NotebookLanguageServer.Transform.Util
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Types
import Language.LSP.Transformer (Doc)
import Text.Regex.PCRE.Light (Regex)
import System.FilePath
import UnliftIO.Directory
import UnliftIO.MVar


rustNotebookConfig :: NotebookConfig RustNotebookTransformer
rustNotebookConfig = NotebookConfig {
  notebookConfigTransformerParams = transformerParams
  , notebookConfigIdTransformerParams = idTransformerParams
  , notebookConfigMakeShadowDocument = makeShadowDocument
  , notebookConfigUnopenedUri = addExtensionToUri ".rs"
  , notebookConfigOnDocumentsChanged = updateLibRs
  , notebookConfigTransformInitialize = transformInitialize
  , notebookConfigCustomUriMethods = ["rust-analyzer/analyzerStatus"]
  , notebookConfigLanguageName = "rust"
  }

-- | rust-analyzer gets pointed at the shadow crate rather than the notebook's directory, and
-- we need didSave notifications to keep the crate on disk up to date.
transformInitialize :: TransformerMonad RustNotebookTransformer n => InitializeParams -> n InitializeParams
transformInitialize params = do
  dir <- asks transformerShadowDir
  pure $ params
    & set rootPath (Just (InL (T.pack dir)))
    & set rootUri (InL (filePathToUri dir))
    & set (capabilities . textDocument . _Just . synchronization) (Just $ TextDocumentSyncClientCapabilities {
                                                                      _dynamicRegistration = Just False
                                                                      , _willSave = Just True
                                                                      , _willSaveWaitUntil = Just False
                                                                      , _didSave = Just True
                                                                      })

makeShadowDocument :: TransformerMonad RustNotebookTransformer n => Uri -> Doc -> n (Uri, FilePath, Regex)
makeShadowDocument _origUri projected = do
  TransformerState {transformerShadowDir} <- ask
  identifier <- makeUUID' 15
  let path = transformerShadowDir </> "src" </> identifier <.> "rs"
  createDirectoryIfMissing True (takeDirectory path)
  liftIO $ T.writeFile path (Rope.toText projected)
  pure (filePathToUri path, path, mkDocRegex (T.pack (identifier <.> "rs")))

updateLibRs :: TransformerMonad RustNotebookTransformer m => m ()
updateLibRs = do
  TransformerState {..} <- ask
  docs <- readMVar transformerDocuments
  let identifiers = [dropExtension (takeFileName newPath) | DocumentState {..} <- fmap snd (M.toList docs)]
  let libRsContent = T.intercalate "\n" [T.pack ("mod " <> x <> ";") | x <- identifiers]
  logInfoN [i|libRsContent: #{libRsContent}|]
  liftIO $ createDirectoryIfMissing True (transformerShadowDir </> "src")
  liftIO $ T.writeFile (transformerShadowDir </> "src" </> "lib.rs") libRsContent
