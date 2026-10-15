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


rustNotebookConfig :: FilePath -> Int -> NotebookConfig RustNotebookTransformer
rustNotebookConfig shadowDir didSaveDebounceMs = NotebookConfig {
  notebookConfigHandles = const True
  , notebookConfigDidSaveDebounceMs = Just didSaveDebounceMs
  , notebookConfigHandlesFormatting = True
  , notebookConfigTransformerParamsFor = \u -> if isNotebook u then transformerParams else idTransformerParams
  , notebookConfigMakeShadowDocument = makeShadowDocument shadowDir
  , notebookConfigUnopenedUri = addExtensionToUri ".rs"
  , notebookConfigOnDocumentsChanged = updateLibRs shadowDir
  , notebookConfigTransformInitialize = transformInitialize shadowDir
  , notebookConfigCustomUriMethods = ["rust-analyzer/analyzerStatus"]
  , notebookConfigIgnoreSymbol = const False
  , notebookConfigLanguageName = "rust"
  }

-- | rust-analyzer gets pointed at the shadow crate rather than the notebook's directory, and
-- we need didSave notifications to keep the crate on disk up to date.
transformInitialize :: TransformerMonad RustNotebookTransformer n => FilePath -> InitializeParams -> n InitializeParams
transformInitialize shadowDir params = pure $ params
  & set rootPath (Just (InL (T.pack shadowDir)))
  & set rootUri (InL (filePathToUri shadowDir))
  & set (capabilities . textDocument . _Just . synchronization) (Just $ TextDocumentSyncClientCapabilities {
                                                                    _dynamicRegistration = Just False
                                                                    , _willSave = Just True
                                                                    , _willSaveWaitUntil = Just False
                                                                    , _didSave = Just True
                                                                    })

makeShadowDocument :: TransformerMonad RustNotebookTransformer n => FilePath -> Uri -> Doc -> n (Uri, FilePath, Regex)
makeShadowDocument shadowDir _origUri projected = do
  identifier <- makeUUID' 15
  let path = shadowDir </> "src" </> identifier <.> "rs"
  createDirectoryIfMissing True (takeDirectory path)
  liftIO $ T.writeFile path (Rope.toText projected)
  pure (filePathToUri path, path, mkDocRegex (T.pack (identifier <.> "rs")))

updateLibRs :: TransformerMonad RustNotebookTransformer m => FilePath -> m ()
updateLibRs shadowDir = do
  TransformerState {..} <- ask
  docs <- readMVar transformerDocuments
  let identifiers = [dropExtension (takeFileName newPath) | DocumentState {..} <- fmap snd (M.toList docs)]
  let libRsContent = T.intercalate "\n" [T.pack ("mod " <> x <> ";") | x <- identifiers]
  logInfoN [i|libRsContent: #{libRsContent}|]
  liftIO $ createDirectoryIfMissing True (shadowDir </> "src")
  liftIO $ T.writeFile (shadowDir </> "src" </> "lib.rs") libRsContent
