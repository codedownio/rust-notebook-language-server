{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Transform.Util where

import Control.Lens hiding ((:>), (<.>))
import Control.Lens.Regex.Text
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Char as C
import qualified Data.List as L
import Data.Map as M
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Capabilities
import Language.LSP.Types.Lens as Lens
import Network.URI
import System.FilePath
import qualified System.Random as R
import UnliftIO.MVar


whenAnything :: (MonadLoggerIO n, HasTextDocument a b, HasUri b Uri) => a -> (Uri -> n a) -> n a
whenAnything params cb = cb (params ^. (textDocument . uri))

whenNotebook :: (MonadLoggerIO n, HasTextDocument a b, HasUri b Uri) => a -> (Uri -> n a) -> n a
whenNotebook params = whenNotebook' (params ^. (textDocument . uri)) params

whenNotebookUri :: (MonadLoggerIO n, HasUri a Uri) => a -> (Uri -> n a) -> n a
whenNotebookUri params = whenNotebook' (params ^. uri) params

whenNotebookByInitialParams :: (MonadLoggerIO n, HasTextDocument a b, HasUri b Uri) => a -> c -> (Uri -> n c) -> n c
whenNotebookByInitialParams params = whenNotebook' (params ^. (textDocument . uri))

whenNotebook' :: (MonadLoggerIO n) => Uri -> a -> (Uri -> n a) -> n a
whenNotebook' uri params notebookParams = if
  | isNotebook uri -> notebookParams uri
  | otherwise -> pure params

isNotebook :: Uri -> Bool
isNotebook uri = case parseURIReference (T.unpack (getUri uri)) of
  Nothing -> False
  Just (URI {..}) -> ".ipynb" `L.isSuffixOf` fmap C.toLower uriPath

-- * whenNotebookResult

whenReverseLookupUri :: (TransformerMonad n, MonadLoggerIO n, HasUri a Uri) => a -> (DocumentState -> n a) -> n a
whenReverseLookupUri params = whenReverseLookupUri' (params ^. uri) params

-- | Note that this takes in server URIs and calls the callback with original URIs
-- TODO: make this more efficient
whenReverseLookupUri' :: (TransformerMonad n) => Uri -> a -> (DocumentState -> n a) -> n a
whenReverseLookupUri' uri params cb = do
  txDocumentsVar <- asks transformerDocuments
  txDocuments <- readMVar txDocumentsVar
  flip fix (M.toList txDocuments) $ \loop items -> case items of
    [] -> return params
    ((_, ds@(DocumentState {..})):rest) ->
      if | newUri == uri -> cb ds
         | otherwise -> loop rest

-- * TransformerMonad

type TransformerMonad n = (
  MonadLoggerIO n
  , MonadReader TransformerState n
  , MonadUnliftIO n
  , MonadFail n
  )

-- * TransformerState

data DocumentState = DocumentState {
  transformer :: RustNotebookTransformer
  , curLines :: Doc
  , origUri :: Uri
  , newUri :: Uri
  , newPath :: FilePath
  , referenceRegex :: Regex
  }

data TransformerState = TransformerState {
  transformerDocuments :: MVar (Map Text DocumentState)
  , transformerInitializeParams :: MVar (Maybe InitializeParams)
  , transformerInitializeResult :: MVar (Maybe InitializeResult)
  , transformerShadowDir :: FilePath
  }

-- * Transformers

newTransformerState :: (MonadIO m) => FilePath -> m TransformerState
newTransformerState shadowDir = TransformerState
  <$> newMVar mempty
  <*> newMVar Nothing
  <*> newMVar Nothing
  <*> pure shadowDir

lookupTransformer :: TransformerMonad m => Uri -> m (Maybe DocumentState)
lookupTransformer uri = do
  TransformerState {..} <- ask
  M.lookup (getUri uri) <$> readMVar transformerDocuments

withTransformer :: (TransformerMonad n) => a -> (DocumentState -> n a) -> Uri -> n a
withTransformer def cb uri = do
  lookupTransformer uri >>= \case
    Nothing -> do
      logWarnN [i|Couldn't find expected transformer for uri #{uri}|]
      return def
    Just tx -> cb tx

modifyTransformer :: (TransformerMonad n) => a -> (DocumentState -> n (DocumentState, a)) -> Uri -> n a
modifyTransformer def cb uri = do
  TransformerState {..} <- ask
  modifyMVar transformerDocuments $ \m -> case M.lookup (getUri uri) m of
    Nothing -> return (m, def)
    Just tx -> do
      (tx', ret) <- cb tx
      return (M.insert (getUri uri) tx' m, ret)

-- * Checking server capabilities

whenServerCapabilitiesSatisfy :: TransformerMonad n => (ServerCapabilities -> (Bool, a)) -> (a -> n ()) -> n ()
whenServerCapabilitiesSatisfy cb action = do
  initializeResultVar <- asks transformerInitializeResult
  readMVar initializeResultVar >>= \case
    Just ((cb . (^. capabilities)) -> (True, sideValue)) -> action sideValue
    _ -> return ()

supportsWillSave :: ServerCapabilities -> (Bool, ())
supportsWillSave (ServerCapabilities { _textDocumentSync=(Just (InL (TextDocumentSyncOptions {_willSave=(Just True)}))) }) = (True, ())
supportsWillSave _ = (False, ())

supportsSave :: ServerCapabilities -> (Bool, Maybe SaveOptions)
supportsSave (ServerCapabilities { _textDocumentSync=(Just (InL (TextDocumentSyncOptions {_save}))) }) = case _save of
  Nothing -> (False, Nothing)
  Just (InL x) -> (x, Nothing)
  Just (InR saveOptions) -> (True, Just saveOptions)
supportsSave _ = (False, Nothing)

-- * Random identifiers

-- Note: for a UUID to appear in a Kubernetes name, it needs to match this regex
-- [a-z0-9]([-a-z0-9]*[a-z0-9])?(\.[a-z0-9]([-a-z0-9]*[a-z0-9])?)*'
uuidLetters :: [Char]
uuidLetters = ['a'..'z'] ++ ['A'..'Z']

numUUIDLetters :: Int
numUUIDLetters = L.length uuidLetters

makeUUID' :: MonadIO m => Int -> m String
makeUUID' n = replicateM n ((uuidLetters L.!!) <$> R.randomRIO (0, numUUIDLetters - 1))

-- * Misc

addExtensionToUri :: (MonadLogger m) => String -> Uri -> m Uri
addExtensionToUri ext u@(Uri t) = case parseURIReference (T.unpack t) of
  Nothing -> do
    logErrorN [i|Couldn't parse URI: #{t}|]
    return u
  Just uri -> return $ Uri $ T.pack $ show $ uri { uriPath = uriPath uri <.> ext }

flipTuple :: (b, a) -> (a, b)
flipTuple (x, y) = (y, x)
