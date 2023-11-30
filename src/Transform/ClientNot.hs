{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Transform.ClientNot where

import Control.Lens hiding ((:>), (<.>), List)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Rope as Rope
import Data.Time
import Language.LSP.Notebook
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import System.FilePath
import Transform.Common
import Transform.ServerRsp.Hover (mkDocRegex)
import Transform.Util
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Directory
import UnliftIO.MVar


type ClientNotMethod m = SMethod (m :: Method 'ClientToServer 'Notification)

type SendExtraNotificationFn n = forall (o :: Method 'ClientToServer 'Notification). ToJSON (TNotificationMessage o) => TNotificationMessage o -> n ()

transformClientNot :: (
  TransformerMonad n, HasJSON (TNotificationMessage m)
  ) => SendExtraNotificationFn n
    -> ClientNotMethod m
    -> TNotificationMessage m
    -> n (TNotificationMessage m)
transformClientNot sendExtraNotification meth msg = do
  start <- liftIO getCurrentTime
  p' <- transformClientNot' sendExtraNotification meth (msg ^. params)
  stop <- liftIO getCurrentTime
  let msg' = set params p' msg
  when (msg' /= msg) $ logDebugN [i|Transforming client not #{meth} in #{diffUTCTime stop start}: (#{A.encode msg} --> #{A.encode msg'})|]
  return msg'

transformClientNot' :: (
  TransformerMonad n
  ) => (forall (o :: Method 'ClientToServer 'Notification). ToJSON (TNotificationMessage o) => TNotificationMessage o -> n ())
    -> ClientNotMethod m
    -> MessageParams m
    -> n (MessageParams m)

transformClientNot' _ SMethod_TextDocumentDidOpen params = whenAnything params $ \u -> do
  let t = params ^. (textDocument . text)
  let ls = Rope.fromText t
  let txParams = if isNotebook u then transformerParams else idTransformerParams
  let (ls', transformer' :: RustNotebookTransformer) = project txParams ls
  TransformerState {..} <- ask
  (newPath, referenceRegex) <- do
    identifier <- makeUUID' 15
    let path = transformerShadowDir </> "src" </> identifier <.> "rs"

    newPath <- do
      createDirectoryIfMissing True (takeDirectory path)
      liftIO $ T.writeFile path (Rope.toText ls')
      return path

    pure (newPath, mkDocRegex (T.pack (identifier <.> "rs")))

  let newUri = filePathToUri newPath

  modifyMVar_ transformerDocuments $ \x -> return $! M.insert (getUri u) (
    DocumentState {
        transformer = transformer'
        , curLines = ls
        , curLines' = ls'
        , origUri = u
        , newUri = newUri
        , newPath = newPath
        , referenceRegex = referenceRegex
        }) x

  updateLibRs

  return $ params
         & set (textDocument . text) (Rope.toText ls')
         & set (textDocument . uri) newUri

transformClientNot' sendExtraNotification SMethod_TextDocumentDidChange params = whenAnything params $ modifyTransformer params $ \ds@(DocumentState {transformer=tx, curLines=before, curLines'=before', origUri, newUri, newPath}) -> do
  let txParams = if isNotebook origUri then transformerParams else idTransformerParams
  let changeEvents = params ^. contentChanges
  let (changeEvents', tx') = handleDiffMulti txParams before changeEvents tx
  let after = applyChanges changeEvents before
  let after' = applyChanges changeEvents' before'

  whenServerCapabilitiesSatisfy supportsWillSave $ \_ ->
    sendExtraNotification $ TNotificationMessage "2.0" SMethod_TextDocumentWillSave $ WillSaveTextDocumentParams {
      _textDocument = TextDocumentIdentifier newUri
      , _reason = TextDocumentSaveReason_AfterDelay
      }

  liftIO $ T.writeFile newPath (Rope.toText after')

  whenServerCapabilitiesSatisfy supportsSave $ \maybeSaveOptions ->
    void $ async $ do
      threadDelay 5_000_000
      sendExtraNotification $ TNotificationMessage "2.0" SMethod_TextDocumentDidSave $ DidSaveTextDocumentParams {
        _textDocument = TextDocumentIdentifier newUri
        , _text = case maybeSaveOptions of
            Just (SaveOptions {_includeText=(Just True)}) -> Just (Rope.toText after')
            _ -> Nothing
        }

  return (
    ds { transformer = tx'
       , curLines = after
       , curLines' = after'
       }
    , params & set contentChanges changeEvents'
             & set (textDocument . uri) newUri
    )

transformClientNot' _ SMethod_TextDocumentDidClose params = whenAnything params $ \u -> do
  TransformerState {..} <- ask
  maybeDocumentState <- modifyMVar transformerDocuments (return . flipTuple . M.updateLookupWithKey (\_ _ -> Nothing) (getUri u))
  newUri <- case maybeDocumentState of
    Just (DocumentState {..}) -> do
      removePathForcibly newPath
      pure newUri
    Nothing -> addExtensionToUri ".rs" u -- The client shouldn't be closing a non-open doc
  updateLibRs
  return $ params
         & set (textDocument . uri) newUri

transformClientNot' _ _ params = return params
