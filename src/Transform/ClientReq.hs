{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Transform.ClientReq where

import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens
import Transform.Common
import Transform.Util
import UnliftIO.Concurrent


type ClientReqMethod m = SMethod (m :: Method FromClient Request)


transformClientReq :: (TransformerMonad n, HasJSON (RequestMessage m)) => ClientReqMethod m -> RequestMessage m -> n (RequestMessage m)
transformClientReq meth msg = do
  start <- liftIO getCurrentTime
  p' <- transformClientReq' meth (msg ^. params)
  stop <- liftIO getCurrentTime
  let msg' = set params p' msg
  when (msg' /= msg) $ logDebugN [i|Transforming client req #{meth} in #{diffUTCTime stop start}: (#{A.encode msg} --> #{A.encode msg'})|]
  return msg'

transformClientReq' :: forall m n. (TransformerMonad n) => ClientReqMethod m -> MessageParams m -> n (MessageParams m)

transformClientReq' SInitialize params = do
  -- Store the non-modified params, so we can access the unmodified rootUri
  asks transformerInitializeParams >>= flip modifyMVar_ (\_ -> return $ Just params)

  dir <- asks transformerShadowDir
  pure $ params
    & set rootPath (Just (T.pack dir))
    & set rootUri (Just (filePathToUri dir))
    & set (capabilities . textDocument . _Just . synchronization) (Just $ TextDocumentSyncClientCapabilities {
                                                                      _dynamicRegistration = Just False
                                                                      , _willSave = Just True
                                                                      , _willSaveWaitUntil = Just False
                                                                      , _didSave = Just True
                                                                      })

transformClientReq' STextDocumentCodeAction params = whenAnything params $ withTransformer params $ doTransformUriAndRange @m params
transformClientReq' STextDocumentCodeLens params = whenAnything params $ withTransformer params $ doTransformUri @m params
transformClientReq' STextDocumentCompletion params = whenAnything params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' STextDocumentDefinition params = whenAnything params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' STextDocumentDocumentHighlight params = whenAnything params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' STextDocumentDocumentSymbol params = whenAnything params $ withTransformer params $ doTransformUri @m params
transformClientReq' STextDocumentHover params = whenAnything params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' STextDocumentImplementation params = whenAnything params $ withTransformer params $ doTransformUriAndPosition @m params
transformClientReq' STextDocumentTypeDefinition params = whenAnything params $ withTransformer params $ doTransformUriAndPosition @m params

-- Custom methods provided by rust-analyzer
transformClientReq' (SCustomMethod "rust-analyzer/analyzerStatus") val =
  case A.fromJSON val of
    A.Error err -> logErrorN [i|Failed to decode custom method "rust-analyzer/analyzerStatus": #{err}|] >> return val
    A.Success (TextDocumentIdentifier uri) ->
      whenAnything' uri val $
        withTransformer val $ \(DocumentState {newUri}) ->
          return $ A.toJSON $ TextDocumentIdentifier newUri

transformClientReq' _ params = return params


doTransformUriAndPosition :: forall m n a. (
  TransformerMonad n, HasPosition (MessageParams m) Position, HasTextDocument (MessageParams m) a, HasUri a Uri
  ) => MessageParams m -> DocumentState -> n (MessageParams m)
doTransformUriAndPosition params' (DocumentState {transformer=tx, newUri}) = do
  let params = params' & set (textDocument . uri) newUri
  case transformPosition (getParams tx) tx (params ^. position) of
    Nothing -> do
      logWarnN [i|Couldn't transform position #{params ^. position}|]
      return params
    Just pos' -> return $ set position pos' params

doTransformUriAndRange :: forall m n a. (
  TransformerMonad n, HasRange (MessageParams m) Range, HasTextDocument (MessageParams m) a, HasUri a Uri
  ) => MessageParams m -> DocumentState -> n (MessageParams m)
doTransformUriAndRange params' (DocumentState {transformer=tx, newUri}) = do
  let params = params' & set (textDocument . uri) newUri
  case transformRange tx (params ^. range) of
    Nothing -> do
      logWarnN [i|Couldn't transform range #{params ^. range}|]
      return params
    Just pos' -> return $ set range pos' params

doTransformUri :: forall m n a. (
  TransformerMonad n, HasTextDocument (MessageParams m) a, HasUri a Uri
  ) => MessageParams m -> DocumentState -> n (MessageParams m)
doTransformUri params' (DocumentState {newUri}) = return $ params' & set (textDocument . uri) newUri
