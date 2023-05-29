{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Transform.ServerRsp where

import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import Data.Maybe
import Data.String.Interpolate
import Data.Time
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens
import Transform.Common
import Transform.ServerRsp.Hover
import Transform.Util
import UnliftIO.MVar


type ServerRspMethod m = SMethod (m :: Method FromClient Request)

transformServerRsp :: (TransformerMonad n, HasJSON (ResponseMessage m)) => ServerRspMethod m -> MessageParams m -> ResponseMessage m -> n (ResponseMessage m)
transformServerRsp meth initialParams msg = do
  case msg ^. result of
    Left _err -> return msg
    Right ret -> do
      start <- liftIO getCurrentTime
      p' <- transformServerRsp' meth initialParams ret
      stop <- liftIO getCurrentTime
      let msg' = set result (Right p') msg
      when (msg' /= msg) $ logDebugN [i|Transforming server rsp #{meth} in #{diffUTCTime stop start}: (#{A.encode msg} --> #{A.encode msg'})|]
      return $ set result (Right p') msg

transformServerRsp' :: (TransformerMonad n) => ServerRspMethod m -> MessageParams m -> ResponseResult m -> n (ResponseResult m)

transformServerRsp' SInitialize _initialParams result = do
  initializeResultVar <- asks transformerInitializeResult
  modifyMVar_ initializeResultVar (\_ -> return $ Just result)
  return result

transformServerRsp' STextDocumentDocumentHighlight initialParams result@(List inner) =
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    return $ List $ mapMaybe (untransformRanged tx) inner

transformServerRsp' STextDocumentHover initialParams result =
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    case result of
      Nothing -> return Nothing
      Just hov -> do
        hov' <- fixupHoverText hov
        return $ untransformRangedMaybe tx hov'

transformServerRsp' STextDocumentDocumentSymbol initialParams result =
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    case result of
      InL (List documentSymbols) -> return $ InL $ List (documentSymbols & filter (not . ignoreSymbol)
                                                                         & mapMaybe (traverseOf range (untransformRange tx))
                                                                         & mapMaybe (traverseOf selectionRange (untransformRange tx)))
      InR (List symbolInformations) -> return $ InR $ List (symbolInformations & filter (not . ignoreSymbol)
                                                                               & mapMaybe (traverseOf (location . range) (untransformRange tx)))
  where
    ignoreSymbol _ = False

transformServerRsp' STextDocumentCodeAction initialParams result@(List xs) =
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {}) -> do
    List <$> filterM (fmap not . isInternalReferringCodeAction) xs
  where
    isInternalReferringCodeAction (InL _command) = pure False
    isInternalReferringCodeAction (InR _codeAction) = pure False

transformServerRsp' STextDocumentCodeLens initialParams result@(List xs) =
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) -> do
    return $ List $ mapMaybe (untransformRanged tx) xs

transformServerRsp' _ _ result = return result
