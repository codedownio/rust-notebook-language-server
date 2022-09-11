{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Transform.ServerRsp where

import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.Logger
import Data.String.Interpolate
import Language.LSP.Notebook (HaskellNotebookTransformer)
import Language.LSP.Notebook.ExpressionToDeclaration (containsExpressionVariable, isExpressionVariable)
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens
import Transform.Util


type ServerRspMethod m = SMethod (m :: Method FromClient Request)

transformServerRsp :: (TransformerMonad n) => ServerRspMethod m -> MessageParams m -> ResponseMessage m -> n (ResponseMessage m)
transformServerRsp meth initialParams msg = do
  logInfoN [i|Transforming server response #{meth}|]
  case msg ^. result of
    Left err -> return msg
    Right ret -> do
      p' <- transformServerRsp' meth initialParams ret
      return $ set result (Right p') msg

transformServerRsp' :: (TransformerMonad n) => ServerRspMethod m -> MessageParams m -> ResponseResult m -> n (ResponseResult m)
transformServerRsp' STextDocumentDocumentHighlight initialParams result = whenNotebookResult initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) -> do
  return $ fmap (untransformRanged tx) result
transformServerRsp' STextDocumentHover initialParams result = whenNotebookResult initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) -> do
  return (untransformRangedMaybe tx <$> result)
transformServerRsp' STextDocumentDocumentSymbol initialParams result = whenNotebookResult initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
  case result of
    InL (List documentSymbols) -> return $ InL (List (filter (not . isInternalSymbol) documentSymbols))
    InR (List symbolInformations) -> return $ InR (List (filter (not . isInternalSymbol) symbolInformations))
  where
    isInternalSymbol x = isExpressionVariable expressionToDeclarationParams (x ^. name)
transformServerRsp' STextDocumentCodeAction initialParams result@(List xs) = whenNotebookResult initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) -> do
  List <$> filterM (fmap not . isInternalReferringCodeAction) xs
  where
    isInternalReferringCodeAction (InL command) = containsExpressionVariable expressionToDeclarationParams (command ^. title)
    isInternalReferringCodeAction (InR codeAction) = containsExpressionVariable expressionToDeclarationParams (codeAction ^. title)
transformServerRsp' _ _ result = return result


untransformRanged :: (HasRange a Range) => HaskellNotebookTransformer -> a -> a
untransformRanged tx x = x
  & over (range . start) (untransformPosition transformerParams tx)
  & over (range . end) (untransformPosition transformerParams tx)

untransformRangedMaybe :: (HasRange a (Maybe Range)) => HaskellNotebookTransformer -> a -> a
untransformRangedMaybe tx x = x
  & over (range . _Just . start) (untransformPosition transformerParams tx)
  & over (range . _Just . end) (untransformPosition transformerParams tx)