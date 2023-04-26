{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Transform.ServerRsp.Hover (
  fixupHoverText

  , mkDocRegex

  -- For testing
  , fixupDocumentReferences'
  ) where

import Control.Lens hiding (List)
import Control.Lens.Regex.Text
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.List as L
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import Data.Text.Rope (Rope)
import Language.LSP.Notebook
import Language.LSP.Transformer
import Language.LSP.Types
import Language.LSP.Types.Lens hiding (id, trace)
import Safe
import Text.Regex.PCRE.Light
import Transform.Util
import UnliftIO.MVar


fixupHoverText :: (TransformerMonad n) => Hover -> n Hover
fixupHoverText initialHover = do
  documentsMap <- ask >>= (readMVar . transformerDocuments)
  flip fix (initialHover, M.toList documentsMap) $ \loop args -> case args of
    (paramsInProgress, []) -> return paramsInProgress
    (paramsInProgress, (_, DocumentState {..}):xs) -> do
      newParams <- traverseOf contents (fixupDocumentReferences referenceRegex transformer curLines) paramsInProgress
      loop (newParams, xs)

fixupDocumentReferences :: forall n. MonadLogger n => Regex -> RustNotebookTransformer -> Rope -> HoverContents -> n HoverContents
fixupDocumentReferences docRegex transformer _curLines (HoverContents (MarkupContent k t)) = (HoverContents . MarkupContent k) <$> (fixupDocumentReferences' docRegex transformer t)
fixupDocumentReferences docRegex transformer _curLines (HoverContentsMS mss) = HoverContentsMS <$> (mapM transformMarkedString mss)
  where
    transformMarkedString :: MarkedString -> n MarkedString
    transformMarkedString (PlainString t) = PlainString <$> (fixupDocumentReferences' docRegex transformer t)
    transformMarkedString (CodeString (LanguageString l t)) = (CodeString . LanguageString l) <$> (fixupDocumentReferences' docRegex transformer t)

fixupDocumentReferences' :: forall n. MonadLogger n => Regex -> RustNotebookTransformer -> Text -> n Text
fixupDocumentReferences' docRegex transformer t =
  traverseOf ((regexing docRegex) . groups) (transformGroup transformer) t

  where
    transformGroup :: RustNotebookTransformer -> [Text] -> n [Text]
    transformGroup transformer orig@[(readMay . T.unpack) -> Just line, (readMay . T.unpack) -> Just ch] = do
      case untransformPosition transformerParams transformer (Position (line - 1) (ch - 1)) of
        Nothing -> return orig
        Just (Position line' ch') -> return [T.pack $ show (line' + 1), T.pack $ show (ch' + 1)]

    transformGroup _ matches = return matches

mkDocRegex :: Text -> Regex
mkDocRegex docName = compile [i|#{escapedName}:(\\d+):(\\d+)|] [utf8]
  where
    escapedName = docName
      & (\x -> if filePrefix `T.isPrefixOf` x then T.drop (T.length filePrefix) x else x)
      & L.foldl' (.) id [T.replace c ("\\" <> c) | c <- pcreChars]

    filePrefix = "file://"

    pcreChars = [".", "^", "$", "*", "+", "?", "(", ")", "[", "{", "\\", "|"]
