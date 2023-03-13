{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.LSP.Notebook.ExpressionToDeclaration where

import Control.Monad.IO.Class
import Data.Char (isDigit)
import Data.Either (fromRight)
import qualified Data.List as L
import Data.Set as Set
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Rope as Rope
import Data.Vector as V hiding (zip)
import GHC
import qualified GHC.Paths
import IHaskell.Eval.Parser
import Language.Haskell.GHC.Parser as GHC
import Language.LSP.Notebook.Util
import Language.LSP.Transformer
import Language.LSP.Types
import Safe
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.Base (defaultExecOpt)
import Text.Regex.PCRE.Text (Regex, compile, compBlank, execute)


newtype ExpressionToDeclaration = ExpressionToDeclaration (Set UInt)
  deriving Show

data EDParams = EDParams { numberPadding :: Int }

isExpressionVariable :: EDParams -> Text -> Bool
isExpressionVariable (EDParams { numberPadding }) t
  | T.length t == 4 + numberPadding = case T.splitAt 4 t of
      ("expr", digits) -> T.all isDigit digits
      _ -> False
  | otherwise = False

containsExpressionVariable :: (MonadIO m, MonadFail m) => EDParams -> Text -> m Bool
containsExpressionVariable (EDParams {..}) t = do
  Right regex <- liftIO $ compile compBlank defaultExecOpt [i|expr\\d{#{numberPadding}}|]
  liftIO (execute regex t) >>= \case
    Right Nothing -> return False
    Left err -> return False -- TODO: exception?
    _ -> return True

instance Transformer ExpressionToDeclaration where
  type Params ExpressionToDeclaration = EDParams

  project :: Params ExpressionToDeclaration -> Doc -> (Doc, ExpressionToDeclaration)
  project (EDParams {..}) (Rope.lines -> ls) = (listToDoc $ go 0 (zip ls [0 ..]) exprIndices, ExpressionToDeclaration (Set.fromList $ fromIntegral <$> mconcat exprIndices))
    where
      locatedCodeBlocks = unsafePerformIO $ runGhc (Just GHC.Paths.libdir) $ parseString (T.unpack (T.intercalate "\n" ls))

      go :: Int -> [(Text, Int)] -> [[Int]] -> [Text]
      go _ [] _ = []
      go _ xs [] = fmap fst xs
      go _ xs ([]:_) = error "Empty group"
      go counter ((l, i):xs) (group@(i1:is):remainingGroups)
        | i == i1 = let
            prefix = "expr" <> paddedNumber <> " = "
            paddedNumber = T.replicate numZerosNeeded "0" <> T.pack (show counter)
            numZerosNeeded = numberPadding - L.length (show counter)
            prefixLen = 4 + numberPadding + 3

            (extraLinesToProcess, remainingLines) = L.splitAt (L.length is) xs
            blankPadding = T.replicate prefixLen " "
            extraLines = fmap ((blankPadding <>) . fst) extraLinesToProcess

            in
              (prefix <> l) : extraLines <> go (counter + 1) remainingLines remainingGroups
        | otherwise = l : go counter xs (group:remainingGroups)

      exprIndices = [getLinesStartingAt t (GHC.line locatedCodeBlock - 1)
                    | locatedCodeBlock@(unloc -> Expression t) <- locatedCodeBlocks
                    , not (looksLikeTemplateHaskell t)]

  transformPosition :: Params ExpressionToDeclaration -> ExpressionToDeclaration -> Position -> Maybe Position
  transformPosition (EDParams {..}) (ExpressionToDeclaration affectedLines) (Position l c)
    | l `Set.member` affectedLines = Just $ Position l (fromIntegral (fromIntegral c + numberPadding + 7))
    | otherwise = Just $ Position l c

  untransformPosition :: Params ExpressionToDeclaration -> ExpressionToDeclaration -> Position -> Position
  untransformPosition (EDParams {..}) (ExpressionToDeclaration affectedLines) (Position l c)
    | l `Set.member` affectedLines = Position l (fromIntegral (fromIntegral c - numberPadding - 7))
    | otherwise = Position l c


-- | This is a hack to deal with the fact that a top-level TH splice can look like either
-- $(deriveStuff 'f)
-- or
-- deriveStuff 'g
--
-- The latter is indistinguishable from an IHaskell expression, so we require the user to use
-- the former in notebooks.
--
-- See https://downloads.haskell.org/~ghc/7.6.3/docs/html/users_guide/template-haskell.html
looksLikeTemplateHaskell :: String -> Bool
looksLikeTemplateHaskell ('$':'(':rest) = lastMay rest == Just ')'
looksLikeTemplateHaskell _ = False
