{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Test where

import Control.Monad
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import GHC
import qualified GHC.Paths


main :: IO ()
main = do
  let text = prints
  locatedCodeBlocks :: [Text] <- undefined
  forM_ locatedCodeBlocks print


prints :: Text
prints =
  [__i|println!("Hello world");
       eprintln!("Hello error");
       format!("Hello {}", "world")
      |]
