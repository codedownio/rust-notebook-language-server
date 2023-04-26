{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Test where

import Control.Monad
import Data.String.Interpolate
import Data.Text (Text)

-- import Data.Function
-- import Language.Rust.Parser
-- import Language.Rust.Syntax


-- parsed1 = inputStreamFromString "fn main () { println!(\"Hello world!\"); }"
--         & parse' @(SourceFile Span)

-- parsed2 = inputStreamFromString [i|println!("Hello world");|]
--         & parse' @(SourceFile Span)

-- parsed3 = inputStreamFromString [i|let mut message = "Hello ".to_owned();|]
--         & parse' @(SourceFile Span)

main :: IO ()
main = do
  let _text = prints
  locatedCodeBlocks :: [Text] <- undefined
  forM_ locatedCodeBlocks print


prints :: Text
prints =
  [__i|println!("Hello world");
       eprintln!("Hello error");
       format!("Hello {}", "world")
      |]
