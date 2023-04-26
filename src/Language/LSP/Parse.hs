
module Language.LSP.Parse (

  ) where

import Data.Function
import Data.String.Interpolate
import Language.Rust.Parser
import Language.Rust.Syntax


parsed1 = inputStreamFromString "fn main () { println!(\"Hello world!\"); }"
        & parse' @(SourceFile Span)

parsed2 = inputStreamFromString [i|println!("Hello world");|]
        & parse' @(SourceFile Span)

parsed3 = inputStreamFromString [i|let mut message = "Hello ".to_owned();|]
        & parse' @(SourceFile Span)
