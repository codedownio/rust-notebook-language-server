(
 (haskell-mode
  .
  (
   (haskell-process-args-stack-ghci . ("--ghci-options=-ferror-spans"
                                       "--no-build"
                                       "--no-load"
                                       "haskell-notebook-language-server:lib"
                                       "haskell-notebook-language-server:exe:haskell-notebook-language-server"
                                       "haskell-notebook-language-server:test:haskell-notebook-language-server-test"
                                       )))
  )
 )
