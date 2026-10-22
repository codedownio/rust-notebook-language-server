{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -F -pgmF sandwich-discover #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- The generated tree passes module file names as string literals
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Test (tests, main) where

import Test.Sandwich

#insert_test_imports


tests :: TopSpec
tests = $(getSpecFromFolder defaultGetSpecFromFolderOptions)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
