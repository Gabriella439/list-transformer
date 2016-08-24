module Main where

import Test.DocTest

main :: IO ()
main = doctest
  [ "-XCPP"
  , "src/List/Transformer.hs"
  ]
