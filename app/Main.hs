module Main where

import WebEff qualified

foreign export javascript "hs_start"
  main :: IO ()

main :: IO ()
main = WebEff.main
