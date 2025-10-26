{-# LANGUAGE OverloadedStrings  #-}
module Main where

--------------------------------------------------------------------------------

foreign export javascript "hs_start"
  main :: IO ()

--------------------------------------------------------------------------------

main :: IO ()
main = putStrLn "woei"
