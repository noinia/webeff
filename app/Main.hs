{-# LANGUAGE OverloadedStrings  #-}
module Main where

import WebEff.Runtime
import WebEff.FFI
import WebEff.FFI.Types
import Data.Coerce
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Dynamic qualified as Dynamic
import Effectful
import Effectful.State.Dynamic
import Control.Lens
import WebEff.Runtime

--------------------------------------------------------------------------------

foreign export javascript "hs_start"
  main :: IO ()

--------------------------------------------------------------------------------










  -- do modify $ \runTime ->
  --                            runTime {

  --                                    }


--------------------------------------------------------------------------------





main :: IO ()
main = do
  body <- js_body
  minButton  <- js_createElement (textToJSString "button")
  minText    <- js_createTextNode (textToJSString "-")

  textValue  <- js_createTextNode (textToJSString "initial text")

  plusButton <- js_createElement (textToJSString "button")
  plusText  <- js_createTextNode (textToJSString "+")

  js_appendChild (coerce body) minButton
  js_appendChild minButton minText

  js_appendChild (coerce body) textValue

  js_appendChild (coerce body) plusButton
  js_appendChild plusButton plusText

  js_addEventListener (coerce minButton) (textToJSString "click") $ \evt ->
    js_log (textToJSString "- clicked")

  js_addEventListener (coerce plusButton) (textToJSString "click") $ \evt ->
    js_log (textToJSString "+ clicked")

  putStrLn "woei"
