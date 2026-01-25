module WebEff.FFI.Prim
  ( JSVal
  , JSString
  , toJSString
  , fromJSString
  ) where


--------------------------------------------------------------------------------

data JSVal = JSVal

data JSString = JSString

toJSString :: String -> JSString
toJSString = undefined

fromJSString :: JSString -> String
fromJSString = undefined
