module WebEff.DOM.FFI.Raw where

import           Data.Coerce
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Wasm.Prim
import           WebEff.DOM.FFI.Types

--------------------------------------------------------------------------------

foreign import javascript unsafe "return document"
  js_document :: IO Document

foreign import javascript unsafe "return document.body"
  js_body :: IO Body

foreign import javascript unsafe "return window"
  js_window :: IO Window

foreign import javascript unsafe "return $1.parentNode"
  js_getParent :: Node -> IO Node

--------------------------------------------------------------------------------

foreign import javascript unsafe "console.log($1)"
  js_log :: JSString -> IO ()

--------------------------------------------------------------------------------

foreign import javascript unsafe "document.createTextNode($1)"
  js_createTextNode :: JSString -> IO Node

foreign import javascript unsafe "document.createElement($1)"
  js_createElement :: JSString -> IO Node

--------------------------------------------------------------------------------

foreign import javascript unsafe "$1.textContent = $2"
  js_set_text_content :: Node -> JSString -> IO ()

--------------------------------------------------------------------------------
-- * Adding or Removing Elements

foreign import javascript unsafe "$1.appendChild($2)"
  js_appendChild :: Node -> Node -> IO ()

foreign import javascript unsafe "$1.insertBefore($2)"
  js_insertBefore :: Node -> Node -> IO ()

foreign import javascript unsafe "$1.removeChild($2)"
  js_removeChild :: Node -> Node -> IO ()

foreign import javascript unsafe "$1.remove()"
  js_removeSelf :: Node -> IO ()

--------------------------------------------------------------------------------
-- * Setting Attributes

foreign import javascript unsafe "$1.setAttribute($2,$3)"
  js_setAttributeString :: Node -> JSString -> JSString -> IO ()

foreign import javascript unsafe "$1.setAttribute($2,$3)"
  js_setAttributeBool :: Node -> JSString -> Bool -> IO ()


foreign import javascript unsafe "$1.setAttribute($2,$3)"
  js_setAttributeInt :: Node -> JSString -> Int -> IO ()

foreign import javascript unsafe "$1.setAttribute($2,$3)"
  js_setAttributeDouble :: Node -> JSString -> Double -> IO ()


foreign import javascript unsafe "$1.removeAttribute($2)"
  js_removeAttribute :: Node
                     -> JSString
                     -- ^ The Attribute Name
                     -> IO ()

--------------------------------------------------------------------------------

-- note; we use an asynchronous event handler. I think this may prevent us from handling
-- some type of oevents. But otherwise this seems to yield weird issues

foreign import javascript "wrapper"
  js_mkEventHandler :: (JSVal -> IO ()) -> IO JsEventListener

foreign import javascript unsafe "$1.addEventListener($2,$3)"
  js_addEventListener :: EventTarget
                      -> JSString
                      -> JsEventListener
                      -> IO ()

foreign import javascript unsafe "$1.removeEventListener($2,$3)"
  js_remove_event_listener :: EventTarget -> JSString -> JsEventListener -> IO ()


-- foreign import javascript unsafe "$1 => {}
-- $1.removeEventListener($2,$3)"
--   js_remove_event_listeners :: EventTarget -> JSString -> IO ()



--------------------------------------------------------------------------------
-- * Getting and Setting Properties

foreign import javascript unsafe "return $1[$2]"
  js_getProperty_JSVal :: JSVal -> JSString -> IO JSVal

foreign import javascript unsafe "return $1[$2]"
  js_getProperty_Int :: JSVal -> JSString -> IO Int

foreign import javascript unsafe "return $1[$2]"
  js_getProperty_Double :: JSVal -> JSString -> IO Double

foreign import javascript unsafe "return $1[$2]"
  js_getProperty_Float :: JSVal -> JSString -> IO Float

foreign import javascript unsafe "return $1[$2]"
  js_getProperty_String :: JSVal -> JSString -> IO JSString
