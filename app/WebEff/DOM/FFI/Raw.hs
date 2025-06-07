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

foreign import javascript unsafe "str => {$1.textContent = str}"
  js_set_text_content :: Node -> JSString -> IO ()

--------------------------------------------------------------------------------
-- * Adding or Removing Elements

foreign import javascript unsafe "$1.appendChild($2)"
  js_appendChild :: Node -> Node -> IO ()

foreign import javascript unsafe "$1.insertBefore($2)"
  js_insertBefore :: Node -> Node -> IO ()

foreign import javascript unsafe "$1.removeChild($2)"
  js_removeChild :: Node -> Node -> IO ()

--------------------------------------------------------------------------------
-- * Setting Attributes

class HasSetAttributeValue t where
  -- | We can set attributes of this type
  js_setAttribute :: Node
                  -> JSString -- ^ The attribute name
                  -> t
                  -> IO ()

instance HasSetAttributeValue Text where
  js_setAttribute node attr = js_setAttribute node attr . textToJSString

instance HasSetAttributeValue JSString where
  js_setAttribute = js_setAttributeString

foreign import javascript unsafe "$1.setAttribute($2,$3)"
  js_setAttributeString :: Node -> JSString -> JSString -> IO ()

instance HasSetAttributeValue Bool where
  js_setAttribute = js_setAttributeBool

foreign import javascript unsafe "$1.setAttribute($2,$3)"
  js_setAttributeBool :: Node -> JSString -> Bool -> IO ()

instance HasSetAttributeValue Int where
  js_setAttribute = js_setAttributeInt

foreign import javascript unsafe "$1.setAttribute($2,$3)"
  js_setAttributeInt :: Node -> JSString -> Int -> IO ()

instance HasSetAttributeValue Double where
  js_setAttribute = js_setAttributeDouble

foreign import javascript unsafe "$1.setAttribute($2,$3)"
  js_setAttributeDouble :: Node -> JSString -> Double -> IO ()


foreign import javascript unsafe "$1.removeAttribute($2)"
  js_removeAttribute :: Node
                     -> JSString
                     -- ^ The Attribute Name
                     -> IO ()

--------------------------------------------------------------------------------

foreign import javascript "wrapper sync"
  js_mkEventHandler :: (JSVal -> IO ()) -> IO JSVal

foreign import javascript unsafe "$1.addEventListener($2,$3)"
  js_addEventListener :: EventTarget
                      -> JSString
                      -> JSVal
                      -> IO ()

foreign import javascript unsafe "$1.removeEventListener($2,$3)"
  js_remove_event_listener :: EventTarget -> JSString -> JSVal -> IO ()


--------------------------------------------------------------------------------

class HasGetPropertyValue t where
  -- | We can get properties of this type
  js_getProperty :: JSVal
                 -> JSString -- ^ The property name
                 -> IO t

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

instance HasGetPropertyValue JSVal where
  js_getProperty = js_getProperty_JSVal
instance HasGetPropertyValue Int where
  js_getProperty = js_getProperty_Int
instance HasGetPropertyValue Double where
  js_getProperty = js_getProperty_Double
instance HasGetPropertyValue Float where
  js_getProperty = js_getProperty_Float
instance HasGetPropertyValue String where
  js_getProperty n prop = fromJSString <$> js_getProperty_String n prop
instance HasGetPropertyValue Text where
  js_getProperty n prop = Text.pack <$> js_getProperty n prop
