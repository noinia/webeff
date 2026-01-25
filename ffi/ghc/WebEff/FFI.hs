module WebEff.FFI
  ( js_document
  , js_body
  , js_head
  , js_window
  , js_getParent
  , js_log
  , js_createTextNode
  , js_createElement
  , js_set_text_content
  , js_appendChild
  , js_insertBefore
  , js_removeChild
  , js_removeSelf
  , js_removeAttribute
  , js_setAttributeString
  , js_setAttributeDouble
  , js_setAttributeBool
  , js_setAttributeInt

  , js_addEventListener
  , js_remove_event_listener

  , js_getProperty_JSVal
  , js_getProperty_Double
  , js_getProperty_Float
  , js_getProperty_String
  , js_getProperty_Int

  , js_setProperty_String
  ) where

import           Data.Coerce
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           WebEff.FFI.Prim
import           WebEff.FFI.Types

--------------------------------------------------------------------------------

-- foreign import javascript unsafe "return document"
js_document :: IO Document
js_document = undefined

-- foreign import javascript unsafe "return document.body"
js_body :: IO Body
js_body = undefined

-- foreign import javascript unsafe "return document.head"
js_head :: IO Head
js_head = undefined

-- foreign import javascript unsafe "return window"
js_window :: IO Window
js_window = undefined

-- foreign import javascript unsafe "return $1.parentNode"
js_getParent :: Node -> IO Node
js_getParent = undefined

--------------------------------------------------------------------------------

-- foreign import javascript unsafe "console.log($1)"
js_log :: JSString -> IO ()
js_log = undefined

--------------------------------------------------------------------------------

-- foreign import javascript unsafe "document.createTextNode($1)"
js_createTextNode :: JSString -> IO Node
js_createTextNode = undefined

-- foreign import javascript unsafe "document.createElement($1)"
js_createElement :: JSString -> IO Node
js_createElement = undefined

--------------------------------------------------------------------------------

-- foreign import javascript unsafe "$1.textContent = $2"
js_set_text_content :: Node -> JSString -> IO ()
js_set_text_content = undefined

--------------------------------------------------------------------------------
-- * Adding or Removing Elements

-- foreign import javascript unsafe "$1.appendChild($2)"
js_appendChild :: Node -> Node -> IO ()
js_appendChild = undefined

-- foreign import javascript unsafe "$1.insertBefore($2)"
js_insertBefore :: Node -> Node -> IO ()
js_insertBefore = undefined

-- foreign import javascript unsafe "$1.removeChild($2)"
js_removeChild :: Node -> Node -> IO ()
js_removeChild = undefined
-- foreign import javascript unsafe "$1.remove()"
js_removeSelf :: Node -> IO ()
js_removeSelf = undefined

--------------------------------------------------------------------------------
-- * Setting Attributes

-- foreign import javascript unsafe "$1.setAttribute($2,$3)"
js_setAttributeString :: Node -> JSString -> JSString -> IO ()
js_setAttributeString = undefined

-- foreign import javascript unsafe "$1.setAttribute($2,$3)"
js_setAttributeBool :: Node -> JSString -> Bool -> IO ()
js_setAttributeBool = undefined

-- foreign import javascript unsafe "$1.setAttribute($2,$3)"
js_setAttributeInt :: Node -> JSString -> Int -> IO ()
js_setAttributeInt = undefined

-- foreign import javascript unsafe "$1.setAttribute($2,$3)"
js_setAttributeDouble :: Node -> JSString -> Double -> IO ()
js_setAttributeDouble = undefined

-- foreign import javascript unsafe "$1.removeAttribute($2)"
js_removeAttribute :: Node
                     -> JSString
                     -- ^ The Attribute Name
                     -> IO ()
js_removeAttribute = undefined

--------------------------------------------------------------------------------

-- note; we use an asynchronous event handler. I think this may prevent us from handling
-- some type of oevents. But otherwise this seems to yield weird issues

-- foreign import javascript "wrapper"
js_mkEventHandler :: (JSVal -> IO ()) -> IO JsEventListener
js_mkEventHandler = undefined

-- foreign import javascript unsafe "$1.addEventListener($2,$3)"
js_raw_addEventListener :: EventTarget
                          -> JSString
                          -> JsEventListener
                          -> IO ()
js_raw_addEventListener = undefined

-- | Create and add a JSEventListener
js_addEventListener                      :: EventTarget
                                         -> JSString
                                         -- ^ The event name
                                         -> (JSVal -> IO ())
                                         -- ^ The handler
                                         -> IO JsEventListener
                                         -- ^ reference to the handler
js_addEventListener target event handler = do jsHandler <- js_mkEventHandler handler
                                              js_raw_addEventListener target event jsHandler
                                              pure jsHandler


-- foreign import javascript unsafe "$1.removeEventListener($2,$3)"
js_remove_event_listener :: EventTarget -> JSString -> JsEventListener -> IO ()
js_remove_event_listener = undefined


-- -- foreign import javascript unsafe "$1 => {}
-- $1.removeEventListener($2,$3)"
-- js_remove_event_listeners :: EventTarget -> JSString -> IO ()



--------------------------------------------------------------------------------
-- * Getting and Setting Properties

-- foreign import javascript unsafe "return $1[$2]"
js_getProperty_JSVal :: JSVal -> JSString -> IO JSVal
js_getProperty_JSVal = undefined

-- foreign import javascript unsafe "return $1[$2]"
js_getProperty_Int :: JSVal -> JSString -> IO Int
js_getProperty_Int = undefined

-- foreign import javascript unsafe "return $1[$2]"
js_getProperty_Double :: JSVal -> JSString -> IO Double
js_getProperty_Double = undefined

-- foreign import javascript unsafe "return $1[$2]"
js_getProperty_Float :: JSVal -> JSString -> IO Float
js_getProperty_Float = undefined

-- foreign import javascript unsafe "return $1[$2]"
js_getProperty_String :: JSVal -> JSString -> IO JSString
js_getProperty_String = undefined

-- foreign import javascript unsafe "$1[$2] = $3"
js_setProperty_String :: JSVal -> JSString -> JSString -> IO ()
js_setProperty_String = undefined
-- second arg is the name of the property
