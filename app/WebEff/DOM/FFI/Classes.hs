module WebEff.DOM.FFI.Classes where

import           Data.Coerce
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Wasm.Prim
import           WebEff.DOM.FFI.Raw
import           WebEff.DOM.FFI.Types

--------------------------------------------------------------------------------


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
instance HasSetAttributeValue Bool where
  js_setAttribute = js_setAttributeBool
instance HasSetAttributeValue Int where
  js_setAttribute = js_setAttributeInt
instance HasSetAttributeValue Double where
  js_setAttribute = js_setAttributeDouble

--------------------------------------------------------------------------------

class HasGetPropertyValue t where
  -- | We can get properties of this type
  js_getProperty :: JSVal
                 -> JSString -- ^ The property name
                 -> IO t

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
