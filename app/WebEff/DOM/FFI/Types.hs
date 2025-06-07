{-# LANGUAGE OverloadedStrings #-}
module WebEff.DOM.FFI.Types where

import           Data.Coerce
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Wasm.Prim

--------------------------------------------------------------------------------

textToJSString :: Text -> JSString
textToJSString = toJSString . Text.unpack

--------------------------------------------------------------------------------
-- * Events

-- | Reference to an event
newtype Event = Event JSVal
  -- deriving stock (Show,Eq,Ord)

newtype JsEventListener a = JsEventListener (Event -> IO a)

-- | The name of an event 'eg 'click'
newtype EventName = EventName Text
  deriving stock (Show,Eq,Ord)
--  deriving newtype (IsString)

-- Reference to something on which we can add an event listener
newtype EventTarget = EventTarget JSVal
--   deriving stock (Show,Eq,Ord)
--   deriving newtype (IsString)

-- | Things that can act as an Event target
class IsEventTarget target where
  asEventTarget :: target -> EventTarget


instance IsEventTarget EventTarget where
  asEventTarget = id

instance IsEventTarget Window where
  asEventTarget = coerce

instance IsEventTarget Document where
  asEventTarget = coerce

instance IsEventTarget Node where
  asEventTarget = coerce


--------------------------------------------------------------------------------
-- * Elements

newtype ElementName = ElementName Text
  deriving stock (Show,Eq,Ord)
  -- deriving newtype (IsString)

-- | A (reference to a) Node
newtype Node = Node JSVal

-- | Alias for Node
type NodeRef = Node

-- | Types that can act as a node
class IsNode node where
  asNode :: node -> Node

instance IsNode Node where
  asNode = id

instance IsNode Body where
  asNode = coerce

--------------------------------------------------------------------------------
-- * Document

-- | A reference to the document
newtype Document = Document JSVal

-- | A reference to the Window
newtype Window = Window JSVal

-- | A reference to the body of the html page
newtype Body = Body JSVal

--------------------------------------------------------------------------------
-- * Attributes

newtype AttributeName = AttributeName Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString)


-- data AttributeValue = AttributeString Text
--                     | AttributeNum    !Int
--                     | AttributeBool   !Bool
--                     deriving (Show)

--------------------------------------------------------------------------------
-- * Properties

newtype PropertyName = PropertyName Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString)
