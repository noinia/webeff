{-# LANGUAGE OverloadedStrings  #-}
module WebEff.Attribute
  ( module WebEff.DOM.Attribute
  , onClick
  , onPointerOver

  , classes


  , HasTextRender(..)
  , HtmlId(..)
  , CssClass(..)
  , CssStyle(..)

  , PointerEvent(..)
  ) where

import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Coerce
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable
import           Effectful
import           WebEff.DOM.Attribute
import           WebEff.DOM.FFI as FFI
import           WebEff.DOM.FFI.Raw (HasSetAttributeValue(..))
import           WebEff.DOM.FFI.Types (NodeRef, ElementName(..), AttributeName, EventName(..))
import qualified WebEff.DOM.FFI.Types as FFI

--------------------------------------------------------------------------------

-- | Internal helper to create event attributes
mkEvent   :: Text -> msg -> Attribute msg
mkEvent e = EventAttr (EventName e)

-- | An on-click event
onClick :: msg -> Attribute msg
onClick = mkEvent "click"

data PointerEvent = PointerEvent

-- | onPointerOver event
onPointerOver :: msg -> Attribute msg
onPointerOver = mkEvent "pointerover"
-- onPointerOver   :: (PointerEvent -> msg) -> Attribute msg
-- onPointerOver f = mkEvent "onpointerover" . f



-- | Renders classes
classes :: Foldable f => f CssClass -> CssClass
classes = CssClass . Text.unwords . map coerce . F.toList










--------------------------------------------------------------------------------


class HasTextRender a where
  renderAsText :: a -> Text

-- instance HasTextRender a => HasTextRender (Identity a) where
--   renderAsText (Identity x) = renderAsText x

instance HasTextRender Text where
  renderAsText = id


newtype HtmlId = HtmlId Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString, HasTextRender,HasSetAttributeValue)

--------------------------------------------------------------------------------

newtype CssClass = CssClass Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString, HasTextRender,HasSetAttributeValue)


-- data Style = Style

newtype CssStyle = CssStyle Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString, HasTextRender,HasSetAttributeValue)
