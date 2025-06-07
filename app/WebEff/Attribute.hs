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
import           WebEff.DOM.FFI.Classes (HasSetAttributeValue(..))
import           WebEff.DOM.FFI.Types (NodeRef, ElementName(..), AttributeName, EventName(..))
import qualified WebEff.DOM.FFI.Types as FFI

--------------------------------------------------------------------------------

-- | Internal helper to create event attributes
mkEvent   :: Text -> EventHandler es msg -> Attribute es msg
mkEvent e = EventAttr (EventName e)

-- | Internal helper to create event attributes
mkEvent'       :: Text -> msg -> Attribute es msg
mkEvent' e msg = mkEvent e (EventHandler $ const (pure msg))


-- | An on-click event
onClick :: msg -> Attribute es msg
onClick = mkEvent' "click"

newtype PointerEvent = PointerEvent { client :: (Int,Int)
                                 --    , screen :: (Int,Int)
                                 -- , page   ::
                                    }
                     deriving (Show,Eq)

-- | onPointerOver event
onPointerOver   :: DOM :> es => (PointerEvent -> msg) -> Attribute es msg
onPointerOver f = mkEvent "pointerover" (EventHandler $ fmap f <$> parsePointerEvent)

parsePointerEvent       :: DOM :> es => FFI.Event -> Eff es PointerEvent
parsePointerEvent event = PointerEvent <$> parseClient event

parseClient       :: DOM :> es => FFI.Event -> Eff es (Int,Int)
parseClient event = (,) <$> getProperty "clientX" event <*> getProperty "clientY" event


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
