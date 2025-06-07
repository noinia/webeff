module WebEff.DOM.Attribute
  ( Attribute(..)
  , AttributeValue(..)
  , (=:)
  , AttributeName
  , EventName

  , AttrValueConstraints
  ) where

import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable
import           Effectful
import           WebEff.DOM.FFI as FFI
import           WebEff.DOM.FFI.Types (NodeRef, ElementName(..), AttributeName, EventName(..))
import qualified WebEff.DOM.FFI.Types as FFI

--------------------------------------------------------------------------------

-- | Constraints that we need ton the attribute values to marshall them.
type AttrValueConstraints a = (HasSetAttributeValue a, Typeable a, Eq a)

-- | An attribute value is either an actual value (of a type that can be marshalled and
-- set in JS).
data AttributeValue where
  AttrValue :: AttrValueConstraints value => value -> AttributeValue

instance Eq AttributeValue where
  (AttrValue (val :: a)) == (AttrValue (val' :: b)) = case eqT @a @b of
                                                        Just Refl -> val == val'
                                                        _         -> False

-- instance Functor AttributeValue where
--   fmap f = \case
--     Message msg -> Message (f msg)
--     AttrValue x -> AttrValue x

-- instance Foldable AttributeValue where
--   foldMap f = \case
--     Message msg -> f msg
--     AttrValue _ -> mempty

-- instance Traversable AttributeValue where
--   traverse f = \case
--     Message msg -> Message <$> f msg
--     AttrValue x -> pure $ AttrValue x


--------------------------------------------------------------------------------

-- | An attribute
data Attribute msg = EventAttr EventName msg
                   | Attr      AttributeName AttributeValue
                   deriving (Eq,Functor,Foldable,Traversable)

-- | Shorthand for assigning attribute values
(=:)            :: AttrValueConstraints value
                => AttributeName -> value -> Attribute msg
attrName =: val = Attr attrName (AttrValue val)

-- (-:) :: EventName -> msg -> Attribute msg
-- evtName -: msg = EventAttr evtName msg
