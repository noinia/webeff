module WebEff.DOM.Types
  ( Attribute(..)
  , AttributeValue(..)
  , (=:)

  , AttrValueConstraints
  , NodeRef
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
import           WebEff.DOM.FFI.Types (ElementName(..), AttributeName(..))
import qualified WebEff.DOM.FFI.Types as FFI

--------------------------------------------------------------------------------

type NodeRef = FFI.Node

type AttrValueConstraints a = (HasSetAttributeValue a, Typeable a, Eq a)

data AttributeValue msg where
    Message   :: msg -> AttributeValue msg
    AttrValue :: AttrValueConstraints value => value -> AttributeValue msg

instance Eq msg => Eq (AttributeValue msg) where
  (Message msg)          == (Message msg')          = msg == msg'
  (AttrValue (val :: a)) == (AttrValue (val' :: b)) = case eqT @a @b of
                                                            Just Refl -> val == val'
                                                            _         -> False
  _                      == _                       = False


instance Functor AttributeValue where
  fmap f = \case
    Message msg -> Message (f msg)
    AttrValue x -> AttrValue x

instance Foldable AttributeValue where
  foldMap f = \case
    Message msg -> f msg
    AttrValue _ -> mempty

instance Traversable AttributeValue where
  traverse f = \case
    Message msg -> Message <$> f msg
    AttrValue x -> pure $ AttrValue x


type Attribute action = (AttributeName, AttributeValue action)


(=:)            :: AttrValueConstraints value
                => AttributeName -> value -> Attribute action
attrName =: val = (attrName, AttrValue val)
